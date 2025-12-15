// Simple emitter using iced-x86 to generate machine code at runtime
use crate::codegen::{self, BinaryOp, CondCode, Instruction, Operand, Reg, UnaryOp};
use iced_x86::{BlockEncoderOptions, IcedError, SymbolResolver, SymbolResult, code_asm::*};
use object::write::{
    Object, Relocation, RelocationFlags, StandardSection, StandardSegment, Symbol, SymbolFlags, SymbolKind, SymbolScope, SymbolSection, SymbolId,
};
use object::{Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationKind, SectionKind};
use std::collections::HashMap;

// Symbol resolver for displaying labels in assembly output
pub struct MySymbolResolver {
    symbols: HashMap<u64, String>,
}

impl MySymbolResolver {
    fn new(symbols: HashMap<u64, String>) -> Self {
        Self { symbols }
    }
}

impl SymbolResolver for MySymbolResolver {
    fn symbol(
        &'_ mut self,
        _instruction: &iced_x86::Instruction,
        _operand: u32,
        _instruction_operand: Option<u32>,
        address: u64,
        _address_size: u32,
    ) -> Option<SymbolResult<'_>> {
        self.symbols
            .get(&address)
            .map(|name| SymbolResult::with_str(address, name.as_str()))
    }
}

#[allow(clippy::type_complexity)]
pub fn get_instructions(
    program: &codegen::Program,
) -> Result<(Vec<iced_x86::Instruction>, MySymbolResolver, HashMap<usize, String>), IcedError> {
    let mut a = CodeAssembler::new(64)?;
    let mut labels: HashMap<String, CodeLabel> = HashMap::new();
    let mut label_idx = HashMap::new();

    // Emit the function
    a.push(gpr64::rbp)?;
    a.mov(gpr64::rbp, gpr64::rsp)?;
    let mut external_calls = Vec::new();
    for ins in &program.functions[0].body { //todo
        emit_instruction(&mut a, ins, &mut labels, &mut label_idx, &HashMap::new(), &mut external_calls)?;
    }
    // Assemble to get real addresses
    let base_address = 0x1000u64;
    let result = a.assemble_options(base_address, BlockEncoderOptions::RETURN_NEW_INSTRUCTION_OFFSETS)?;

    // Build address-to-label mapping for the SymbolResolver
    let mut address_map = HashMap::new();
    for (name, code_label) in labels {
        if let Ok(address) = result.label_ip(&code_label) {
            address_map.insert(address, name);
        }
    }
    let resolver = MySymbolResolver::new(address_map);

    // Decode the assembled instructions to get ones with proper addresses
    let code_bytes = &result.inner.code_buffer;
    let mut decoder = iced_x86::Decoder::new(64, code_bytes, iced_x86::DecoderOptions::NONE);
    decoder.set_ip(base_address);
    let instructions: Vec<iced_x86::Instruction> = decoder.into_iter().collect();

    Ok((instructions, resolver, label_idx))
}

pub fn emit_object(program: &codegen::Program) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    emit_object_internal(program, true)
}

pub fn emit_object_for_linking(program: &codegen::Program) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    emit_object_internal(program, false)
}

fn emit_object_internal(program: &codegen::Program, include_start: bool) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut a = CodeAssembler::new(64)?;

    // Create labels for all functions upfront
    let mut fn_labels: HashMap<String, CodeLabel> = HashMap::new();
    for fun_def in &program.functions {
        fn_labels.insert(fun_def.name.clone(), a.create_label());
    }

    // Track external function calls (instruction index, function name)
    let mut external_calls: Vec<(usize, String)> = Vec::new();

    // _start entry point (calls main, then exits) - only for standalone executables
    let mut start_lbl = a.create_label();
    if include_start {
        a.set_label(&mut start_lbl)?;
        a.call(*fn_labels.get("main").expect("no main function"))?;
        a.mov(gpr64::rdi, gpr64::rax)?;
        if cfg!(target_os = "macos") {
            a.mov(gpr64::rax, 0x2000001i64)?;
        } else {
            a.mov(gpr64::rax, 60i64)?;
        }
        a.syscall()?;
    }

    // Emit all functions
    for fun_def in &program.functions {
        let lbl = fn_labels.get_mut(&fun_def.name).unwrap();
        a.set_label(lbl)?;
        emit_function(&mut a, fun_def, &fn_labels, &mut external_calls)?;
    }

    let result = a.assemble_options(0, BlockEncoderOptions::RETURN_NEW_INSTRUCTION_OFFSETS)?;

    let main_off = result.label_ip(fn_labels.get("main").unwrap())?;

    let (start_off, start_size) = if include_start {
        let start_off = result.label_ip(&start_lbl)?;
        let start_size = main_off - start_off;
        (start_off, start_size)
    } else {
        (0, 0)
    };

    // Get function offsets before moving the code buffer
    let mut func_offsets: Vec<(String, u64)> = Vec::new();
    for fun_def in &program.functions {
        let func_off = result.label_ip(fn_labels.get(&fun_def.name).unwrap())?;
        func_offsets.push((fun_def.name.clone(), func_off));
    }

    // Clone instruction offsets before moving the inner
    let instruction_offsets = result.inner.new_instruction_offsets.clone();

    // Now move the code buffer
    let code = result.inner.code_buffer;

    let mut obj = if cfg!(target_os = "linux") {
        let mut obj = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);

        let note_stack = obj.add_section(
            obj.segment_name(StandardSegment::Data).to_vec(),
            b".note.GNU-stack".to_vec(),
            SectionKind::Metadata,
        );
        obj.append_section_data(note_stack, &[], 1);
        obj
    } else {
        Object::new(BinaryFormat::MachO, Architecture::X86_64, Endianness::Little)
    };

    obj.add_file_symbol(b"ncc".to_vec());
    let text = obj.section_id(StandardSection::Text);
    obj.append_section_data(text, &code, 1);

    if include_start {
        let symbol_name = if cfg!(target_os = "macos") {
            b"start".to_vec()
        } else {
            b"_start".to_vec()
        };
        obj.add_symbol(Symbol {
            name: symbol_name,
            value: start_off,
            size: start_size,
            kind: SymbolKind::Text,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Section(text),
            flags: SymbolFlags::None,
        });
    }

    // Add all function symbols
    for (func_name, func_off) in &func_offsets {
        let func_size = code.len() as u64 - func_off;
        obj.add_symbol(Symbol {
            name: func_name.as_bytes().to_vec(),
            value: *func_off,
            size: func_size,
            kind: SymbolKind::Text,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Section(text),
            flags: SymbolFlags::None,
        });
    }

    // Add external function symbols and relocations
    let mut external_symbols: HashMap<String, SymbolId> = HashMap::new();
    for (_ins_idx, func_name) in &external_calls {
        if !external_symbols.contains_key(func_name) {
            let symbol_id = obj.add_symbol(Symbol {
                name: func_name.as_bytes().to_vec(),
                value: 0,
                size: 0,
                kind: SymbolKind::Text,
                scope: SymbolScope::Unknown,
                weak: false,
                section: SymbolSection::Undefined,
                flags: SymbolFlags::None,
            });
            external_symbols.insert(func_name.clone(), symbol_id);
        }
    }

    // Add relocations for external calls
    // The instruction offsets tell us where each instruction starts in the byte stream
    if !external_calls.is_empty() {
        for (ins_idx, func_name) in &external_calls {
            if let Some(symbol_id) = external_symbols.get(func_name) {
                // The call instruction is 5 bytes: E8 (opcode) + 4 bytes (offset)
                // We need to add a relocation at the offset location (1 byte after the instruction start)
                let ins_offset = instruction_offsets[*ins_idx];
                let reloc_offset = (ins_offset + 1) as u64;

                obj.add_relocation(text, Relocation {
                    offset: reloc_offset,
                    flags: RelocationFlags::Generic {
                        kind: RelocationKind::Relative,
                        encoding: RelocationEncoding::X86Branch,
                        size: 32,
                    },
                    symbol: *symbol_id,
                    addend: -4,
                })?;
            }
        }
    }

    Ok(obj.write()?)
}

fn emit_function(a: &mut CodeAssembler, fun_def: &codegen::FunctionDefinition, fn_labels:  &HashMap<String, CodeLabel>, external_calls: &mut Vec<(usize, String)>) -> Result<(), IcedError> {
    let mut labels: HashMap<String, CodeLabel> = HashMap::new();
    let mut label_idx = HashMap::new();
    a.push(gpr64::rbp)?;
    a.mov(gpr64::rbp, gpr64::rsp)?;
    for ins in &fun_def.body {
        emit_instruction(a, ins, &mut labels, &mut label_idx, fn_labels, external_calls)?;
    }
    Ok(())
}

fn gpr32(reg: &Reg) -> AsmRegister32 {
    use codegen::Reg::*;
    match reg {
        AX => registers::gpr32::eax,
        DX => registers::gpr32::edx,
        R10 => registers::gpr32::r10d,
        R11 => registers::gpr32::r11d,
        CX => registers::gpr32::ecx,
        DI => registers::gpr32::edi,
        SI => registers::gpr32::esi,
        R8 => registers::gpr32::r8d,
        R9 => registers::gpr32::r9d,
    }
}

fn gpr64_reg(reg: &Reg) -> AsmRegister64 {
    use codegen::Reg::*;
    match reg {
        AX => gpr64::rax,
        DX => gpr64::rdx,
        R10 => gpr64::r10,
        R11 => gpr64::r11,
        CX => gpr64::rcx,
        DI => gpr64::rdi,
        SI => gpr64::rsi,
        R8 => gpr64::r8,
        R9 => gpr64::r9,
    }
}

fn mem_rbp(offset: i64) -> AsmMemoryOperand {
    if offset >= 0 {
        dword_ptr(gpr64::rbp + offset)
    } else {
        dword_ptr(gpr64::rbp - (-offset))
    }
}

fn emit_instruction(
    a: &mut CodeAssembler,
    ins: &Instruction,
    labels: &mut HashMap<String, CodeLabel>,
    label_idx: &mut HashMap<usize, String>,
    fn_labels: &HashMap<String, CodeLabel>,
    external_calls: &mut Vec<(usize, String)>,
) -> Result<(), IcedError> {
    match ins {
        Instruction::Mov { src, dst } => match (src, dst) {
            (Operand::Imm(v), Operand::Reg(r)) => a.mov(gpr32(r), *v as i32)?,
            (Operand::Reg(s), Operand::Reg(d)) => a.mov(gpr32(d), gpr32(s))?,
            (Operand::Reg(s), Operand::Stack(off)) => a.mov(mem_rbp(*off), gpr32(s))?,
            (Operand::Stack(off), Operand::Reg(d)) => a.mov(gpr32(d), mem_rbp(*off))?,
            (Operand::Imm(v), Operand::Stack(off)) => a.mov(mem_rbp(*off), *v as i32)?,
            _ => unreachable!("unsupported mov combination"),
        },
        Instruction::Unary { op, dst } => match op {
            UnaryOp::Neg => match dst {
                Operand::Reg(r) => a.neg(gpr32(r))?,
                Operand::Stack(off) => a.neg(mem_rbp(*off))?,
                _ => unreachable!(),
            },
            UnaryOp::Not => match dst {
                Operand::Reg(r) => a.not(gpr32(r))?,
                Operand::Stack(off) => a.not(mem_rbp(*off))?,
                _ => unreachable!(),
            },
        },
        Instruction::Binary { op, src, dst } => match op {
            BinaryOp::Add => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.add(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.add(gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.add(mem_rbp(*off), gpr32(s))?,
                (Operand::Imm(v), Operand::Stack(off)) => a.add(mem_rbp(*off), *v as i32)?,
                _ => unimplemented!(),
            },
            BinaryOp::Sub => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.sub(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.sub(gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.sub(mem_rbp(*off), gpr32(s))?,
                (Operand::Imm(v), Operand::Stack(off)) => a.sub(mem_rbp(*off), *v as i32)?,
                _ => unimplemented!(),
            },
            BinaryOp::Mult => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.imul_2(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.imul_3(gpr32(d), gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.imul_2(gpr32(s), mem_rbp(*off))?,
                (Operand::Stack(off), Operand::Reg(d)) => a.imul_2(gpr32(d), mem_rbp(*off))?,
                _ => unimplemented!("Mult {:?}, {:?}", src, dst),
            },
            BinaryOp::BitAnd => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.and(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.and(gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.and(mem_rbp(*off), gpr32(s))?,
                (Operand::Imm(v), Operand::Stack(off)) => a.and(mem_rbp(*off), *v as i32)?,
                _ => unimplemented!(),
            },
            BinaryOp::BitOr => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.or(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.or(gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.or(mem_rbp(*off), gpr32(s))?,
                (Operand::Imm(v), Operand::Stack(off)) => a.or(mem_rbp(*off), *v as i32)?,
                _ => unimplemented!(),
            },
            BinaryOp::BitXOr => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.xor(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.xor(gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.xor(mem_rbp(*off), gpr32(s))?,
                (Operand::Imm(v), Operand::Stack(off)) => a.xor(mem_rbp(*off), *v as i32)?,
                _ => unimplemented!(),
            },
            BinaryOp::BitShl => match (src, dst) {
                (Operand::Imm(v), Operand::Reg(d)) => a.shl(gpr32(d), *v as i32)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d)) => a.shl(gpr32(d), gpr8::cl)?,
                (Operand::Imm(v), Operand::Stack(off)) => a.shl(mem_rbp(*off), *v as i32)?,
                (Operand::Reg(Reg::CX), Operand::Stack(off)) => a.shl(mem_rbp(*off), gpr8::cl)?,
                _ => unimplemented!(),
            },
            BinaryOp::BitSar => match (src, dst) {
                (Operand::Imm(v), Operand::Reg(d)) => a.sar(gpr32(d), *v as i32)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d)) => a.sar(gpr32(d), cl)?,
                (Operand::Imm(v), Operand::Stack(off)) => a.sar(mem_rbp(*off), *v as i32)?,
                (Operand::Reg(Reg::CX), Operand::Stack(off)) => a.sar(mem_rbp(*off), gpr8::cl)?,
                _ => unimplemented!(),
            },
        },
        Instruction::Cmp { v1, v2 } => match (v1, v2) {
            (Operand::Reg(r1), Operand::Reg(r2)) => a.cmp(gpr32(r2), gpr32(r1))?,
            (Operand::Reg(r1), Operand::Imm(v)) => a.cmp(gpr32(r1), *v as i32)?,
            (Operand::Reg(r1), Operand::Stack(off)) => a.cmp(mem_rbp(*off), gpr32(r1))?,
            (Operand::Stack(off), Operand::Reg(r)) => a.cmp(gpr32(r), mem_rbp(*off))?,
            (Operand::Stack(o1), Operand::Imm(v)) => a.cmp(mem_rbp(*o1), *v as i32)?,
            (Operand::Imm(v), Operand::Reg(r)) => a.cmp(gpr32(r), *v as i32)?,
            (Operand::Imm(v), Operand::Stack(off)) => a.cmp(mem_rbp(*off), *v as i32)?,
            _ => unimplemented!(),
        },
        Instruction::Cdq => {
            a.cdq()?;
        }
        Instruction::Idiv(op) => match op {
            Operand::Reg(r) => a.idiv(gpr32(r))?,
            Operand::Stack(off) => a.idiv(mem_rbp(*off))?,
            _ => unimplemented!(),
        },
        Instruction::Jmp(label) => {
            let l = *labels.entry(label.0.clone()).or_insert_with(|| a.create_label());
            a.jmp(l)?;
        }
        Instruction::JmpCC { code, label } => {
            let l = *labels.entry(label.0.clone()).or_insert_with(|| a.create_label());
            match code {
                CondCode::E => a.je(l)?,
                CondCode::NE => a.jne(l)?,
                CondCode::G => a.jg(l)?,
                CondCode::GE => a.jge(l)?,
                CondCode::L => a.jl(l)?,
                CondCode::LE => a.jle(l)?,
            };
        }
        Instruction::SetCC { code, op } => match op {
            Operand::Reg(_r) => match code {
                CondCode::E => a.sete(gpr8::al)?,
                CondCode::NE => a.setne(gpr8::al)?,
                CondCode::G => a.setg(gpr8::al)?,
                CondCode::GE => a.setge(gpr8::al)?,
                CondCode::L => a.setl(gpr8::al)?,
                CondCode::LE => a.setle(gpr8::al)?,
            },
            Operand::Stack(off) => match code {
                CondCode::E => a.sete(mem_rbp(*off))?,
                CondCode::NE => a.setne(mem_rbp(*off))?,
                CondCode::G => a.setg(mem_rbp(*off))?,
                CondCode::GE => a.setge(mem_rbp(*off))?,
                CondCode::L => a.setl(mem_rbp(*off))?,
                CondCode::LE => a.setle(mem_rbp(*off))?,
            },
            _ => unimplemented!(),
        },
        Instruction::Label(lbl) => {
            let l = labels.entry(lbl.0.clone()).or_insert_with(|| a.create_label());
            a.set_label(l)?;
            label_idx.insert(a.instructions().len(), lbl.0.clone());
        }
        Instruction::AllocateStack(off) => {
            if *off != 0 {
                a.sub(gpr64::rsp, *off as i32)?;
            }
        }
        Instruction::Ret => {
            a.mov(gpr64::rsp, gpr64::rbp)?;
            a.pop(gpr64::rbp)?;
            a.ret()?;
        },
        Instruction::DeallocateStack(off)  => {
            a.add(gpr64::rsp, *off as i32)?;
        }
        Instruction::Push(op) => match op{
            Operand::Imm(c) => a.push(*c as i32)?,
            Operand::Reg(reg) => a.push(gpr64_reg(reg))?,
            Operand::Stack(off) => a.push(qword_ptr(gpr64::rbp - (-*off)))?,
            Operand::Pseudo(_) => unreachable!(),
        }
        Instruction::Call(name) => {
            if let Some(&lbl) = fn_labels.get(&name.0) {
                // Internal function call
                a.call(lbl)?;
            } else {
                // External function call - emit call to 0 and track for relocation
                let ins_count = a.instructions().len();
                external_calls.push((ins_count, name.0.clone()));
                a.call(0)?;
            }
        }
    }
    Ok(())
}
