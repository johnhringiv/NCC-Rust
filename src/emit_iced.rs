// Simple emitter using iced-x86 to generate machine code at runtime
use crate::codegen::{self, BinaryOp, CondCode, Instruction, Operand, Reg, UnaryOp};
use crate::tacky::StaticVariable;
use iced_x86::{BlockEncoderOptions, IcedError, SymbolResolver, SymbolResult, code_asm::*};
use object::write::{
    Object, Relocation, RelocationFlags, StandardSection, StandardSegment, Symbol, SymbolFlags, SymbolId, SymbolKind,
    SymbolScope, SymbolSection,
};
use object::{
    Architecture, BinaryFormat, Endianness, Object as ObjectRead, ObjectSection, ObjectSymbol, RelocationEncoding,
    RelocationKind, SectionKind,
};
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
) -> Result<(Vec<iced_x86::Instruction>, MySymbolResolver, HashMap<usize, String>), Box<dyn std::error::Error>> {
    let (obj_bytes, internal_labels) = emit_object_with_labels(program)?;

    // Parse the object file
    let obj = object::File::parse(&*obj_bytes)?;

    // Extract .text section
    let text_section = obj.section_by_name(".text").ok_or("No .text section found")?;
    let code = text_section.data()?;

    // Build symbol resolver from object file symbols
    let mut address_map = HashMap::new();
    let mut function_offsets = HashMap::new();
    for symbol in obj.symbols() {
        if let Ok(name) = symbol.name()
            && !name.is_empty()
            && symbol.kind() == object::SymbolKind::Text
        {
            address_map.insert(symbol.address(), name.to_string());
            function_offsets.insert(name.to_string(), symbol.address());
        }
    }

    // Build combined label index (functions + internal labels)
    // The internal_labels map instruction indices to names
    // We need to convert instruction indices to byte offsets
    let mut label_idx = HashMap::new();

    // Add function names at their byte offsets
    for (name, offset) in function_offsets {
        label_idx.insert(offset as usize, name);
    }

    // Decode instructions to get byte offsets for internal labels
    let mut decoder = iced_x86::Decoder::new(64, code, iced_x86::DecoderOptions::NONE);
    decoder.set_ip(0);
    let instructions: Vec<iced_x86::Instruction> = decoder.into_iter().collect();

    // Map instruction indices from internal_labels to byte offsets
    let mut current_offset = 0;
    for (ins_idx, ins) in instructions.iter().enumerate() {
        if let Some(label_name) = internal_labels.get(&ins_idx) {
            label_idx.insert(current_offset, label_name.clone());
            // Also add to address_map so SymbolResolver can resolve jump targets
            address_map.insert(current_offset as u64, label_name.clone());
        }
        current_offset += ins.len();
    }

    Ok((instructions, MySymbolResolver::new(address_map), label_idx))
}

pub fn emit_object(program: &codegen::Program) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    emit_object_with_labels(program).map(|(bytes, _)| bytes)
}

/// Emits an ELF/Mach-O object file from the codegen IR using iced-x86.
///
/// This is the core emission function that produces relocatable machine code.
/// It performs the following steps:
///
/// 1. Creates iced-x86 labels for all functions and static variables upfront
/// 2. Emits machine code for each function body, tracking:
///    - External function calls (for PLT relocations)
///    - Data references (for RIP-relative relocations)
///    - Internal jump labels (for disassembly)
/// 3. Assembles the code using iced-x86's block encoder
/// 4. Builds an object file with the `object` crate containing:
///    - `.text` section with the assembled code
///    - `.data`/`.bss` sections for static variables
///    - Symbol table entries for functions and variables
///    - Relocations for external calls and data references
///
/// # Returns
///
/// A tuple containing:
/// - The serialized object file bytes
/// - A map from instruction index to label name (for disassembly/debugging)
#[allow(clippy::type_complexity)]
fn emit_object_with_labels(
    program: &codegen::Program,
) -> Result<(Vec<u8>, HashMap<usize, String>), Box<dyn std::error::Error>> {
    let mut a = CodeAssembler::new(64)?;

    // Create labels for all functions upfront
    let mut fn_labels: HashMap<String, CodeLabel> = HashMap::new();
    for fun_def in &program.functions {
        fn_labels.insert(fun_def.name.clone(), a.create_label());
    }

    // Create labels for static variables to get RIP-relative addressing
    let mut data_labels: HashMap<String, CodeLabel> = HashMap::new();
    for sv in &program.static_vars {
        data_labels.insert(sv.name.clone(), a.create_label());
    }
    // Also create labels for extern variables (defined elsewhere)
    for ext_name in &program.extern_vars {
        data_labels.insert(ext_name.clone(), a.create_label());
    }

    // Track external function calls (instruction index, function name)
    let mut external_calls: Vec<(usize, String)> = Vec::new();
    let mut data_relocs: Vec<(usize, String)> = Vec::new();

    // Track all internal labels across all functions
    let mut all_label_idx: HashMap<usize, String> = HashMap::new();

    // Emit all functions
    for fun_def in &program.functions {
        let lbl = fn_labels.get_mut(&fun_def.name).unwrap();
        a.set_label(lbl)?;

        let mut labels = HashMap::new();
        let mut label_idx = HashMap::new();
        emit_function_body(
            &mut a,
            fun_def,
            &fn_labels,
            &data_labels,
            &mut external_calls,
            &mut data_relocs,
            &mut labels,
            &mut label_idx,
        )?;

        // Aggregate labels from this function
        all_label_idx.extend(label_idx);
    }

    let result = a.assemble_options(
        0,
        BlockEncoderOptions::RETURN_NEW_INSTRUCTION_OFFSETS | BlockEncoderOptions::RETURN_CONSTANT_OFFSETS,
    )?;

    // Get function offsets before moving the code buffer
    let mut func_offsets: Vec<(String, u64, bool)> = Vec::new();
    for fun_def in &program.functions {
        let func_off = result.label_ip(fn_labels.get(&fun_def.name).unwrap())?;
        func_offsets.push((fun_def.name.clone(), func_off, fun_def.global));
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

    // Add all function symbols
    for (func_name, func_off, global) in &func_offsets {
        let func_size = code.len() as u64 - func_off;
        obj.add_symbol(Symbol {
            name: func_name.as_bytes().to_vec(),
            value: *func_off,
            size: func_size,
            kind: SymbolKind::Text,
            // Use Dynamic for global symbols (DEFAULT visibility) to allow linking across object files
            // Use Compilation for static symbols (LOCAL binding)
            scope: if *global {
                SymbolScope::Dynamic
            } else {
                SymbolScope::Compilation
            },
            weak: false,
            section: SymbolSection::Section(text),
            flags: SymbolFlags::None,
        });
    }

    let data = obj.section_id(StandardSection::Data);
    let bss = obj.section_id(StandardSection::UninitializedData);
    let mut data_offset: u64 = 0;
    let mut bss_offset: u64 = 0;
    let mut static_var_symbols: HashMap<String, SymbolId> = HashMap::new();
    for StaticVariable { name, global, init } in &program.static_vars {
        let (offset, section) = if *init == 0 {
            obj.append_section_bss(bss, 4, 4);
            let offset = bss_offset;
            bss_offset += 4;
            (offset, &bss)
        } else {
            let init_bytes = init.to_le_bytes();
            obj.append_section_data(data, &init_bytes, 4);
            let offset = data_offset;
            data_offset += 4;
            (offset, &data)
        };

        let sym_id = obj.add_symbol(Symbol {
            name: name.as_bytes().to_vec(),
            value: offset,
            size: 4,
            kind: SymbolKind::Data,
            // Use Dynamic for global symbols (DEFAULT visibility) to allow linking across object files
            // Use Compilation for static symbols (LOCAL binding)
            scope: if *global {
                SymbolScope::Dynamic
            } else {
                SymbolScope::Compilation
            },
            weak: false,
            section: SymbolSection::Section(*section),
            flags: SymbolFlags::None,
        });
        static_var_symbols.insert(name.clone(), sym_id);
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

                obj.add_relocation(
                    text,
                    Relocation {
                        offset: reloc_offset,
                        flags: RelocationFlags::Generic {
                            kind: RelocationKind::Relative,
                            encoding: RelocationEncoding::X86Branch,
                            size: 32,
                        },
                        symbol: *symbol_id,
                        addend: -4,
                    },
                )?;
            }
        }
    }

    // Add undefined symbols for extern variables (defined elsewhere)
    for ext_name in &program.extern_vars {
        let sym_id = obj.add_symbol(Symbol {
            name: ext_name.as_bytes().to_vec(),
            value: 0,
            size: 0,
            kind: SymbolKind::Data,
            scope: SymbolScope::Unknown,
            weak: false,
            section: SymbolSection::Undefined,
            flags: SymbolFlags::None,
        });
        static_var_symbols.insert(ext_name.clone(), sym_id);
    }

    if !static_var_symbols.is_empty() {
        for (ins_idx, var_name) in &data_relocs {
            if let Some(symbol_id) = static_var_symbols.get(var_name) {
                let ins_offset = instruction_offsets[*ins_idx] as u64;
                let const_offsets = &result.inner.constant_offsets[*ins_idx];
                let reloc_offset = ins_offset + const_offsets.displacement_offset() as u64;

                // The addend is the negative of the bytes remaining after the displacement field.
                // For R_X86_64_PC32: patched_value = S + A - P
                // We want: displacement = S - RIP_next = S - (P + bytes_remaining)
                // So: A = -bytes_remaining = -(displacement_size + immediate_size)
                let bytes_remaining = const_offsets.displacement_size() + const_offsets.immediate_size();
                let addend = -(bytes_remaining as i64);

                obj.add_relocation(
                    text,
                    Relocation {
                        offset: reloc_offset,
                        flags: RelocationFlags::Generic {
                            kind: RelocationKind::Relative,
                            encoding: RelocationEncoding::Generic,
                            size: 32,
                        },
                        symbol: *symbol_id,
                        addend,
                    },
                )?;
            }
        }
    }

    Ok((obj.write()?, all_label_idx))
}

/// Emits machine code for a single function body.
///
/// Generates the standard x86-64 function prologue (`push rbp; mov rbp, rsp`)
/// followed by machine code for each instruction in the function body.
///
/// # Parameters
///
/// - `a`: The iced-x86 code assembler to emit instructions into
/// - `fun_def`: The function definition containing the instruction list
/// - `fn_labels`: Pre-created labels for all functions (for internal calls)
/// - `data_labels`: Pre-created labels for static/extern variables (for RIP-relative access)
/// - `external_calls`: Output vec tracking external call sites for relocation
/// - `data_relocs`: Output vec tracking data reference sites for relocation
/// - `labels`: Working map of jump label names to iced-x86 labels (within this function)
/// - `label_idx`: Output map from instruction index to label name (for disassembly)
fn emit_function_body(
    a: &mut CodeAssembler,
    fun_def: &codegen::FunctionDefinition,
    fn_labels: &HashMap<String, CodeLabel>,
    data_labels: &HashMap<String, CodeLabel>,
    external_calls: &mut Vec<(usize, String)>,
    data_relocs: &mut Vec<(usize, String)>,
    labels: &mut HashMap<String, CodeLabel>,
    label_idx: &mut HashMap<usize, String>,
) -> Result<(), IcedError> {
    a.push(gpr64::rbp)?;
    a.mov(gpr64::rbp, gpr64::rsp)?;
    for ins in &fun_def.body {
        emit_instruction(
            a,
            ins,
            labels,
            label_idx,
            fn_labels,
            data_labels,
            external_calls,
            data_relocs,
        )?;
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

fn mem_rbp(offset: i32) -> AsmMemoryOperand {
    if offset >= 0 {
        dword_ptr(gpr64::rbp + offset)
    } else {
        dword_ptr(gpr64::rbp - (-offset))
    }
}

// Data operands (static variables) - RIP-relative with relocation
// Using labels creates RIP-relative addressing; the displacement will be patched by linker
fn emit_instruction(
    a: &mut CodeAssembler,
    ins: &Instruction,
    labels: &mut HashMap<String, CodeLabel>,
    label_idx: &mut HashMap<usize, String>,
    fn_labels: &HashMap<String, CodeLabel>,
    data_labels: &HashMap<String, CodeLabel>,
    external_calls: &mut Vec<(usize, String)>,
    data_relocs: &mut Vec<(usize, String)>,
) -> Result<(), IcedError> {
    match ins {
        Instruction::Mov { src, dst } => match (src, dst) {
            (Operand::Imm(v), Operand::Reg(r)) => a.mov(gpr32(r), *v)?,
            (Operand::Reg(s), Operand::Reg(d)) => a.mov(gpr32(d), gpr32(s))?,
            (Operand::Reg(s), Operand::Stack(off)) => a.mov(mem_rbp(*off), gpr32(s))?,
            (Operand::Stack(off), Operand::Reg(d)) => a.mov(gpr32(d), mem_rbp(*off))?,
            (Operand::Data(name), Operand::Reg(d)) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.mov(gpr32(d), dword_ptr(*lbl))?
            }
            (Operand::Imm(v), Operand::Stack(off)) => a.mov(mem_rbp(*off), *v)?,
            (Operand::Imm(v), Operand::Data(name)) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.mov(dword_ptr(*lbl), *v)?
            }
            (Operand::Reg(s), Operand::Data(name)) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.mov(dword_ptr(*lbl), gpr32(s))?
            }
            _ => unreachable!("unsupported mov combination"),
        },
        Instruction::Unary { op, dst } => match op {
            UnaryOp::Neg => match dst {
                Operand::Reg(r) => a.neg(gpr32(r))?,
                Operand::Stack(off) => a.neg(mem_rbp(*off))?,
                Operand::Data(name) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.neg(dword_ptr(*lbl))?
                }
                _ => unreachable!(),
            },
            UnaryOp::Not => match dst {
                Operand::Reg(r) => a.not(gpr32(r))?,
                Operand::Stack(off) => a.not(mem_rbp(*off))?,
                Operand::Data(name) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.not(dword_ptr(*lbl))?
                }
                _ => unreachable!(),
            },
        },
        Instruction::Binary { op, src, dst } => match op {
            BinaryOp::Add => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.add(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.add(gpr32(d), *v)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.add(mem_rbp(*off), gpr32(s))?,
                (Operand::Reg(s), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.add(dword_ptr(*lbl), gpr32(s))?
                }
                (Operand::Imm(v), Operand::Stack(off)) => a.add(mem_rbp(*off), *v)?,
                (Operand::Imm(v), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.add(dword_ptr(*lbl), *v)?
                }
                _ => unreachable!(),
            },
            BinaryOp::Sub => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.sub(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.sub(gpr32(d), *v)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.sub(mem_rbp(*off), gpr32(s))?,
                (Operand::Reg(s), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.sub(dword_ptr(*lbl), gpr32(s))?
                }
                (Operand::Imm(v), Operand::Stack(off)) => a.sub(mem_rbp(*off), *v)?,
                (Operand::Imm(v), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.sub(dword_ptr(*lbl), *v)?
                }
                _ => unreachable!(),
            },
            BinaryOp::Mult => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.imul_2(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.imul_3(gpr32(d), gpr32(d), *v)?,
                (Operand::Stack(off), Operand::Reg(d)) => a.imul_2(gpr32(d), mem_rbp(*off))?,
                (Operand::Data(name), Operand::Reg(d)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.imul_2(gpr32(d), dword_ptr(*lbl))?
                }
                _ => unreachable!("Mult {:?}, {:?}", src, dst),
            },
            BinaryOp::BitAnd => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.and(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.and(gpr32(d), *v)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.and(mem_rbp(*off), gpr32(s))?,
                (Operand::Reg(s), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.and(dword_ptr(*lbl), gpr32(s))?
                }
                (Operand::Imm(v), Operand::Stack(off)) => a.and(mem_rbp(*off), *v)?,
                (Operand::Imm(v), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.and(dword_ptr(*lbl), *v)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitOr => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.or(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.or(gpr32(d), *v)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.or(mem_rbp(*off), gpr32(s))?,
                (Operand::Reg(s), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.or(dword_ptr(*lbl), gpr32(s))?
                }
                (Operand::Imm(v), Operand::Stack(off)) => a.or(mem_rbp(*off), *v)?,
                (Operand::Imm(v), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.or(dword_ptr(*lbl), *v)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitXOr => match (src, dst) {
                (Operand::Reg(s), Operand::Reg(d)) => a.xor(gpr32(d), gpr32(s))?,
                (Operand::Imm(v), Operand::Reg(d)) => a.xor(gpr32(d), *v)?,
                (Operand::Reg(s), Operand::Stack(off)) => a.xor(mem_rbp(*off), gpr32(s))?,
                (Operand::Reg(s), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.xor(dword_ptr(*lbl), gpr32(s))?
                }
                (Operand::Imm(v), Operand::Stack(off)) => a.xor(mem_rbp(*off), *v)?,
                (Operand::Imm(v), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.xor(dword_ptr(*lbl), *v)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitShl => match (src, dst) {
                (Operand::Imm(v), Operand::Reg(d)) => a.shl(gpr32(d), *v)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d)) => a.shl(gpr32(d), gpr8::cl)?,
                (Operand::Imm(v), Operand::Stack(off)) => a.shl(mem_rbp(*off), *v)?,
                (Operand::Imm(v), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.shl(dword_ptr(*lbl), *v)?
                }
                (Operand::Reg(Reg::CX), Operand::Stack(off)) => a.shl(mem_rbp(*off), gpr8::cl)?,
                (Operand::Reg(Reg::CX), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.shl(dword_ptr(*lbl), gpr8::cl)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitSar => match (src, dst) {
                (Operand::Imm(v), Operand::Reg(d)) => a.sar(gpr32(d), *v)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d)) => a.sar(gpr32(d), cl)?,
                (Operand::Imm(v), Operand::Stack(off)) => a.sar(mem_rbp(*off), *v)?,
                (Operand::Imm(v), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.sar(dword_ptr(*lbl), *v)?
                }
                (Operand::Reg(Reg::CX), Operand::Stack(off)) => a.sar(mem_rbp(*off), gpr8::cl)?,
                (Operand::Reg(Reg::CX), Operand::Data(name)) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.sar(dword_ptr(*lbl), gpr8::cl)?
                }
                _ => unreachable!(),
            },
        },
        Instruction::Cmp { v1, v2 } => match (v1, v2) {
            (Operand::Reg(r1), Operand::Reg(r2)) => a.cmp(gpr32(r2), gpr32(r1))?,
            (Operand::Reg(r1), Operand::Stack(off)) => a.cmp(mem_rbp(*off), gpr32(r1))?,
            (Operand::Reg(r1), Operand::Data(name)) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.cmp(dword_ptr(*lbl), gpr32(r1))?
            }
            (Operand::Stack(off), Operand::Reg(r)) => a.cmp(gpr32(r), mem_rbp(*off))?,
            (Operand::Data(name), Operand::Reg(r)) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.cmp(gpr32(r), dword_ptr(*lbl))?
            }
            (Operand::Imm(v), Operand::Reg(r)) => a.cmp(gpr32(r), *v)?,
            (Operand::Imm(v), Operand::Stack(off)) => a.cmp(mem_rbp(*off), *v)?,
            (Operand::Imm(v), Operand::Data(name)) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.cmp(dword_ptr(*lbl), *v)?
            }
            _ => unreachable!(),
        },
        Instruction::Cdq => {
            a.cdq()?;
        }
        Instruction::Idiv(op) => match op {
            Operand::Reg(r) => a.idiv(gpr32(r))?,
            Operand::Stack(off) => a.idiv(mem_rbp(*off))?,
            Operand::Data(name) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.idiv(dword_ptr(*lbl))?
            }
            _ => unreachable!(),
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
            Operand::Data(name) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                match code {
                    CondCode::E => a.sete(byte_ptr(*lbl))?,
                    CondCode::NE => a.setne(byte_ptr(*lbl))?,
                    CondCode::G => a.setg(byte_ptr(*lbl))?,
                    CondCode::GE => a.setge(byte_ptr(*lbl))?,
                    CondCode::L => a.setl(byte_ptr(*lbl))?,
                    CondCode::LE => a.setle(byte_ptr(*lbl))?,
                }
            }
            _ => unreachable!(),
        },
        Instruction::Label(lbl) => {
            let l = labels.entry(lbl.0.clone()).or_insert_with(|| a.create_label());
            a.set_label(l)?;
            label_idx.insert(a.instructions().len(), lbl.0.clone());
        }
        Instruction::AllocateStack(off) => {
            if *off != 0 {
                a.sub(gpr64::rsp, *off)?;
            }
        }
        Instruction::Ret => {
            a.mov(gpr64::rsp, gpr64::rbp)?;
            a.pop(gpr64::rbp)?;
            a.ret()?;
        }
        Instruction::DeallocateStack(off) => {
            a.add(gpr64::rsp, *off)?;
        }
        Instruction::Push(op) => match op {
            Operand::Imm(c) => a.push(*c)?,
            Operand::Reg(reg) => a.push(gpr64_reg(reg))?,
            Operand::Stack(off) => a.push(qword_ptr(gpr64::rbp - (-*off)))?,
            Operand::Pseudo(_) | Operand::Data(_) => unreachable!(),
        },
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
