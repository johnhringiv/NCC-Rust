//! # Emitter (iced-x86) — Assembly AST to Relocatable Object File
//!
//! Primary emitter that encodes the assembly AST into machine code using
//! [iced-x86](https://github.com/icedland/iced) and writes ELF (Linux) or
//! Mach-O (macOS) object files using the [object](https://github.com/gimli-rs/object) crate.
//!
//! ## Technical Approach
//!
//! Labels are pre-created for all functions and static variables so that forward
//! references resolve naturally. The [`CodeAssembler`] accumulates instructions, then
//! the block encoder resolves label addresses and produces a flat code buffer with
//! instruction-offset metadata. Relocations for external calls (`E8` + placeholder)
//! and RIP-relative data accesses are computed from the offset metadata and added to
//! the object file for the linker to patch.
//!
//! ## What This Pass Accomplishes
//!
//! - Encodes all x86-64 instructions to machine code (no external assembler needed)
//! - Builds a complete object file with `.text`, `.data`, and `.bss` sections
//! - Emits symbol table entries with correct scope (global vs file-local)
//! - Emits relocations for external function calls and static variable references
//! - Handles platform differences (ELF vs Mach-O implicit addend patching)
//! - Provides [`get_instructions`] for disassembly/pretty-printing with symbol names
//!
//! ## Call Order
//!
//! ```text
//! emit_object()                          — public entry point (returns object bytes)
//!   └─ emit_object_with_labels()         — core: assemble + build object file
//!        └─ emit_function_body()         — prologue + instruction loop
//!             └─ emit_instruction()      — encode one instruction, track relocations
//!                  ├─ gpr32() / gpr64_reg()  — register mapping helpers
//!                  ├─ mem_rbp()              — [rbp+offset] memory operands
//!                  └─ make_lbl_ptr()         — [label] RIP-relative operands
//!
//! get_instructions()                     — public (for pretty-printing)
//!   └─ emit_object_with_labels()         — generate object, then decode back
//! ```

use crate::codegen::{self, AssemblyType, BinaryOp, CondCode, Instruction, Operand, Reg, StaticVariable, UnaryOp};
use crate::tacky::VarInit;
use crate::validate::StaticInt;
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
use std::rc::Rc;

/// Creates an undefined symbol (resolved by linker at link time).
fn undefined_symbol(name: &str, kind: SymbolKind) -> Symbol {
    Symbol {
        name: name.as_bytes().to_vec(),
        value: 0,
        size: 0,
        kind,
        scope: SymbolScope::Unknown,
        weak: false,
        section: SymbolSection::Undefined,
        flags: SymbolFlags::None,
    }
}

/// Symbol resolver for displaying labels in assembly output.
///
/// Handles two types of symbol resolution:
/// - `symbols`: Direct address-to-name mapping for functions and internal labels
/// - `reloc_symbols`: Instruction IP to symbol name for RIP-relative data references
pub struct MySymbolResolver {
    symbols: HashMap<u64, Rc<str>>,
    reloc_symbols: HashMap<u64, Rc<str>>,
}

impl MySymbolResolver {
    fn new(symbols: HashMap<u64, Rc<str>>, reloc_symbols: HashMap<u64, Rc<str>>) -> Self {
        Self { symbols, reloc_symbols }
    }
}

impl SymbolResolver for MySymbolResolver {
    fn symbol(
        &'_ mut self,
        instruction: &iced_x86::Instruction,
        _operand: u32,
        _instruction_operand: Option<u32>,
        address: u64,
        _address_size: u32,
    ) -> Option<SymbolResult<'_>> {
        // First check direct address mapping (functions, internal labels)
        if let Some(name) = self.symbols.get(&address) {
            return Some(SymbolResult::with_str(address, name));
        }
        // Then check relocation-based symbols (data references)
        if let Some(name) = self.reloc_symbols.get(&instruction.ip()) {
            return Some(SymbolResult::with_str(address, name));
        }
        None
    }
}

#[allow(clippy::type_complexity)]
pub fn get_instructions(
    program: &codegen::Program,
) -> Result<(Vec<iced_x86::Instruction>, MySymbolResolver, HashMap<usize, Rc<str>>), Box<dyn std::error::Error>> {
    use object::read::RelocationTarget;

    let (obj_bytes, internal_labels) = emit_object_with_labels(program)?;

    // Parse the object file
    let obj = object::File::parse(&*obj_bytes)?;

    // Extract .text section
    let text_section = obj.section_by_name(".text").ok_or("No .text section found")?;
    let code = text_section.data()?;

    // Build symbol index to name map
    let mut symbol_names: HashMap<object::SymbolIndex, Rc<str>> = HashMap::new();
    for symbol in obj.symbols() {
        if let Ok(name) = symbol.name() {
            symbol_names.insert(symbol.index(), Rc::from(name));
        }
    }

    // Build symbol resolver from object file symbols
    let mut address_map: HashMap<u64, Rc<str>> = HashMap::new();
    let mut function_offsets: HashMap<Rc<str>, u64> = HashMap::new();
    for symbol in obj.symbols() {
        if let Ok(name) = symbol.name()
            && !name.is_empty()
            && symbol.kind() == object::SymbolKind::Text
        {
            let name: Rc<str> = Rc::from(name);
            address_map.insert(symbol.address(), name.clone());
            function_offsets.insert(name, symbol.address());
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

    // Build instruction offset ranges: (start, end) for each instruction
    let mut ins_ranges: Vec<(usize, usize)> = Vec::new();
    let mut offset = 0;
    for ins in &instructions {
        ins_ranges.push((offset, offset + ins.len()));
        offset += ins.len();
    }

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

    // Build relocation-based symbol map: instruction IP -> symbol name
    // This allows the symbol resolver to show data symbol names for RIP-relative accesses
    let mut reloc_symbols: HashMap<u64, Rc<str>> = HashMap::new();
    for (reloc_offset, reloc) in text_section.relocations() {
        if let RelocationTarget::Symbol(sym_idx) = reloc.target()
            && let Some(symbol_name) = symbol_names.get(&sym_idx)
        {
            // Find which instruction contains this relocation
            for (start, end) in ins_ranges.iter() {
                if reloc_offset as usize >= *start && (reloc_offset as usize) < *end {
                    reloc_symbols.insert(*start as u64, symbol_name.clone());
                    break;
                }
            }
        }
    }

    Ok((
        instructions,
        MySymbolResolver::new(address_map, reloc_symbols),
        label_idx,
    ))
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
) -> Result<(Vec<u8>, HashMap<usize, Rc<str>>), Box<dyn std::error::Error>> {
    let mut a = CodeAssembler::new(64)?;

    // Create labels for all functions upfront
    let mut fn_labels: HashMap<Rc<str>, CodeLabel> = HashMap::new();
    for fun_def in &program.functions {
        fn_labels.insert(fun_def.name.clone(), a.create_label());
    }

    // Create labels for static variables (including extern) to get RIP-relative addressing
    let mut data_labels: HashMap<Rc<str>, CodeLabel> = HashMap::new();
    for sv in &program.static_vars {
        data_labels.insert(sv.name.clone(), a.create_label());
    }

    // Track external function calls (instruction index, function name)
    let mut external_calls: Vec<(usize, Rc<str>)> = Vec::new();
    let mut data_relocs: Vec<(usize, Rc<str>)> = Vec::new();

    // Track all internal labels across all functions
    let mut all_label_idx: HashMap<usize, Rc<str>> = HashMap::new();

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
    let mut func_offsets: Vec<(Rc<str>, u64, bool)> = Vec::new();
    for fun_def in &program.functions {
        let func_off = result.label_ip(fn_labels.get(&fun_def.name).unwrap())?;
        func_offsets.push((fun_def.name.clone(), func_off, fun_def.global));
    }

    // Clone instruction offsets before moving the inner
    let instruction_offsets = result.inner.new_instruction_offsets.clone();
    let constant_offsets = result.inner.constant_offsets.clone();

    // Now move the code buffer
    let mut code = result.inner.code_buffer;

    // On Mach-O, patch displacement values for data references.
    // The Mach-O linker uses implicit addends: final = existing + (symbol - P - 4)
    // We want: final = symbol - (P + bytes_remaining)
    // So: existing = -(bytes_remaining - 4)
    // For most instructions where bytes_remaining = 4 (displacement only), existing should be 0.
    if cfg!(target_os = "macos") {
        for (ins_idx, _var_name) in &data_relocs {
            let ins_offset = instruction_offsets[*ins_idx] as usize;
            let const_off = &constant_offsets[*ins_idx];
            let disp_offset = ins_offset + const_off.displacement_offset();
            let bytes_remaining = const_off.displacement_size() + const_off.immediate_size();
            // Mach-O linker adds 4 to pcrel addend, so we need existing = -(bytes_remaining - 4)
            let implicit_addend = -((bytes_remaining as i32) - 4);
            let bytes = implicit_addend.to_le_bytes();
            code[disp_offset..disp_offset + 4].copy_from_slice(&bytes);
        }
    }

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
        let mut obj = Object::new(BinaryFormat::MachO, Architecture::X86_64, Endianness::Little);
        // Without an LC_BUILD_VERSION load command, macOS `ld` warns
        // "no platform load command found ... assuming: macOS". Emit one targeting
        // macOS 11.0. MachOBuildVersion is #[non_exhaustive], so build via Default
        // then set fields. minos/sdk use the nibble-packed XXXX.YY.ZZ form (11.0 = 11 << 16).
        let mut build_version = object::write::MachOBuildVersion::default();
        build_version.platform = object::macho::PLATFORM_MACOS;
        build_version.minos = 11 << 16;
        build_version.sdk = 11 << 16;
        obj.set_macho_build_version(build_version);
        obj
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
    let mut static_var_symbols: HashMap<Rc<str>, SymbolId> = HashMap::new();
    for StaticVariable {
        name,
        global,
        init,
        alignment,
    } in &program.static_vars
    {
        let sym_id = match init {
            // Defined variable - allocate storage in .data or .bss
            VarInit::Defined(init_val) => {
                let (offset, section) = match init_val {
                    StaticInt::IntInit(0) | StaticInt::LongInit(0) => {
                        obj.append_section_bss(bss, *alignment, *alignment);
                        let offset = bss_offset;
                        bss_offset += alignment;
                        (offset, &bss)
                    }
                    StaticInt::IntInit(val) => {
                        let init_bytes = val.to_le_bytes();
                        obj.append_section_data(data, &init_bytes, *alignment);
                        let offset = data_offset;
                        data_offset += alignment;
                        (offset, &data)
                    }
                    StaticInt::LongInit(val) => {
                        let init_bytes = val.to_le_bytes();
                        obj.append_section_data(data, &init_bytes, *alignment);
                        let offset = data_offset;
                        data_offset += alignment;
                        (offset, &data)
                    }
                };

                obj.add_symbol(Symbol {
                    name: name.as_bytes().to_vec(),
                    value: offset,
                    size: *alignment,
                    kind: SymbolKind::Data,
                    scope: if *global {
                        SymbolScope::Dynamic
                    } else {
                        SymbolScope::Compilation
                    },
                    weak: false,
                    section: SymbolSection::Section(*section),
                    flags: SymbolFlags::None,
                })
            }
            // Extern variable - undefined symbol resolved by linker
            VarInit::Extern => obj.add_symbol(undefined_symbol(name, SymbolKind::Data)),
        };
        static_var_symbols.insert(name.clone(), sym_id);
    }

    // Add external function symbols and relocations
    let mut external_symbols: HashMap<Rc<str>, SymbolId> = HashMap::new();
    for (_ins_idx, func_name) in &external_calls {
        if !external_symbols.contains_key(func_name) {
            let symbol_id = obj.add_symbol(undefined_symbol(func_name, SymbolKind::Text));
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
#[allow(clippy::too_many_arguments)]
fn emit_function_body(
    a: &mut CodeAssembler,
    fun_def: &codegen::FunctionDefinition,
    fn_labels: &HashMap<Rc<str>, CodeLabel>,
    data_labels: &HashMap<Rc<str>, CodeLabel>,
    external_calls: &mut Vec<(usize, Rc<str>)>,
    data_relocs: &mut Vec<(usize, Rc<str>)>,
    labels: &mut HashMap<Rc<str>, CodeLabel>,
    label_idx: &mut HashMap<usize, Rc<str>>,
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
        SP => registers::gpr32::esp,
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
        SP => gpr64::rsp,
    }
}

/// Constructs a sized memory operand for a stack slot at `[rbp + offset]`.
///
/// Returns a `dword ptr` (Longword) or `qword ptr` (Quadword) memory reference.
/// Handles negative offsets via subtraction to satisfy iced-x86's operand API.
fn mem_rbp(offset: i32, asm_ty: AssemblyType) -> AsmMemoryOperand {
    let pos = if offset >= 0 {
        gpr64::rbp + offset
    } else {
        gpr64::rbp - (-offset)
    };
    match asm_ty {
        AssemblyType::Longword => dword_ptr(pos),
        AssemblyType::Quadword => qword_ptr(pos),
    }
}

fn make_lbl_ptr(lbl: &CodeLabel, asm_ty: &AssemblyType) -> AsmMemoryOperand {
    match asm_ty {
        AssemblyType::Longword => dword_ptr(*lbl),
        AssemblyType::Quadword => qword_ptr(*lbl),
    }
}

// Data operands (static variables) use RIP-relative addressing with relocations.
//
// Note: Many Data destination patterns (e.g., `Add(Reg, Data)`, `Neg(Data)`) are currently
// unreached because the codegen uses load→op→store sequences for static variables.
// These patterns will become reachable after implementing copy propagation and dead store
// elimination, which can optimize `tmp = x; tmp = tmp + 1; x = tmp` into `x = x + 1`.
#[allow(clippy::too_many_arguments)]
fn emit_instruction(
    a: &mut CodeAssembler,
    ins: &Instruction,
    labels: &mut HashMap<Rc<str>, CodeLabel>,
    label_idx: &mut HashMap<usize, Rc<str>>,
    fn_labels: &HashMap<Rc<str>, CodeLabel>,
    data_labels: &HashMap<Rc<str>, CodeLabel>,
    external_calls: &mut Vec<(usize, Rc<str>)>,
    data_relocs: &mut Vec<(usize, Rc<str>)>,
) -> Result<(), IcedError> {
    match ins {
        Instruction::Mov { src, dst, size } => match (src, dst, size) {
            (Operand::Imm(v), Operand::Reg(r), AssemblyType::Longword) => a.mov(gpr32(r), *v as i32)?,
            (Operand::Imm(v), Operand::Reg(r), AssemblyType::Quadword) => a.mov(gpr64_reg(r), *v)?,

            (Operand::Reg(s), Operand::Reg(d), AssemblyType::Longword) => a.mov(gpr32(d), gpr32(s))?,
            (Operand::Reg(s), Operand::Reg(d), AssemblyType::Quadword) => a.mov(gpr64_reg(d), gpr64_reg(s))?,

            (Operand::Reg(s), Operand::Stack(off), AssemblyType::Longword) => a.mov(mem_rbp(*off, *size), gpr32(s))?,
            (Operand::Reg(s), Operand::Stack(off), AssemblyType::Quadword) => {
                a.mov(mem_rbp(*off, *size), gpr64_reg(s))?
            }

            (Operand::Stack(off), Operand::Reg(d), AssemblyType::Longword) => a.mov(gpr32(d), mem_rbp(*off, *size))?,
            (Operand::Stack(off), Operand::Reg(d), AssemblyType::Quadword) => {
                a.mov(gpr64_reg(d), mem_rbp(*off, *size))?
            }
            (Operand::Data(name), Operand::Reg(d), _) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                match size {
                    AssemblyType::Longword => a.mov(gpr32(d), dword_ptr(*lbl))?,
                    AssemblyType::Quadword => a.mov(gpr64_reg(d), qword_ptr(*lbl))?,
                }
            }

            // large imm to memory is rewritten in fix_invalid, downcast is safe
            (Operand::Imm(v), Operand::Stack(off), _) => a.mov(mem_rbp(*off, *size), *v as i32)?,
            (Operand::Imm(v), Operand::Data(name), size) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.mov(make_lbl_ptr(lbl, size), *v as i32)?
            }

            (Operand::Reg(s), Operand::Data(name), _) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                match size {
                    AssemblyType::Longword => a.mov(dword_ptr(*lbl), gpr32(s))?,
                    AssemblyType::Quadword => a.mov(qword_ptr(*lbl), gpr64_reg(s))?,
                }
            }
            _ => unreachable!("unsupported mov combination: {:?}", ins),
        },
        Instruction::Movsx { src, dst } => match (src, dst) {
            (Operand::Reg(s), Operand::Reg(d)) => a.movsxd(gpr64_reg(d), gpr32(s))?,
            (Operand::Stack(off), Operand::Reg(d)) => a.movsxd(gpr64_reg(d), mem_rbp(*off, AssemblyType::Longword))?,
            (Operand::Data(name), Operand::Reg(d)) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.movsxd(gpr64_reg(d), dword_ptr(*lbl))?;
            }
            _ => unreachable!("unsupported movsx combination: {:?}", ins),
        },
        Instruction::Unary { op, dst, size } => match op {
            UnaryOp::Neg => match (dst, size) {
                (Operand::Reg(r), AssemblyType::Longword) => a.neg(gpr32(r))?,
                (Operand::Reg(r), AssemblyType::Quadword) => a.neg(gpr64_reg(r))?,
                (Operand::Stack(off), _) => a.neg(mem_rbp(*off, *size))?,
                (Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.neg(make_lbl_ptr(lbl, size))?
                }
                _ => unreachable!(),
            },
            UnaryOp::Not => match (dst, size) {
                (Operand::Reg(r), AssemblyType::Longword) => a.not(gpr32(r))?,
                (Operand::Reg(r), AssemblyType::Quadword) => a.not(gpr64_reg(r))?,
                (Operand::Stack(off), _) => a.not(mem_rbp(*off, *size))?,
                (Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.not(make_lbl_ptr(lbl, size))?
                }
                _ => unreachable!(),
            },
        },
        Instruction::Binary { op, src, dst, size } => match op {
            BinaryOp::Add => match (src, dst, size) {
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Longword) => a.add(gpr32(d), gpr32(s))?,
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Quadword) => a.add(gpr64_reg(d), gpr64_reg(s))?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => a.add(gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Longword) => {
                    a.add(mem_rbp(*off, *size), gpr32(s))?
                }
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Quadword) => {
                    a.add(mem_rbp(*off, *size), gpr64_reg(s))?
                }
                (Operand::Reg(s), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    match size {
                        AssemblyType::Longword => a.add(dword_ptr(*lbl), gpr32(s))?,
                        AssemblyType::Quadword => a.add(qword_ptr(*lbl), gpr64_reg(s))?,
                    }
                }

                // large imm is rewritten in fix_invalid, downcast is safe
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => a.add(gpr64_reg(d), *v as i32)?,
                (Operand::Imm(v), Operand::Stack(off), _) => a.add(mem_rbp(*off, *size), *v as i32)?,
                (Operand::Imm(v), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.add(make_lbl_ptr(lbl, size), *v as i32)?
                }
                _ => unreachable!(),
            },
            BinaryOp::Sub => match (src, dst, size) {
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Longword) => a.sub(gpr32(d), gpr32(s))?,
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Quadword) => a.sub(gpr64_reg(d), gpr64_reg(s))?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => a.sub(gpr32(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Longword) => {
                    a.sub(mem_rbp(*off, *size), gpr32(s))?
                }
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Quadword) => {
                    a.sub(mem_rbp(*off, *size), gpr64_reg(s))?
                }
                (Operand::Reg(s), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    match size {
                        AssemblyType::Longword => a.sub(dword_ptr(*lbl), gpr32(s))?,
                        AssemblyType::Quadword => a.sub(qword_ptr(*lbl), gpr64_reg(s))?,
                    }
                }

                // large imm is rewritten in fix_invalid, downcast is safe
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => a.sub(gpr64_reg(d), *v as i32)?,
                (Operand::Imm(v), Operand::Stack(off), _) => a.sub(mem_rbp(*off, *size), *v as i32)?,
                (Operand::Imm(v), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.sub(make_lbl_ptr(lbl, size), *v as i32)?
                }
                _ => unreachable!(),
            },
            BinaryOp::Mult => match (src, dst, size) {
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Longword) => a.imul_2(gpr32(d), gpr32(s))?,
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Quadword) => a.imul_2(gpr64_reg(d), gpr64_reg(s))?,
                (Operand::Stack(off), Operand::Reg(d), AssemblyType::Longword) => {
                    a.imul_2(gpr32(d), mem_rbp(*off, *size))?
                }
                (Operand::Stack(off), Operand::Reg(d), AssemblyType::Quadword) => {
                    a.imul_2(gpr64_reg(d), mem_rbp(*off, *size))?
                }
                (Operand::Data(name), Operand::Reg(d), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    match size {
                        AssemblyType::Longword => a.imul_2(gpr32(d), dword_ptr(*lbl))?,
                        AssemblyType::Quadword => a.imul_2(gpr64_reg(d), qword_ptr(*lbl))?,
                    }
                }

                // large imm is rewritten in fix_invalid, downcast is safe
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => {
                    a.imul_3(gpr32(d), gpr32(d), *v as i32)?
                }
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => {
                    a.imul_3(gpr64_reg(d), gpr64_reg(d), *v as i32)?
                }
                _ => unreachable!("Mult {:?}, {:?}", src, dst),
            },
            BinaryOp::BitAnd => match (src, dst, size) {
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Longword) => a.and(gpr32(d), gpr32(s))?,
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Quadword) => a.and(gpr64_reg(d), gpr64_reg(s))?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => a.and(gpr32(d), *v as i32)?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => a.and(gpr64_reg(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Longword) => {
                    a.and(mem_rbp(*off, *size), gpr32(s))?
                }
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Quadword) => {
                    a.and(mem_rbp(*off, *size), gpr64_reg(s))?
                }
                (Operand::Reg(s), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    match size {
                        AssemblyType::Longword => a.and(dword_ptr(*lbl), gpr32(s))?,
                        AssemblyType::Quadword => a.and(qword_ptr(*lbl), gpr64_reg(s))?,
                    }
                }

                // large imm is rewritten in fix_invalid, downcast is safe
                (Operand::Imm(v), Operand::Stack(off), _) => a.and(mem_rbp(*off, *size), *v as i32)?,
                (Operand::Imm(v), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.and(make_lbl_ptr(lbl, size), *v as i32)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitOr => match (src, dst, size) {
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Longword) => a.or(gpr32(d), gpr32(s))?,
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Quadword) => a.or(gpr64_reg(d), gpr64_reg(s))?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => a.or(gpr32(d), *v as i32)?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => a.or(gpr64_reg(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Longword) => {
                    a.or(mem_rbp(*off, *size), gpr32(s))?
                }
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Quadword) => {
                    a.or(mem_rbp(*off, *size), gpr64_reg(s))?
                }
                (Operand::Reg(s), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    match size {
                        AssemblyType::Longword => a.or(dword_ptr(*lbl), gpr32(s))?,
                        AssemblyType::Quadword => a.or(qword_ptr(*lbl), gpr64_reg(s))?,
                    }
                }

                // large imm is rewritten in fix_invalid, downcast is safe
                (Operand::Imm(v), Operand::Stack(off), _) => a.or(mem_rbp(*off, *size), *v as i32)?,
                (Operand::Imm(v), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.or(make_lbl_ptr(lbl, size), *v as i32)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitXOr => match (src, dst, size) {
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Longword) => a.xor(gpr32(d), gpr32(s))?,
                (Operand::Reg(s), Operand::Reg(d), AssemblyType::Quadword) => a.xor(gpr64_reg(d), gpr64_reg(s))?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => a.xor(gpr32(d), *v as i32)?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => a.xor(gpr64_reg(d), *v as i32)?,
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Longword) => {
                    a.xor(mem_rbp(*off, *size), gpr32(s))?
                }
                (Operand::Reg(s), Operand::Stack(off), AssemblyType::Quadword) => {
                    a.xor(mem_rbp(*off, *size), gpr64_reg(s))?
                }
                (Operand::Reg(s), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    match size {
                        AssemblyType::Longword => a.xor(dword_ptr(*lbl), gpr32(s))?,
                        AssemblyType::Quadword => a.xor(qword_ptr(*lbl), gpr64_reg(s))?,
                    }
                }

                // large imm is rewritten in fix_invalid, downcast is safe
                (Operand::Imm(v), Operand::Stack(off), _) => a.xor(mem_rbp(*off, *size), *v as i32)?,
                (Operand::Imm(v), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.xor(make_lbl_ptr(lbl, size), *v as i32)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitShl => match (src, dst, size) {
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => a.shl(gpr32(d), *v as i32)?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => a.shl(gpr64_reg(d), *v as i32)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d), AssemblyType::Longword) => a.shl(gpr32(d), gpr8::cl)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d), AssemblyType::Quadword) => a.shl(gpr64_reg(d), gpr8::cl)?,
                (Operand::Imm(v), Operand::Stack(off), _) => a.shl(mem_rbp(*off, *size), *v as i32)?,
                (Operand::Imm(v), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.shl(make_lbl_ptr(lbl, size), *v as i32)?
                }
                (Operand::Reg(Reg::CX), Operand::Stack(off), _) => a.shl(mem_rbp(*off, *size), gpr8::cl)?,
                (Operand::Reg(Reg::CX), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.shl(make_lbl_ptr(lbl, size), gpr8::cl)?
                }
                _ => unreachable!(),
            },
            BinaryOp::BitSar => match (src, dst, size) {
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Longword) => a.sar(gpr32(d), *v as i32)?,
                (Operand::Imm(v), Operand::Reg(d), AssemblyType::Quadword) => a.sar(gpr64_reg(d), *v as i32)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d), AssemblyType::Longword) => a.sar(gpr32(d), gpr8::cl)?,
                (Operand::Reg(Reg::CX), Operand::Reg(d), AssemblyType::Quadword) => a.sar(gpr64_reg(d), gpr8::cl)?,
                (Operand::Imm(v), Operand::Stack(off), _) => a.sar(mem_rbp(*off, *size), *v as i32)?,
                (Operand::Imm(v), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.sar(make_lbl_ptr(lbl, size), *v as i32)?
                }
                (Operand::Reg(Reg::CX), Operand::Stack(off), _) => a.sar(mem_rbp(*off, *size), gpr8::cl)?,
                (Operand::Reg(Reg::CX), Operand::Data(name), _) => {
                    let lbl = data_labels.get(name).unwrap();
                    data_relocs.push((a.instructions().len(), name.clone()));
                    a.sar(make_lbl_ptr(lbl, size), gpr8::cl)?
                }
                _ => unreachable!(),
            },
        },
        Instruction::Cmp { v1, v2, size } => match (v1, v2, size) {
            (Operand::Reg(r1), Operand::Reg(r2), AssemblyType::Longword) => a.cmp(gpr32(r2), gpr32(r1))?,
            (Operand::Reg(r1), Operand::Reg(r2), AssemblyType::Quadword) => a.cmp(gpr64_reg(r2), gpr64_reg(r1))?,
            (Operand::Reg(r1), Operand::Stack(off), AssemblyType::Longword) => {
                a.cmp(mem_rbp(*off, *size), gpr32(r1))?
            }
            (Operand::Reg(r1), Operand::Stack(off), AssemblyType::Quadword) => {
                a.cmp(mem_rbp(*off, *size), gpr64_reg(r1))?
            }
            (Operand::Reg(r1), Operand::Data(name), _) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                match size {
                    AssemblyType::Longword => a.cmp(dword_ptr(*lbl), gpr32(r1))?,
                    AssemblyType::Quadword => a.cmp(qword_ptr(*lbl), gpr64_reg(r1))?,
                }
            }
            (Operand::Stack(off), Operand::Reg(r), AssemblyType::Longword) => a.cmp(gpr32(r), mem_rbp(*off, *size))?,
            (Operand::Stack(off), Operand::Reg(r), AssemblyType::Quadword) => {
                a.cmp(gpr64_reg(r), mem_rbp(*off, *size))?
            }
            (Operand::Data(name), Operand::Reg(r), _) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                match size {
                    AssemblyType::Longword => a.cmp(gpr32(r), dword_ptr(*lbl))?,
                    AssemblyType::Quadword => a.cmp(gpr64_reg(r), qword_ptr(*lbl))?,
                }
            }
            (Operand::Imm(v), Operand::Reg(r), AssemblyType::Longword) => a.cmp(gpr32(r), *v as i32)?,
            (Operand::Imm(v), Operand::Reg(r), AssemblyType::Quadword) => a.cmp(gpr64_reg(r), *v as i32)?,
            (Operand::Imm(v), Operand::Stack(off), _) => a.cmp(mem_rbp(*off, *size), *v as i32)?,
            (Operand::Imm(v), Operand::Data(name), _) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.cmp(make_lbl_ptr(lbl, size), *v as i32)?
            }
            _ => unreachable!("unsupported cmp combination: v1={:?}, v2={:?}, size={:?}", v1, v2, size),
        },
        Instruction::Cdq(size) => match size {
            AssemblyType::Longword => a.cdq()?,
            AssemblyType::Quadword => a.cqo()?,
        },
        Instruction::Idiv(op, size) => match (op, size) {
            (Operand::Reg(r), AssemblyType::Longword) => a.idiv(gpr32(r))?,
            (Operand::Reg(r), AssemblyType::Quadword) => a.idiv(gpr64_reg(r))?,
            (Operand::Stack(off), _) => a.idiv(mem_rbp(*off, *size))?,
            (Operand::Data(name), _) => {
                let lbl = data_labels.get(name).unwrap();
                data_relocs.push((a.instructions().len(), name.clone()));
                a.idiv(make_lbl_ptr(lbl, size))?
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
                CondCode::E => a.sete(byte_ptr(gpr64::rbp - (-*off)))?,
                CondCode::NE => a.setne(byte_ptr(gpr64::rbp - (-*off)))?,
                CondCode::G => a.setg(byte_ptr(gpr64::rbp - (-*off)))?,
                CondCode::GE => a.setge(byte_ptr(gpr64::rbp - (-*off)))?,
                CondCode::L => a.setl(byte_ptr(gpr64::rbp - (-*off)))?,
                CondCode::LE => a.setle(byte_ptr(gpr64::rbp - (-*off)))?,
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
        Instruction::Ret => {
            a.mov(gpr64::rsp, gpr64::rbp)?;
            a.pop(gpr64::rbp)?;
            a.ret()?;
        }
        Instruction::Push(op) => match op {
            Operand::Imm(c) => a.push(*c as i32)?, // i64 handled in fix_invalid
            Operand::Reg(reg) => a.push(gpr64_reg(reg))?,
            Operand::Stack(off) => a.push(qword_ptr(gpr64::rbp - (-*off)))?,
            Operand::Pseudo(_) | Operand::Data(_) => unreachable!(),
        },
        Instruction::Call(name) => {
            if let Some(&lbl) = fn_labels.get(&name.0) {
                // Internal function call
                a.call(lbl)?;
            } else {
                // External function call - track for relocation
                let ins_count = a.instructions().len();
                external_calls.push((ins_count, name.0.clone()));
                // The Mach-O linker adds the existing displacement to (symbol - RIP).
                // We want the final result to be (symbol - RIP), so we need displacement = 0.
                // For ELF, the explicit addend in the relocation handles this, and the
                // existing displacement is ignored.
                // E8 = CALL rel32 opcode, 00 00 00 00 = 0 displacement
                a.db(&[0xE8, 0x00, 0x00, 0x00, 0x00])?;
            }
        }
    }
    Ok(())
}
