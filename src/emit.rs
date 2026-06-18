//! # Emitter (text) — Assembly AST to AT&T-Syntax Text Assembly
//!
//! Deprecated text-based emitter (enabled via `--no-iced`). Generates AT&T-syntax
//! x86-64 assembly as a [`String`], intended to be written to a `.s` file and
//! assembled by the system `as` assembler.
//!
//! ## Call Order
//!
//! ```text
//! emit_program()                — public entry point, returns full assembly text
//!   ├─ emit_function()          — .globl directive, prologue, instruction loop
//!   │    └─ emit_instruction()  — single instruction to AT&T syntax
//!   │         └─ emit_operand() — register/immediate/stack/data operand formatting
//!   └─ emit_static_variable()   — .data/.bss directives for static vars
//! ```

use crate::codegen::{
    AssemblyType, BinaryOp, FunctionDefinition, Instruction, Operand, Program, Reg, StaticConstant, StaticVariable,
    UnaryOp,
};
use crate::tacky::VarInit;
use crate::validate;

enum RegWidth {
    Byte,
    DWord,
    QWord,
    Xmm,
}

impl RegWidth {
    fn from_size(size: &AssemblyType) -> Self {
        match size {
            AssemblyType::Longword => RegWidth::DWord,
            AssemblyType::Quadword => RegWidth::QWord,
            AssemblyType::Double => RegWidth::Xmm,
        }
    }
}

/// Integer instruction size suffix (`l`/`q`). `Double` has no suffix — SSE instructions use
/// dedicated mnemonics (`movsd`, `addsd`, …) and are emitted on their own paths.
fn size_suffix(size: &AssemblyType) -> &'static str {
    match size {
        AssemblyType::Longword => "l",
        AssemblyType::Quadword => "q",
        AssemblyType::Double => unreachable!("SSE instructions use dedicated mnemonics, not a size suffix"),
    }
}

fn emit_reg(reg: &Reg, reg_width: &RegWidth) -> &'static str {
    match reg_width {
        RegWidth::Byte => match reg {
            Reg::AX => "al",
            Reg::DX => "dl",
            Reg::CX => "cl",
            Reg::DI => "dil",
            Reg::SI => "sil",
            Reg::R8 => "r8b",
            Reg::R9 => "r9b",
            Reg::R10 => "r10b",
            Reg::R11 => "r11b",
            Reg::SP => "spl",
            _ => unreachable!("XMM register requested with a general-purpose width"),
        },
        RegWidth::DWord => match reg {
            Reg::AX => "eax",
            Reg::DX => "edx",
            Reg::CX => "ecx",
            Reg::DI => "edi",
            Reg::SI => "esi",
            Reg::R8 => "r8d",
            Reg::R9 => "r9d",
            Reg::R10 => "r10d",
            Reg::R11 => "r11d",
            Reg::SP => "esp",
            _ => unreachable!("XMM register requested with a general-purpose width"),
        },
        RegWidth::QWord => match reg {
            Reg::AX => "rax",
            Reg::DX => "rdx",
            Reg::CX => "rcx",
            Reg::DI => "rdi",
            Reg::SI => "rsi",
            Reg::R8 => "r8",
            Reg::R9 => "r9",
            Reg::R10 => "r10",
            Reg::R11 => "r11",
            Reg::SP => "rsp",
            _ => unreachable!("XMM register requested with a general-purpose width"),
        },
        // SSE registers are width-independent
        RegWidth::Xmm => match reg {
            Reg::XMM0 => "xmm0",
            Reg::XMM1 => "xmm1",
            Reg::XMM2 => "xmm2",
            Reg::XMM3 => "xmm3",
            Reg::XMM4 => "xmm4",
            Reg::XMM5 => "xmm5",
            Reg::XMM6 => "xmm6",
            Reg::XMM7 => "xmm7",
            Reg::XMM14 => "xmm14",
            Reg::XMM15 => "xmm15",
            _ => unreachable!("general-purpose register requested with XMM width"),
        },
    }
}

fn emit_unaryop(op: &UnaryOp, size: &AssemblyType) -> String {
    let suffix = size_suffix(size);
    match op {
        UnaryOp::Neg => format!("neg{suffix}"),
        UnaryOp::Not => format!("not{suffix}"),
        // unary shift-by-1 (the round-to-odd step in u64 -> double)
        UnaryOp::Shr => format!("shr{suffix}"),
    }
}

fn emit_binaryop(op: &BinaryOp, size: &AssemblyType) -> String {
    let suffix = size_suffix(size);
    match op {
        BinaryOp::Add => format!("add{suffix}"),
        BinaryOp::Sub => format!("sub{suffix}"),
        BinaryOp::Mult => format!("imul{suffix}"),
        BinaryOp::BitAnd => format!("and{suffix}"),
        BinaryOp::BitOr => format!("or{suffix}"),
        BinaryOp::BitXOr => format!("xor{suffix}"),
        BinaryOp::BitShl => format!("shl{suffix}"),
        BinaryOp::BitSar => format!("sar{suffix}"),
        BinaryOp::BitShr => format!("shr{suffix}"),
        // SSE scalar divide is emitted directly in emit_instruction (dedicated mnemonic, no suffix)
        BinaryOp::DivDouble => unreachable!("divsd is handled on the double Binary path"),
    }
}

fn emit_operand(operand: &Operand, reg_width: &RegWidth) -> String {
    let mut output = String::new();
    match operand {
        Operand::Imm(value) => {
            output.push_str(&format!("${value}"));
        }
        Operand::Reg(reg) => {
            output.push_str(&format!("%{}", emit_reg(reg, reg_width)));
        }
        Operand::Stack(offset) => {
            output.push_str(&format!("{offset}(%rbp)"));
        }
        &Operand::Pseudo(_) => unreachable!("Must be eliminated before emission"),
        Operand::Data(name) => {
            // RIP-relative addressing for static/extern variables
            if cfg!(target_os = "macos") {
                output.push_str(&format!("_{name}(%rip)"));
            } else {
                output.push_str(&format!("{name}(%rip)"));
            }
        }
    }
    output
}

/// Emits AT&T-syntax x86-64 assembly text for a single instruction.
///
/// `fn_name` is used to prefix labels and jump targets, ensuring they are scoped
/// to the enclosing function (e.g., `main.label0`).
fn emit_instruction(ins: &Instruction, fn_name: &str) -> String {
    let mut output = String::new();
    match ins {
        Instruction::Mov { src, dst, size: AssemblyType::Double } => {
            output.push_str(&format!(
                "movsd {}, {}",
                emit_operand(src, &RegWidth::Xmm),
                emit_operand(dst, &RegWidth::Xmm)
            ));
        }
        Instruction::Mov { src, dst, size } => {
            let suffix = size_suffix(size);
            let width = RegWidth::from_size(size);
            output.push_str(&format!(
                "mov{suffix} {}, {}",
                emit_operand(src, &width),
                emit_operand(dst, &width)
            ));
        }
        Instruction::Movsx { src, dst } => {
            // movslq - sign-extend 32-bit to 64-bit
            output.push_str(&format!(
                "movslq {}, {}",
                emit_operand(src, &RegWidth::DWord),
                emit_operand(dst, &RegWidth::QWord)
            ));
        }
        Instruction::Ret => {
            output.push_str("movq %rbp, %rsp\n");
            output.push_str("\tpopq %rbp\n");
            output.push_str("\tret");
        }
        Instruction::Unary { op, dst, size } => {
            let width = RegWidth::from_size(size);
            output.push_str(&format!("{} {}\n", emit_unaryop(op, size), emit_operand(dst, &width)));
        }
        Instruction::Binary { op, src, dst, size: AssemblyType::Double } => {
            // SSE scalar-double arithmetic: dedicated mnemonics, XMM operands
            let mnemonic = match op {
                BinaryOp::Add => "addsd",
                BinaryOp::Sub => "subsd",
                BinaryOp::Mult => "mulsd",
                BinaryOp::DivDouble => "divsd",
                BinaryOp::BitXOr => "xorpd",
                _ => unreachable!("non-SSE binary op with Double size: {op:?}"),
            };
            output.push_str(&format!(
                "{mnemonic} {}, {}",
                emit_operand(src, &RegWidth::Xmm),
                emit_operand(dst, &RegWidth::Xmm)
            ));
        }
        Instruction::Binary {
            op: op @ (BinaryOp::BitShl | BinaryOp::BitSar | BinaryOp::BitShr),
            src,
            dst,
            size,
        } => {
            // shl, sar, and shr always use an immediate or cl register as the source operand
            let width = RegWidth::from_size(size);
            output.push_str(&format!(
                "{} {}, {}",
                emit_binaryop(op, size),
                emit_operand(src, &RegWidth::Byte),
                emit_operand(dst, &width)
            ));
        }
        Instruction::Binary { op, src, dst, size } => {
            let width = RegWidth::from_size(size);
            output.push_str(&format!(
                "{} {}, {}",
                emit_binaryop(op, size),
                emit_operand(src, &width),
                emit_operand(dst, &width)
            ));
        }
        Instruction::Idiv(op, size) => {
            let suffix = size_suffix(size);
            let width = RegWidth::from_size(size);
            output.push_str(&format!("idiv{suffix} {} ", emit_operand(op, &width)));
        }
        Instruction::Div(op, size) => {
            let suffix = size_suffix(size);
            let width = RegWidth::from_size(size);
            output.push_str(&format!("div{suffix} {} ", emit_operand(op, &width)));
        }
        Instruction::MovZeroExtend { .. } => unreachable!("MovZeroExtend in emit"),
        Instruction::Cvttsd2si { src, dst, size } => {
            // double -> signed int: src is the double (xmm/mem), dst a GP register of width `size`
            let dst_width = RegWidth::from_size(size);
            output.push_str(&format!(
                "cvttsd2si {}, {}",
                emit_operand(src, &RegWidth::Xmm),
                emit_operand(dst, &dst_width)
            ));
        }
        Instruction::Cvtsi2sd { src, dst, size } => {
            // int -> double: src is the integer (GP reg/mem of width `size`), dst an XMM register.
            // The integer-size suffix (l/q) disambiguates a memory source for the assembler.
            let src_width = RegWidth::from_size(size);
            output.push_str(&format!(
                "cvtsi2sd{} {}, {}",
                size_suffix(size),
                emit_operand(src, &src_width),
                emit_operand(dst, &RegWidth::Xmm)
            ));
        }
        Instruction::Cdq(size) => {
            let ins = match size {
                AssemblyType::Longword => "cdq",
                AssemblyType::Quadword => "cqo",
                AssemblyType::Double => unreachable!("cdq/cqo are integer-only"),
            };
            output.push_str(ins);
        }
        Instruction::Cmp { v1, v2, size: AssemblyType::Double } => {
            // comisd: AT&T order is `comisd src, dst`; v2 is the XMM register (fix_invalid)
            output.push_str(&format!(
                "comisd {}, {}",
                emit_operand(v1, &RegWidth::Xmm),
                emit_operand(v2, &RegWidth::Xmm)
            ));
        }
        Instruction::Cmp { v1, v2, size } => {
            let suffix = size_suffix(size);
            let width = RegWidth::from_size(size);
            output.push_str(&format!(
                "cmp{suffix} {}, {}",
                emit_operand(v1, &width),
                emit_operand(v2, &width)
            ));
        }
        Instruction::Jmp(target) => {
            output.push_str(&format!("jmp {}.{}", fn_name, target.0));
        }
        Instruction::JmpCC { code, label } => {
            output.push_str(&format!("j{} {}.{}", code.ins_suffix(), fn_name, label.0));
        }
        Instruction::Label(label) => {
            output.push_str(&format!("{}.{}:", fn_name, label.0));
        }
        Instruction::SetCC { code, op } => {
            output.push_str(&format!(
                "set{} {}",
                code.ins_suffix(),
                emit_operand(op, &RegWidth::Byte)
            ));
        }
        Instruction::Push(op) => {
            output.push_str(&format!("pushq {}", emit_operand(op, &RegWidth::QWord)));
        }
        Instruction::Call(name) => {
            if cfg!(target_os = "macos") {
                output.push_str(&format!("call _{}", name.0));
            } else {
                output.push_str(&format!("call {}@PLT", name.0));
            }
        }
    }
    output
}

fn emit_function(fun_def: &FunctionDefinition) -> String {
    let mut output = String::new();
    let FunctionDefinition { name, body, global } = fun_def;
    let processed_name = if cfg!(target_os = "macos") {
        format!("_{name}")
    } else {
        name.to_string()
    };
    if *global {
        output.push_str(&format!("\t.globl {processed_name}\n"));
    }
    output.push_str(&format!("{processed_name}:\n"));
    output.push_str("\tpushq %rbp\n");
    output.push_str("\tmovq %rsp, %rbp\n");
    for ins in body.iter() {
        output.push_str(&format!("\t{}\n", emit_instruction(ins, name)));
    }
    output
}

/// Emits a static variable as AT&T-syntax assembly directives.
///
/// Places zero-initialized variables in `.bss` and non-zero variables in `.data`,
/// with appropriate alignment and size directives (`.long` for 32-bit int/uint, `.quad`
/// for 64-bit long/ulong).
/// Extern variables (unresolved by the linker) produce no output.
/// On macOS, symbol names are prefixed with `_`.
fn emit_static_variable(sv: &StaticVariable) -> String {
    let mut output = String::new();
    let StaticVariable {
        name,
        global,
        init,
        alignment,
    } = sv;
    let processed_name = if cfg!(target_os = "macos") {
        format!("_{name}")
    } else {
        name.to_string()
    };

    // Extern variables don't need any output - they're resolved by the linker
    let VarInit::Defined(init_val) = init else {
        return output;
    };

    if *global {
        output.push_str(&format!("\t.globl {processed_name}\n"));
    }

    match init_val {
        validate::StaticInit::IntInit(0)
        | validate::StaticInit::LongInit(0)
        | validate::StaticInit::UIntInit(0)
        | validate::StaticInit::ULongInit(0) => {
            // BSS section for zero-initialized data
            output.push_str("\t.bss\n");
            output.push_str(&format!("\t.align {alignment}\n"));
            output.push_str(&format!("{processed_name}:\n"));
            output.push_str(&format!("\t.zero {alignment}\n"));
        }
        nonzero => {
            // Data section: directive (.long/.quad) and value follow the type
            let (directive, value) = nonzero.data_directive();
            output.push_str("\t.data\n");
            output.push_str(&format!("\t.align {alignment}\n"));
            output.push_str(&format!("{processed_name}:\n"));
            output.push_str(&format!("\t{directive} {value}\n"));
        }
    }

    output
}

/// Emits a `double` constant-pool entry as read-only data.
///
/// Placed in `.rodata` (ELF) / `.const` (Mach-O) with the raw IEEE-754 bit pattern via `.quad`,
/// matching the bytes the iced backend writes. Constants are always internal (no `.globl`); on
/// macOS the symbol name is `_`-prefixed to match how [`emit_operand`] references `Data` operands.
fn emit_static_constant(sc: &StaticConstant) -> String {
    let StaticConstant { name, init, alignment } = sc;
    let validate::StaticInit::DoubleInit(d) = init else {
        return String::new(); // only doubles live in the constant pool
    };
    let processed_name = if cfg!(target_os = "macos") {
        format!("_{name}")
    } else {
        name.to_string()
    };

    let mut output = String::new();
    if cfg!(target_os = "macos") {
        output.push_str("\t.const\n");
    } else {
        output.push_str("\t.section .rodata\n");
    }
    output.push_str(&format!("\t.align {alignment}\n"));
    output.push_str(&format!("{processed_name}:\n"));
    output.push_str(&format!("\t.quad {:#018x}\n", d.to_bits()));
    output
}

pub fn emit_program(program: &Program) -> String {
    let mut output = String::new();

    // Emit functions in text section
    if !program.functions.is_empty() {
        output.push_str("\t.text\n");
        for function in &program.functions {
            output.push_str(&emit_function(function));
        }
    }

    // Emit static variables
    for sv in &program.static_vars {
        output.push_str(&emit_static_variable(sv));
    }

    // Emit the double constant pool (read-only)
    for sc in &program.static_constants {
        output.push_str(&emit_static_constant(sc));
    }

    if cfg!(target_os = "linux") {
        output.push_str("\n.section .note.GNU-stack,\"\",@progbits\n");
    }
    output
}
