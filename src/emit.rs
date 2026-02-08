use crate::codegen::{AssemblyType, BinaryOp, FunctionDefinition, Instruction, Operand, Program, Reg, UnaryOp, StaticVariable};
use crate::tacky::{VarInit};
use crate::validate;

enum RegWidth {
    Byte,
    DWord,
    QWord,
}

impl RegWidth {
    fn from_size(size: &AssemblyType) -> Self {
        match size {
            AssemblyType::Longword => RegWidth::DWord,
            AssemblyType::Quadword => RegWidth::QWord,
        }
    }
}

fn size_suffix(size: &AssemblyType) -> &'static str {
    match size {
        AssemblyType::Longword => "l",
        AssemblyType::Quadword => "q",
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
            Reg::SP  => "rsp",
        },
    }
}

fn emit_unaryop(op: &UnaryOp, size: &AssemblyType) -> String {
    let suffix = size_suffix(size);
    match op {
        UnaryOp::Neg => format!("neg{suffix}"),
        UnaryOp::Not => format!("not{suffix}"),
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

fn emit_instruction(ins: &Instruction, fn_name: &str) -> String {
    let mut output = String::new();
    match ins {
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
        Instruction::Binary {
            op: op @ (BinaryOp::BitShl | BinaryOp::BitSar),
            src,
            dst,
            size,
        } => {
            // shl and sar always use an immediate or cl register as the source operand
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
        Instruction::Cdq(size) => {
            let ins = match size {
                AssemblyType::Longword => "cdq",
                AssemblyType::Quadword => "cqo",
            };
            output.push_str(ins);
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

fn emit_static_variable(sv: &StaticVariable) -> String {
    let mut output = String::new();
    let StaticVariable { name, global, init, alignment } = sv;
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
        validate::StaticInt::IntInit(0) | validate::StaticInt::LongInit(0) => {
            // BSS section for zero-initialized data
            output.push_str("\t.bss\n");
            output.push_str(&format!("\t.align {alignment}\n"));
            output.push_str(&format!("{processed_name}:\n"));
            output.push_str(&format!("\t.zero {alignment}\n"));
        }
        validate::StaticInt::IntInit(val) => {
            // Data section for initialized int
            output.push_str("\t.data\n");
            output.push_str("\t.align 4\n");
            output.push_str(&format!("{processed_name}:\n"));
            output.push_str(&format!("\t.long {val}\n"));
        }
        validate::StaticInt::LongInit(val) => {
            // Data section for initialized long
            output.push_str("\t.data\n");
            output.push_str("\t.align 8\n");
            output.push_str(&format!("{processed_name}:\n"));
            output.push_str(&format!("\t.quad {val}\n"));
        }
    }

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

    if cfg!(target_os = "linux") {
        output.push_str("\n.section .note.GNU-stack,\"\",@progbits\n");
    }
    output
}
