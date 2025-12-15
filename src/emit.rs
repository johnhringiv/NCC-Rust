use crate::codegen::{Reg, UnaryOp, BinaryOp, Operand, Instruction, FunctionDefinition, Program};

enum RegWidth {
    Byte,
    DWord,
    QWord,
}

fn emit_reg(reg: &Reg, reg_width: &RegWidth) -> String {
    match reg_width {
        RegWidth::Byte => match reg {
            Reg::AX => "al".to_string(),
            Reg::DX => "dl".to_string(),
            Reg::CX => "cl".to_string(),
            Reg::DI => "dil".to_string(),
            Reg::SI => "sil".to_string(),
            Reg::R8 => "r8b".to_string(),
            Reg::R9 => "r9b".to_string(),
            Reg::R10 => "r10b".to_string(),
            Reg::R11 => "r11b".to_string(),
        },
        RegWidth::DWord => match reg {
            Reg::AX => "eax".to_string(),
            Reg::DX => "edx".to_string(),
            Reg::CX => "ecx".to_string(),
            Reg::DI => "edi".to_string(),
            Reg::SI => "esi".to_string(),
            Reg::R8 => "r8d".to_string(),
            Reg::R9 => "r9d".to_string(),
            Reg::R10 => "r10d".to_string(),
            Reg::R11 => "r11d".to_string(),
        },
        RegWidth::QWord => match reg {
            Reg::AX => "rax".to_string(),
            Reg::DX => "rdx".to_string(),
            Reg::CX => "rcx".to_string(),
            Reg::DI => "rdi".to_string(),
            Reg::SI => "rsi".to_string(),
            Reg::R8 => "r8".to_string(),
            Reg::R9 => "r9".to_string(),
            Reg::R10 => "r10".to_string(),
            Reg::R11 => "r11".to_string(),
        },
    }
}

fn emit_unaryop(op: &UnaryOp) -> String {
    match op {
        UnaryOp::Neg => "negl".to_string(),
        UnaryOp::Not => "notl".to_string(),
    }
}

fn emit_binaryop(op: &BinaryOp) -> String {
    match op {
        BinaryOp::Add => "addl".to_string(),
        BinaryOp::Sub => "subl".to_string(),
        BinaryOp::Mult => "imull".to_string(),
        BinaryOp::BitAnd => "andl".to_string(),
        BinaryOp::BitOr => "orl".to_string(),
        BinaryOp::BitXOr => "xorl".to_string(),
        BinaryOp::BitShl => "shll".to_string(),
        BinaryOp::BitSar => "sarl".to_string(),
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
    }
    output
}

fn emit_instruction(ins: &Instruction, fn_name: &str) -> String {
    fn emit_operand_dw(operand: &Operand) -> String {
        emit_operand(operand, &RegWidth::DWord)
    }
    let mut output = String::new();
    match ins {
        Instruction::Mov { src, dst } => {
            output.push_str(&format!("movl {}, {}", emit_operand_dw(src), emit_operand_dw(dst)));
        }
        Instruction::Ret => {
            output.push_str("movq %rbp, %rsp\n");
            output.push_str("\tpopq %rbp\n");
            output.push_str("\tret");
        }
        Instruction::Unary { op, dst } => {
            output.push_str(&format!("{} {}\n", emit_unaryop(op), emit_operand_dw(dst)));
        }
        Instruction::AllocateStack(offset) => {
            output.push_str(&format!("subq ${}, %rsp", offset));
        }
        Instruction::Binary {
            op: op @ (BinaryOp::BitShl | BinaryOp::BitSar),
            src,
            dst,
        } => {
            // shr and sar always use an immediate or cl register as the source operand
            output.push_str(&format!(
                "{} {}, {}",
                emit_binaryop(op),
                emit_operand(src, &RegWidth::Byte),
                emit_operand_dw(dst)
            ));
        }
        Instruction::Binary { op, src, dst } => {
            output.push_str(&format!(
                "{} {}, {}",
                emit_binaryop(op),
                emit_operand_dw(src),
                emit_operand_dw(dst)
            ));
        }
        Instruction::Idiv(op) => {
            output.push_str(&format!("idivl {} ", emit_operand_dw(op)));
        }
        Instruction::Cdq => {
            output.push_str("cdq");
        }
        Instruction::Cmp { v1, v2 } => {
            output.push_str(&format!("cmpl {}, {}", emit_operand_dw(v1), emit_operand_dw(v2)));
        }
        Instruction::Jmp(target) => {
            // Use raw label name with function prefix for uniqueness
            output.push_str(&format!("jmp {}.{}", fn_name, target.0));
        }
        Instruction::JmpCC { code, label } => {
            output.push_str(&format!("j{} {}.{}", code.ins_suffix(), fn_name, label.0));
        }
        Instruction::Label(label) => {
            // Use raw label name with function prefix for uniqueness
            output.push_str(&format!("{}.{}:", fn_name, label.0));
        }
        Instruction::SetCC { code, op } => {
            output.push_str(&format!(
                "set{} {}",
                code.ins_suffix(),
                emit_operand(op, &RegWidth::Byte)
            ));
        },
        Instruction::DeallocateStack(offset) => {
            output.push_str(&format!("addq ${}, %rsp", offset));
        },
        Instruction::Push(op) => {
            output.push_str(&format!("pushq {}", emit_operand(op, &RegWidth::QWord)));
        },
        Instruction::Call(name) => {
            // Use name.0 to get raw function name without .L prefix
            // On Linux, use @PLT for external function calls
            if cfg!(target_os = "macos") {
                output.push_str(&format!("call _{}", name.0));
            } else {
                output.push_str(&format!("call {}@PLT", name.0));
            }
        },
    }
    output
}

fn emit_function(fun_def: &FunctionDefinition) -> String {
    let mut output = String::new();
    let FunctionDefinition { name, body } = fun_def;
    let processed_name = if cfg!(target_os = "macos") {
        format!("_{name}")
    } else {
        name.to_string()
    };
    output.push_str(&format!("\t.global {processed_name}\n"));
    output.push_str(&format!("{processed_name}:\n"));
    output.push_str("\tpushq %rbp\n");
    output.push_str("\tmovq %rsp, %rbp\n");
    for ins in body.iter() {
        output.push_str(&format!("\t{}\n", emit_instruction(ins, name)));
    }
    output
}

pub fn emit_program(program: &Program) -> String {
    let mut output = String::new();
    for function in &program.functions {
        output.push_str(&emit_function(function));
    }
    if cfg!(target_os = "linux") {
        output.push_str("\n.section .note.GNU-stack,\"\",@progbits\n");
    }
    output
}
