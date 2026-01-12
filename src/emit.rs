use crate::codegen::{BinaryOp, FunctionDefinition, Instruction, Operand, Program, Reg, UnaryOp};
use crate::tacky::{StaticVariable, VarInit};

enum RegWidth {
    Byte,
    DWord,
    QWord,
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
        },
    }
}

fn emit_unaryop(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "negl",
        UnaryOp::Not => "notl",
    }
}

fn emit_binaryop(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "addl",
        BinaryOp::Sub => "subl",
        BinaryOp::Mult => "imull",
        BinaryOp::BitAnd => "andl",
        BinaryOp::BitOr => "orl",
        BinaryOp::BitXOr => "xorl",
        BinaryOp::BitShl => "shll",
        BinaryOp::BitSar => "sarl",
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
        }
        Instruction::DeallocateStack(offset) => {
            output.push_str(&format!("addq ${}, %rsp", offset));
        }
        Instruction::Push(op) => {
            output.push_str(&format!("pushq {}", emit_operand(op, &RegWidth::QWord)));
        }
        Instruction::Call(name) => {
            // Use name.0 to get raw function name without .L prefix
            // On Linux, use @PLT for external function calls
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
    let StaticVariable { name, global, init } = sv;
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

    if *init_val == 0 {
        // BSS section for zero-initialized data
        output.push_str("\t.bss\n");
        output.push_str("\t.align 4\n");
        output.push_str(&format!("{processed_name}:\n"));
        output.push_str("\t.zero 4\n");
    } else {
        // Data section for initialized data
        output.push_str("\t.data\n");
        output.push_str("\t.align 4\n");
        output.push_str(&format!("{processed_name}:\n"));
        output.push_str(&format!("\t.long {init_val}\n"));
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
