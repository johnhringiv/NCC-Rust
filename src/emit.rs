use crate::codegen;

enum RegWidth {
    Byte,
    DWord,
}
fn emit_reg(reg: &codegen::Reg, reg_width: &RegWidth) -> String {
    match reg_width {
        RegWidth::Byte => {
            match reg {
                codegen::Reg::AX => "al".to_string(),
                codegen::Reg::R10 => "r10b".to_string(),
                codegen::Reg::R11 => "r11b".to_string(),
                codegen::Reg::DX => "dl".to_string(),
                codegen::Reg::CX => "cl".to_string(),
            }
        }
        RegWidth::DWord => {
            match reg {
                codegen::Reg::AX => "eax".to_string(),
                codegen::Reg::R10 => "r10d".to_string(),
                codegen::Reg::R11 => "r11d".to_string(),
                codegen::Reg::DX => "edx".to_string(),
                codegen::Reg::CX => "ecx".to_string(),
            }
        }
    }
}

fn emit_unaryop(op: &codegen::UnaryOp) -> String {
    match op {
        codegen::UnaryOp::Neg => "negl".to_string(),
        codegen::UnaryOp::Not => "notl".to_string(),
    }
}

fn emit_binaryop(op: &codegen::BinaryOp) -> String {
    match op {
        codegen::BinaryOp::Add => "addl".to_string(),
        codegen::BinaryOp::Sub => "subl".to_string(),
        codegen::BinaryOp::Mult => "imull".to_string(),
        codegen::BinaryOp::BitAnd => "andl".to_string(),
        codegen::BinaryOp::BitOr => "orl".to_string(),
        codegen::BinaryOp::BitXOr => "xorl".to_string(),
        codegen::BinaryOp::BitShl => "shll".to_string(),
        codegen::BinaryOp::BitSar => "sarl".to_string(),
    }
}

fn emit_operand(operand: &codegen::Operand, reg_width: &RegWidth) -> String {
    let mut output = String::new();
    match operand {
        codegen::Operand::Imm(value) => {
            output.push_str(&format!("${}", value));
        },
        codegen::Operand::Reg(reg) => {
            output.push_str(&format!("%{}", emit_reg(reg, reg_width)));
        }
        codegen::Operand::Stack(offset) => {
            output.push_str(&format!("{}(%rbp)", offset));
        },
        &codegen::Operand::Pseudo(_) => unreachable!("Must be eliminated before emission"),
    }
    output
}

fn emit_instruction(ins: &codegen::Instruction) -> String {
    fn emit_operand_dw(operand: &codegen::Operand) -> String {
        emit_operand(operand, &RegWidth::DWord)
    }
    let mut output = String::new();
    match ins {
        codegen::Instruction::Mov { src, dst } => {
            output.push_str(&format!("movl {}, {}", emit_operand_dw(src), emit_operand_dw(dst)));
        }
        codegen::Instruction::Ret => {
            output.push_str("movq %rbp, %rsp\n");
            output.push_str("\tpopq %rbp\n");
            output.push_str("\tret");
        }
        codegen::Instruction::Unary { op, dst } => {
            output.push_str(&format!("{} {}\n", emit_unaryop(op), emit_operand_dw(dst)));
        }
        codegen::Instruction::AllocateStack(offset) => {
            output.push_str(&format!("subq ${}, %rsp", offset * -1));
        }
        codegen::Instruction::Binary { op, src, dst} => {
            output.push_str(&format!("{} {}, {}", emit_binaryop(op), emit_operand_dw(src), emit_operand_dw(dst)));
        }
        codegen::Instruction::Idiv(op) => {
            output.push_str(&format!("idivl {} ", emit_operand_dw(op)));
        }
        codegen::Instruction::Cdq => {output.push_str("cdq");}
        codegen::Instruction::Cmp { v1, v2 } => {
            output.push_str(&format!("cmpl {}, {}", emit_operand_dw(v1), emit_operand_dw(v2)));
        }
        codegen::Instruction::Jmp(target) => {
            output.push_str(&format!("jmp {}", target));
        }
        codegen::Instruction::JmpCC {code, label} => {
            output.push_str(&format!("j{} {}", code.ins_suffix(), label));
        }
        codegen::Instruction::Label(label) => {
            output.push_str(&format!("{}:", label));
        }
        codegen::Instruction::SetCC {code, op} => {
            output.push_str(&format!("set{} {}", code.ins_suffix(), emit_operand(op, &RegWidth::Byte)));
        }
    }
    output
}

fn emit_function(fun_def: &codegen::FunctionDefinition) -> String {
    let mut output = String::new();
    let codegen::FunctionDefinition { name, body } = fun_def;
    let processed_name = if cfg!(target_os = "macos") {
        format!("_{}", name)
    } else { name.to_string() };
    output.push_str(&format!("\t.global {}\n", processed_name));
    output.push_str(&format!("{}:\n", processed_name));
    output.push_str("\tpushq %rbp\n");
    output.push_str("\tmovq %rsp, %rbp\n");
    for ins in body.iter() {
        output.push_str(&format!("\t{}\n", emit_instruction(ins)));
    }
    output
}

pub fn emit_program(program: &codegen::Program) -> String {
    let codegen::Program {function} = program;
    let mut output = emit_function(function);
    if cfg!(target_os = "linux") {
        output.push_str("\n.section .note.GNU-stack,\"\",@progbits\n");
    }
    output
}
