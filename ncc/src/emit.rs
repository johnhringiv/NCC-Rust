use crate::codegen;

pub fn emit_reg(reg: &codegen::Reg) -> String {
    match reg {
        codegen::Reg::AX => "eax".to_string(),
        codegen::Reg::R10 => "r10d".to_string(),
        codegen::Reg::R11 => "r11d".to_string(),
        codegen::Reg::DX => "edx".to_string(),
    }
}

pub fn emit_unaryop(op: &codegen::UnaryOp) -> String {
    match op {
        codegen::UnaryOp::Neg => "negl".to_string(),
        codegen::UnaryOp::Not => "notl".to_string(),
    }
}

pub fn emit_binaryop(op: &codegen::BinaryOp) -> String {
    match op {
        codegen::BinaryOp::Add => "addl".to_string(),
        codegen::BinaryOp::Sub => "subl".to_string(),
        codegen::BinaryOp::Mult => "imull".to_string()
    }
}

pub fn emit_operand(operand: &codegen::Operand) -> String {
    let mut output = String::new();
    match operand {
        codegen::Operand::Imm(value) => {
            output.push_str(&format!("${}", value));
        },
        codegen::Operand::Reg(reg) => {
            output.push_str(&format!("%{}", emit_reg(&reg)))
        }
        codegen::Operand::Stack(offset) => {
            output.push_str(&format!("{}(%rbp)", offset));
        },
        &codegen::Operand::Pseudo(_) => unreachable!("Must be eliminated before emission"),
    }
    output
}

pub fn emit_instruction(ins: &codegen::Instruction) -> String {
    let mut output = String::new();
    match ins {
        codegen::Instruction::Mov { src, dst } => {
            output.push_str(&format!("movl {}, {}", emit_operand(src), emit_operand(dst)));
        }
        codegen::Instruction::Ret => {
            output.push_str("movq %rbp, %rsp\n");
            output.push_str("\tpopq %rbp\n");
            output.push_str("\tret");
        }
        codegen::Instruction::Unary { op, dst } => {
            output.push_str(&format!("{} {}\n", emit_unaryop(op), emit_operand(dst)));
        }
        codegen::Instruction::AllocateStack(offset) => {
            output.push_str(&format!("subq ${}, %rsp", offset * -1));
        }
        codegen::Instruction::Binary { op, src, dst} => {
            output.push_str(&format!("{} {}, {}", emit_binaryop(op), emit_operand(src), emit_operand(dst)));
        }
        codegen::Instruction::Idiv(op) => {
            output.push_str(&format!("idivl {} ", emit_operand(op)));
        }
        codegen::Instruction::Cdq => {output.push_str("cdq");}
    }
    output
}

pub fn emit_function(fun_def: &codegen::FunctionDefinition) -> String {
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