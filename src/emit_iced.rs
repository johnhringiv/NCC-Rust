// Simple emitter using iced-x86 to generate machine code at runtime
use iced_x86::code_asm::*;
use iced_x86::code_asm::registers::{self, gpr64, gpr8};
use iced_x86::IcedError;
use crate::codegen::{self, BinaryOp, CondCode, Instruction, Operand, Reg, UnaryOp};
use std::collections::HashMap;

pub fn emit_program(program: &codegen::Program) -> Result<Vec<u8>, IcedError> {
    let mut a = CodeAssembler::new(64)?;
    emit_function(&mut a, &program.function)?;
    a.assemble(0)
}

fn emit_function(a: &mut CodeAssembler, fun_def: &codegen::FunctionDefinition) -> Result<(), IcedError> {
    let mut labels: HashMap<String, CodeLabel> = HashMap::new();
    a.push(gpr64::rbp)?;
    a.mov(gpr64::rbp, gpr64::rsp)?;
    for ins in &fun_def.body {
        emit_instruction(a, ins, &mut labels)?;
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
    }
}

fn mem_rbp(offset: i64) -> AsmMemoryOperand {
    if offset >= 0 {
        dword_ptr(gpr64::rbp + offset)
    } else {
        dword_ptr(gpr64::rbp - (-offset))
    }
}

fn emit_instruction(a: &mut CodeAssembler, ins: &Instruction, labels: &mut HashMap<String, CodeLabel>) -> Result<(), IcedError> {
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
        Instruction::Binary { op, src, dst } => {
            match op {
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
                    (Operand::Reg(s), Operand::Stack(off)) => {
                        a.mov(registers::gpr32::r10d, mem_rbp(*off))?;
                        a.imul_2(registers::gpr32::r10d, gpr32(s))?;
                        a.mov(mem_rbp(*off), registers::gpr32::r10d)?;
                    }
                    _ => unimplemented!(),
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
                    (Operand::Reg(_s), Operand::Reg(d)) => a.shl(gpr32(d), gpr8::cl)?,
                    (Operand::Imm(v), Operand::Stack(off)) => a.shl(mem_rbp(*off), *v as i32)?,
                    (Operand::Reg(s), Operand::Stack(off)) => {
                        a.mov(registers::gpr32::ecx, gpr32(s))?;
                        a.shl(mem_rbp(*off), gpr8::cl)?;
                    }
                    _ => unimplemented!(),
                },
                BinaryOp::BitSar => match (src, dst) {
                    (Operand::Imm(v), Operand::Reg(d)) => a.sar(gpr32(d), *v as i32)?,
                    (Operand::Reg(_s), Operand::Reg(d)) => a.sar(gpr32(d), gpr8::cl)?,
                    (Operand::Imm(v), Operand::Stack(off)) => a.sar(mem_rbp(*off), *v as i32)?,
                    (Operand::Reg(s), Operand::Stack(off)) => {
                        a.mov(registers::gpr32::ecx, gpr32(s))?;
                        a.sar(mem_rbp(*off), gpr8::cl)?;
                    }
                    _ => unimplemented!(),
                },
            }
        }
        Instruction::Cmp { v1, v2 } => {
            match (v1, v2) {
                (Operand::Reg(r1), Operand::Reg(r2)) => a.cmp(gpr32(r2), gpr32(r1))?,
                (Operand::Reg(r1), Operand::Imm(v)) => a.cmp(gpr32(r1), *v as i32)?,
                (Operand::Reg(r1), Operand::Stack(off)) => a.cmp(gpr32(r1), mem_rbp(*off))?,
                (Operand::Stack(off), Operand::Reg(r)) => a.cmp(mem_rbp(*off), gpr32(r))?,
                (Operand::Stack(o1), Operand::Imm(v)) => a.cmp(mem_rbp(*o1), *v as i32)?,
                (Operand::Imm(v), Operand::Reg(r)) => a.cmp(gpr32(r), *v as i32)?,
                (Operand::Imm(v), Operand::Stack(off)) => a.cmp(mem_rbp(*off), *v as i32)?,
                _ => unimplemented!(),
            }
        }
        Instruction::Cdq => { a.cdq()?; }
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
            Operand::Stack(off) => {
                match code {
                    CondCode::E => a.sete(mem_rbp(*off))?,
                    CondCode::NE => a.setne(mem_rbp(*off))?,
                    CondCode::G => a.setg(mem_rbp(*off))?,
                    CondCode::GE => a.setge(mem_rbp(*off))?,
                    CondCode::L => a.setl(mem_rbp(*off))?,
                    CondCode::LE => a.setle(mem_rbp(*off))?,
                }
            }
            _ => unimplemented!(),
        },
        Instruction::Label(lbl) => {
            let l = labels.entry(lbl.0.clone()).or_insert_with(|| a.create_label());
            a.set_label(l)?;
        }
        Instruction::AllocateStack(off) => {
            if *off != 0 { a.sub(gpr64::rsp, -*off as i32)?; }
        }
        Instruction::Ret => {
            a.mov(gpr64::rsp, gpr64::rbp)?;
            a.pop(gpr64::rbp)?;
            a.ret()?;
        }
    }
    Ok(())
}
