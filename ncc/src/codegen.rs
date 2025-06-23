use crate::tacky;
use crate::parser;

#[derive(Clone, Debug, PartialEq)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11,
    CX
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(i64),
    Reg(Reg),
    Pseudo(String),
    Stack(i64)
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    BitAnd,
    BitOr,
    BitXOr,
    BitShl,
    BitSar,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Mov {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: UnaryOp,
        dst: Operand,
    },
    Binary {
        op: BinaryOp,
        src: Operand,
        dst: Operand
    },
    Idiv(Operand),
    Cdq,
    AllocateStack(i64),
    Ret
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: FunctionDefinition,
}

fn convert_instruction(instruction: &tacky::Instruction) -> Vec<Instruction> {
    match instruction {
        tacky::Instruction::Return(x) =>   {
            let val = convert_val(&x.clone());
            vec![Instruction::Mov {
                src: val,
                dst: Operand::Reg(Reg::AX),
            }, Instruction::Ret]
        },
        tacky::Instruction::Unary {op, src, dst } => {
            let src = convert_val(src);
            let dst = convert_val(dst);
            let op = convert_unary_op(op);
            vec![Instruction::Mov {src, dst: dst.clone()}, Instruction::Unary {op, dst}]

        },
        tacky::Instruction::Binary {op, src1, src2, dst} => {
            let src1 = convert_val(src1);
            let src2 = convert_val(src2);
            let dst = convert_val(dst);
            match op {
                parser::BinOp::Add | parser::BinOp::Subtract | parser::BinOp::Multiply | parser::BinOp::BitwiseAnd | parser::BinOp::BitwiseOr | parser::BinOp::BitwiseXOr | parser::BinOp::BitwiseLeftShift | parser::BinOp::BitwiseRightShift => {
                    vec![Instruction::Mov {src: src1, dst: dst.clone()}, Instruction::Binary {op: convert_binary_op(op), src: src2, dst}]
                }
                parser::BinOp::Divide => {
                    vec![Instruction::Mov { src: src1, dst: Operand::Reg(Reg::AX) },
                         Instruction::Cdq,
                         Instruction::Idiv(src2),
                         Instruction::Mov { src: Operand::Reg(Reg::AX), dst }]
                }
                parser::BinOp::Remainder => {
                    vec![Instruction::Mov { src: src1, dst: Operand::Reg(Reg::AX) },
                         Instruction::Cdq,
                         Instruction::Idiv(src2),
                         Instruction::Mov { src: Operand::Reg(Reg::DX), dst }]
                }
            }

        }
    }
}

fn convert_binary_op(op: &parser::BinOp) -> BinaryOp {
    match op {
        parser::BinOp::Add => BinaryOp::Add,
        parser::BinOp::Subtract => BinaryOp::Sub,
        parser::BinOp::Multiply => BinaryOp::Mult,
        parser::BinOp::Divide | parser::BinOp::Remainder => unreachable!("Special case for division and remainder should be handled in the instruction conversion"),
        parser::BinOp::BitwiseAnd => BinaryOp::BitAnd,
        parser::BinOp::BitwiseOr => BinaryOp::BitOr,
        parser::BinOp::BitwiseXOr => BinaryOp::BitXOr,
        parser::BinOp::BitwiseLeftShift => BinaryOp::BitShl,
        parser::BinOp::BitwiseRightShift => BinaryOp::BitSar
    }
}

fn convert_unary_op(op: &parser::UnaryOp) -> UnaryOp {
    match op {
        parser::UnaryOp::Negate => UnaryOp::Neg,
        parser::UnaryOp::BitwiseComplement => UnaryOp::Not,
    }
}

fn convert_val(ast: &tacky::Val) -> Operand {
    match ast {
        tacky::Val::Constant(value) => Operand::Imm(*value),
        tacky::Val::Var(s) => Operand::Pseudo(s.clone()),
    }
}

fn convert_function(ast: &tacky::FunctionDefinition) -> FunctionDefinition {
    let tacky::FunctionDefinition { name, body } = ast;
    {
        FunctionDefinition {
            name: name.to_string(),
            body: body.iter().flat_map(convert_instruction).collect(),
        }
        
    }
}

pub fn generate(ast: &tacky::Program) -> Program {
    let tacky::Program{function} = ast;
    let mut p = Program {
        function: convert_function(function),
    };
    let stack_offset = replace_pseudo_registers(&mut p);
    fix_invalid(&mut p, stack_offset);
    p
}

struct StackMapping {
    stack_mapping: std::collections::HashMap<String, i64>,
    offset: i64,
}

impl StackMapping {
    pub fn new() -> Self {
        StackMapping {
            stack_mapping: std::collections::HashMap::new(),
            offset: 0
        }
    }
    
    pub fn get_stack_location(&mut self, pseudo: &str) -> Operand {
        let offset_option = self.stack_mapping.get(pseudo);
        let offset = match offset_option {
            Some(offset) => offset,
            None => { 
                self.offset -= 4;
                self.stack_mapping.insert(pseudo.to_string(), self.offset);
                &self.offset
            }
        };
        Operand::Stack(*offset)
    }
    
    pub fn replace_pseudo(&mut self, operand: &Operand) -> Operand {
        match operand {
            Operand::Pseudo(pseudo) => self.get_stack_location(pseudo),
            _ => operand.clone()
        }
    }
}

pub fn replace_pseudo_registers(program: &mut Program) -> i64 {
    let Program {function : FunctionDefinition { name: _, body}} = program;
    let mut stack_mapping = StackMapping::new();
    
    for ins in body.iter_mut(){
        match ins {
            Instruction::Mov { src, dst } => {
                *src = stack_mapping.replace_pseudo(src);
                *dst = stack_mapping.replace_pseudo(dst);
            }
            Instruction::Unary {op: _, dst } => {
                *dst = stack_mapping.replace_pseudo(dst)
            }
            Instruction::Binary {op: _, src, dst} => {
                *src = stack_mapping.replace_pseudo(src);
                *dst = stack_mapping.replace_pseudo(dst);
            }
            Instruction::Idiv(src) => {*src = stack_mapping.replace_pseudo(src);}
            _ => {}
        }
    }
    stack_mapping.offset
}

pub fn fix_invalid(program: &mut Program, stack_offset: i64) {
    let Program {function : FunctionDefinition { name: _, body}} = program;
    let mut new_ins = vec![Instruction::AllocateStack(stack_offset)];
    for ins in body.iter() {
        match ins {
            Instruction::Mov {src: Operand::Stack(src), dst: Operand::Stack(dst) } => {
                new_ins.push(Instruction::Mov { src: Operand::Stack(*src), dst: Operand::Reg(Reg::R10), });
                new_ins.push(Instruction::Mov { src: Operand::Reg(Reg::R10), dst: Operand::Stack(*dst), });
            }
            Instruction::Idiv(Operand::Imm(c)) => {
                new_ins.push(Instruction::Mov {src: Operand::Imm(*c), dst: Operand::Reg(Reg::R10), });
                new_ins.push(Instruction::Idiv(Operand::Reg(Reg::R10)));
            }
            Instruction::Binary {op: BinaryOp::Mult, src, dst: Operand::Stack(dst)} => {
                new_ins.push(Instruction::Mov { src: Operand::Stack(*dst), dst: Operand::Reg(Reg::R11) });
                new_ins.push(Instruction::Binary { op: BinaryOp::Mult, src: src.clone(), dst: Operand::Reg(Reg::R11)});
                new_ins.push(Instruction::Mov {src: Operand::Reg(Reg::R11), dst: Operand::Stack(*dst) });
            }
            Instruction::Binary {op: op @ (BinaryOp::Add | BinaryOp::Sub | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXOr), src: Operand::Stack(src), dst: Operand::Stack(dst)} => { 
                new_ins.push(Instruction::Mov { src: Operand::Stack(*src), dst: Operand::Reg(Reg::R10) });
                new_ins.push(Instruction::Binary { op: op.clone(), src: Operand::Reg(Reg::R10), dst: Operand::Stack(*dst) }); 
            }
            Instruction::Binary {op: op @ (BinaryOp::BitShl | BinaryOp::BitSar), src: Operand::Stack(src), dst} => {
                new_ins.push(Instruction::Mov { src: Operand::Stack(*src), dst: Operand::Reg(Reg::CX) });
                new_ins.push(Instruction::Binary { op: op.clone(), src: Operand::Reg(Reg::CX), dst: dst.clone() });
            }
            _ => {new_ins.push(ins.clone())}
        }
    }
    *body = new_ins;
}

#[cfg(test)]
mod tests {
    use crate::lexer::{tokenizer};
    use crate::parser::parse_program;
    use super::*;
    use crate::tacky;

    #[test]
    fn basic_return() {
        let input = std::fs::read_to_string("../writing-a-c-compiler-tests/tests/chapter_1/valid/multi_digit.c").expect("Failed to read input file");
        let mut tokens = tokenizer(&input).unwrap();
        let ast = parse_program(&mut tokens).unwrap();
        let tacky = tacky::tackify_program(&ast);
        
        let expected = Program {function : FunctionDefinition { name: "main".to_string(), body: vec![Instruction::AllocateStack(0), Instruction::Mov {src: Operand::Imm(100), dst: Operand::Reg(Reg::AX)}] }};
        assert_eq!(generate(&tacky), expected);
    }
}