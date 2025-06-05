use crate::codegen::Operand::Pseudo;
use crate::tacky;

#[derive(Clone, Debug, PartialEq)]
pub enum Reg {
    AX,
    R10
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

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: UnaryOp,
        dst: Operand,
    },
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

        }
    }
}

fn convert_unary_op(op: &tacky::UnaryOp) -> UnaryOp {
    match op {
        tacky::UnaryOp::Negate => UnaryOp::Neg,
        tacky::UnaryOp::Complement => UnaryOp::Not,
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
            Pseudo(pseudo) => self.get_stack_location(pseudo),
            _ => operand.clone()
        }
    }
}

pub fn replace_pseudo_registers(program: &mut Program) -> i64 {
    let Program {function : FunctionDefinition { name, body}} = program;
    let mut stack_mapping = StackMapping::new();
    
    for ins in body.iter_mut(){
        match ins {
            Instruction::Mov { src, dst } => {
                *ins = Instruction::Mov {
                    src: stack_mapping.replace_pseudo(src),
                    dst: stack_mapping.replace_pseudo(dst),
                };
                
                
            }
            Instruction::Unary {op, dst } => {
                *ins = Instruction::Unary {
                    op: op.clone(),
                    dst: stack_mapping.replace_pseudo(dst),
                };
            }
            _ => {}
        }
    }
    stack_mapping.offset
}

pub fn fix_invalid(program: &mut Program, stack_offset: i64) {
    let Program {function : FunctionDefinition { name, body}} = program;

    // Collect indices of invalid instructions
    let mut to_fix = Vec::new();
    for (idx, ins) in body.iter().enumerate() {
        if let Instruction::Mov { src: Operand::Stack(_), dst: Operand::Stack(_) } = ins {
            to_fix.push(idx);
        }
    }
    
    let mut offset = 0;
    for idx in to_fix {
        if let Instruction::Mov { src: Operand::Stack(src), dst: Operand::Stack(dst) } = &body[idx + offset] {
            let tmp_mov = Instruction::Mov {
                src: Operand::Stack(*src),
                dst: Operand::Reg(Reg::R10),
            };
            let mov = Instruction::Mov {
                src: Operand::Reg(Reg::R10),
                dst: Operand::Stack(*dst),
            };
            // Replace the original instruction and insert the new one after
            body.splice(idx + offset..=idx + offset, [tmp_mov, mov]);
            offset += 1;
        }
    }
    
    body.insert(0, Instruction::AllocateStack(stack_offset));
    
    //this doesn't compile because of the borrow checker
    // for (idx, ins) in body.iter_mut().enumerate() {
    //     match ins {
    //         Instruction::Mov {src : Operand::Stack(src), dst : Operand::Stack(dst)} => {
    //             let tmp_mov = Instruction::Mov {
    //                 src: Operand::Stack(*src),
    //                 dst: Operand::Reg(Reg::R10), // Using a temporary register
    //             };
    //             let mov = Instruction::Mov {
    //                 src: Operand::Reg(Reg::R10),
    //                 dst: Operand::Stack(*dst),
    //             };
    //             body.splice(idx..idx, [tmp_mov, mov]);
    //         }
    //         _ => {}
    //     }
    // }
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