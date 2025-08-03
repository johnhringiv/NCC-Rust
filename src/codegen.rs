use crate::parser;
use crate::parser::Identifier;
use crate::pretty::{ItfDisplay, Node, cyan, simple_node};
use crate::tacky;
use crate::tacky::BinOp;

#[derive(Clone, Debug, PartialEq)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11,
    CX,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(i64),
    Reg(Reg),
    Pseudo(String),
    Stack(i64),
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
    Mov { src: Operand, dst: Operand },
    Unary { op: UnaryOp, dst: Operand },
    Binary { op: BinaryOp, src: Operand, dst: Operand },
    Cmp { v1: Operand, v2: Operand },
    Idiv(Operand),
    Cdq,
    Jmp(Identifier),
    JmpCC { code: CondCode, label: Identifier },
    SetCC { code: CondCode, op: Operand },
    Label(Identifier),
    AllocateStack(i64),
    Ret,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

impl CondCode {
    pub fn ins_suffix(&self) -> String {
        match self {
            CondCode::E => "e".to_string(),
            CondCode::NE => "ne".to_string(),
            CondCode::G => "g".to_string(),
            CondCode::GE => "ge".to_string(),
            CondCode::L => "l".to_string(),
            CondCode::LE => "le".to_string(),
        }
    }
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
        tacky::Instruction::Return(x) => {
            let val = convert_val(&x.clone());
            vec![
                Instruction::Mov {
                    src: val,
                    dst: Operand::Reg(Reg::AX),
                },
                Instruction::Ret,
            ]
        }
        tacky::Instruction::Unary {
            op: parser::UnaryOp::Not,
            src,
            dst,
        } => {
            let src = convert_val(src);
            let dst = convert_val(dst);
            vec![
                Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: src,
                },
                Instruction::Mov {
                    src: Operand::Imm(0),
                    dst: dst.clone(),
                },
                Instruction::SetCC {
                    code: CondCode::E,
                    op: dst,
                },
            ]
        }
        tacky::Instruction::Unary { op, src, dst } => {
            let src = convert_val(src);
            let dst = convert_val(dst);
            let op = convert_unary_op(op);
            vec![
                Instruction::Mov { src, dst: dst.clone() },
                Instruction::Unary { op, dst },
            ]
        }
        tacky::Instruction::Binary { op, src1, src2, dst } => {
            let src1 = convert_val(src1);
            let src2 = convert_val(src2);
            let dst = convert_val(dst);
            match op {
                BinOp::Add
                | BinOp::Subtract
                | BinOp::Multiply
                | BinOp::BitwiseAnd
                | BinOp::BitwiseOr
                | BinOp::BitwiseXOr
                | BinOp::BitwiseLeftShift
                | BinOp::BitwiseRightShift => {
                    vec![
                        Instruction::Mov {
                            src: src1,
                            dst: dst.clone(),
                        },
                        Instruction::Binary {
                            op: BinaryOp::from(op),
                            src: src2,
                            dst,
                        },
                    ]
                }
                BinOp::Divide => {
                    vec![
                        Instruction::Mov {
                            src: src1,
                            dst: Operand::Reg(Reg::AX),
                        },
                        Instruction::Cdq,
                        Instruction::Idiv(src2),
                        Instruction::Mov {
                            src: Operand::Reg(Reg::AX),
                            dst,
                        },
                    ]
                }
                BinOp::Remainder => {
                    vec![
                        Instruction::Mov {
                            src: src1,
                            dst: Operand::Reg(Reg::AX),
                        },
                        Instruction::Cdq,
                        Instruction::Idiv(src2),
                        Instruction::Mov {
                            src: Operand::Reg(Reg::DX),
                            dst,
                        },
                    ]
                }
                BinOp::Equal
                | BinOp::NotEqual
                | BinOp::LessThan
                | BinOp::LessOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterOrEqual => {
                    let code = match op {
                        BinOp::Equal => CondCode::E,
                        BinOp::NotEqual => CondCode::NE,
                        BinOp::LessThan => CondCode::L,
                        BinOp::LessOrEqual => CondCode::LE,
                        BinOp::GreaterThan => CondCode::G,
                        BinOp::GreaterOrEqual => CondCode::GE,
                        _ => unreachable!(),
                    };
                    vec![
                        Instruction::Cmp { v1: src2, v2: src1 },
                        Instruction::Mov {
                            src: Operand::Imm(0),
                            dst: dst.clone(),
                        },
                        Instruction::SetCC { code, op: dst },
                    ]
                }
            }
        }
        tacky::Instruction::JumpIfZero { condition, target } => {
            vec![
                Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: convert_val(condition),
                },
                Instruction::JmpCC {
                    code: CondCode::E,
                    label: target.clone(),
                },
            ]
        }
        tacky::Instruction::JumpIfNotZero { condition, target } => {
            vec![
                Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: convert_val(condition),
                },
                Instruction::JmpCC {
                    code: CondCode::NE,
                    label: target.clone(),
                },
            ]
        }
        tacky::Instruction::Jump { target } => {
            vec![Instruction::Jmp(target.clone())]
        }
        tacky::Instruction::Label(label) => {
            vec![Instruction::Label(label.clone())]
        }
        tacky::Instruction::Copy { src, dst } => {
            let src = convert_val(src);
            let dst = convert_val(dst);
            vec![Instruction::Mov { src, dst }]
        }
    }
}

impl From<&BinOp> for BinaryOp {
    fn from(op: &BinOp) -> Self {
        match op {
            BinOp::Add => BinaryOp::Add,
            BinOp::Subtract => BinaryOp::Sub,
            BinOp::Multiply => BinaryOp::Mult,
            BinOp::Divide | BinOp::Remainder => {
                unreachable!("Special case for division and remainder should be handled in the instruction conversion")
            }
            BinOp::BitwiseAnd => BinaryOp::BitAnd,
            BinOp::BitwiseOr => BinaryOp::BitOr,
            BinOp::BitwiseXOr => BinaryOp::BitXOr,
            BinOp::BitwiseLeftShift => BinaryOp::BitShl,
            BinOp::BitwiseRightShift => BinaryOp::BitSar,
            _ => unimplemented!("Binary operation not implemented: {:?}", op),
        }
    }
}

fn convert_unary_op(op: &parser::UnaryOp) -> UnaryOp {
    match op {
        parser::UnaryOp::Negate => UnaryOp::Neg,
        parser::UnaryOp::BitwiseComplement => UnaryOp::Not,
        _ => unreachable!("Unary operation not implemented: {:?}", op),
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
    let tacky::Program { function } = ast;
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
            offset: 0,
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
            _ => operand.clone(),
        }
    }
}

pub fn replace_pseudo_registers(program: &mut Program) -> i64 {
    let Program {
        function: FunctionDefinition { name: _, body },
    } = program;
    let mut stack_mapping = StackMapping::new();

    for ins in body.iter_mut() {
        match ins {
            Instruction::Mov { src, dst } => {
                *src = stack_mapping.replace_pseudo(src);
                *dst = stack_mapping.replace_pseudo(dst);
            }
            Instruction::Unary { op: _, dst } => *dst = stack_mapping.replace_pseudo(dst),
            Instruction::Binary { op: _, src, dst } => {
                *src = stack_mapping.replace_pseudo(src);
                *dst = stack_mapping.replace_pseudo(dst);
            }
            Instruction::Idiv(src) => {
                *src = stack_mapping.replace_pseudo(src);
            }
            Instruction::Cmp { v1, v2 } => {
                *v1 = stack_mapping.replace_pseudo(v1);
                *v2 = stack_mapping.replace_pseudo(v2);
            }
            Instruction::SetCC { op, .. } => {
                *op = stack_mapping.replace_pseudo(op);
            }
            _ => {}
        }
    }
    stack_mapping.offset
}

pub fn fix_invalid(program: &mut Program, stack_offset: i64) {
    let Program {
        function: FunctionDefinition { name: _, body },
    } = program;
    let mut new_ins = vec![Instruction::AllocateStack(stack_offset)];
    for ins in body.iter() {
        match ins {
            Instruction::Mov {
                src: Operand::Stack(src),
                dst: Operand::Stack(dst),
            } => {
                new_ins.push(Instruction::Mov {
                    src: Operand::Stack(*src),
                    dst: Operand::Reg(Reg::R10),
                });
                new_ins.push(Instruction::Mov {
                    src: Operand::Reg(Reg::R10),
                    dst: Operand::Stack(*dst),
                });
            }
            Instruction::Idiv(Operand::Imm(c)) => {
                new_ins.push(Instruction::Mov {
                    src: Operand::Imm(*c),
                    dst: Operand::Reg(Reg::R10),
                });
                new_ins.push(Instruction::Idiv(Operand::Reg(Reg::R10)));
            }
            Instruction::Binary {
                op: BinaryOp::Mult,
                src,
                dst: Operand::Stack(dst),
            } => {
                new_ins.push(Instruction::Mov {
                    src: Operand::Stack(*dst),
                    dst: Operand::Reg(Reg::R11),
                });
                new_ins.push(Instruction::Binary {
                    op: BinaryOp::Mult,
                    src: src.clone(),
                    dst: Operand::Reg(Reg::R11),
                });
                new_ins.push(Instruction::Mov {
                    src: Operand::Reg(Reg::R11),
                    dst: Operand::Stack(*dst),
                });
            }
            Instruction::Binary {
                op: op @ (BinaryOp::Add | BinaryOp::Sub | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXOr),
                src: Operand::Stack(src),
                dst: Operand::Stack(dst),
            } => {
                new_ins.push(Instruction::Mov {
                    src: Operand::Stack(*src),
                    dst: Operand::Reg(Reg::R10),
                });
                new_ins.push(Instruction::Binary {
                    op: op.clone(),
                    src: Operand::Reg(Reg::R10),
                    dst: Operand::Stack(*dst),
                });
            }
            Instruction::Binary {
                op: op @ (BinaryOp::BitShl | BinaryOp::BitSar),
                src: Operand::Stack(src),
                dst,
            } => {
                new_ins.push(Instruction::Mov {
                    src: Operand::Stack(*src),
                    dst: Operand::Reg(Reg::CX),
                });
                new_ins.push(Instruction::Binary {
                    op: op.clone(),
                    src: Operand::Reg(Reg::CX),
                    dst: dst.clone(),
                });
            }
            Instruction::Cmp {
                v1: Operand::Stack(v1),
                v2: Operand::Stack(v2),
            } => {
                new_ins.push(Instruction::Mov {
                    src: Operand::Stack(*v1),
                    dst: Operand::Reg(Reg::R10),
                });
                new_ins.push(Instruction::Cmp {
                    v1: Operand::Reg(Reg::R10),
                    v2: Operand::Stack(*v2),
                });
            }
            Instruction::Cmp {
                v1,
                v2: Operand::Imm(c),
            } => {
                new_ins.push(Instruction::Mov {
                    src: Operand::Imm(*c),
                    dst: Operand::Reg(Reg::R11),
                });
                new_ins.push(Instruction::Cmp {
                    v1: v1.clone(),
                    v2: Operand::Reg(Reg::R11),
                });
            }
            _ => new_ins.push(ins.clone()),
        }
    }
    *body = new_ins;
}

// Implementing ItfDisplay for the enums and structs
simple_node!(Reg);
simple_node!(UnaryOp);
simple_node!(BinaryOp);
simple_node!(CondCode);

impl ItfDisplay for Operand {
    fn itf_node(&self) -> Node {
        match self {
            Operand::Imm(i) => Node::branch(cyan("Imm"), vec![i.itf_node()]),
            Operand::Reg(r) => Node::branch(cyan("Reg"), vec![r.itf_node()]),
            Operand::Pseudo(s) => Node::branch(cyan("Pseudo"), vec![s.itf_node()]),
            Operand::Stack(o) => Node::branch(cyan("Stack"), vec![o.itf_node()]),
        }
    }
}

impl ItfDisplay for Instruction {
    fn itf_node(&self) -> Node {
        match self {
            Instruction::Mov { src, dst } => Node::branch(cyan("Mov"), vec![src.itf_node(), dst.itf_node()]),
            Instruction::Unary { op, dst } => Node::branch(cyan("Unary"), vec![op.itf_node(), dst.itf_node()]),
            Instruction::Binary { op, src, dst } => {
                Node::branch(cyan("Binary"), vec![op.itf_node(), src.itf_node(), dst.itf_node()])
            }
            Instruction::Cmp { v1, v2 } => Node::branch(cyan("Cmp"), vec![v1.itf_node(), v2.itf_node()]),
            Instruction::Idiv(op) => Node::branch(cyan("Idiv"), vec![op.itf_node()]),
            Instruction::Cdq => Node::leaf(cyan("Cdq")),
            Instruction::Jmp(label) => Node::branch(cyan("Jmp"), vec![label.itf_node()]),
            Instruction::JmpCC { code, label } => Node::branch(cyan("JmpCC"), vec![code.itf_node(), label.itf_node()]),
            Instruction::SetCC { code, op } => Node::branch(cyan("SetCC"), vec![code.itf_node(), op.itf_node()]),
            Instruction::Label(l) => Node::branch(cyan("Label"), vec![l.itf_node()]),
            Instruction::AllocateStack(off) => Node::branch(cyan("AllocateStack"), vec![off.itf_node()]),
            Instruction::Ret => Node::leaf(cyan("Ret")),
        }
    }
}

impl ItfDisplay for FunctionDefinition {
    fn itf_node(&self) -> Node {
        let name_line = Node::leaf(format!("name: {}", self.name.itf_node().text));
        let body_children: Vec<Node> = self.body.iter().map(|i| i.itf_node()).collect();
        let body_node = Node::branch(cyan("body"), body_children);
        Node::branch(cyan("FunctionDefinition"), vec![name_line, body_node])
    }
}

impl ItfDisplay for Program {
    fn itf_node(&self) -> Node {
        Node::branch(cyan("Program"), vec![self.function.itf_node()])
    }
}
