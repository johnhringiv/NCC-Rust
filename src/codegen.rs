use std::collections::HashMap;
use crate::parser;
use crate::parser::Identifier;
use crate::tacky;
use crate::tacky::{BinOp, Val};

#[derive(Clone, Debug, PartialEq)]
pub enum Reg {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(i32),
    Reg(Reg),
    Pseudo(String),
    Stack(i32),
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
    AllocateStack(i32),
    DeallocateStack(i32),
    Push(Operand),
    Call(Identifier),
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
    pub functions: Vec<FunctionDefinition>,
}

fn convert_function_call(fun_name: &Identifier, args: &Vec<Val>, dst: &Val) -> Vec<Instruction> {
    let arg_registers = [ Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9 ];
    let (register_args, stack_args) = args.split_at(args.len().min(6));
    let mut instructions = Vec::new();
    let stack_padding = if stack_args.len() % 2 != 0 {
        instructions.push(Instruction::AllocateStack(8));
        8
    } else { 0 };
    for (tacky_arg, reg) in register_args.iter().zip(arg_registers) {
        let assembly_arg = convert_val(tacky_arg);
        instructions.push(Instruction::Mov { src: assembly_arg, dst: Operand::Reg(reg) });
    }
    for tacky_arg in stack_args.iter().rev() {
        let assembly_arg = convert_val(tacky_arg);
        match assembly_arg {
            Operand::Imm(_) | Operand::Reg(_) => instructions.push(Instruction::Push(assembly_arg)),
            Operand::Stack(_) | Operand::Pseudo(_) => {
                instructions.push(Instruction::Mov {src: assembly_arg, dst: Operand::Reg(Reg::AX) });
                instructions.push(Instruction::Push(Operand::Reg(Reg::AX)))
            }
        }
    }
    instructions.push(Instruction::Call(fun_name.clone()));

    let bytes_to_remove = 8 * stack_args.len() as i32 + stack_padding;
    if bytes_to_remove > 0 {
        instructions.push(Instruction::DeallocateStack(bytes_to_remove));
    }
    let assembly_dst = convert_val(dst);
    instructions.push(Instruction::Mov { src: Operand::Reg(Reg::AX), dst: assembly_dst });

    instructions
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
        },
        tacky::Instruction::FunCall { fun_name, args, dst, } => convert_function_call(&fun_name, &args, &dst)
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
    let tacky::FunctionDefinition { name, params, body } = ast;
    let arg_registers = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
    let mut instructions = vec![];

    for (Identifier(param), reg) in params.iter().zip(arg_registers) {
        instructions.push(Instruction::Mov {src: Operand::Reg(reg), dst: Operand::Pseudo(param.clone())});
    }

    for (i, Identifier(param)) in params.iter().skip(6).enumerate() {
        let stack_offset = 16 + (i as i32 * 8); // +16 for saved RBP and return address
        instructions.push(Instruction::Mov {
            src: Operand::Stack(stack_offset),
            dst: Operand::Pseudo(param.clone()),
        });
    }

    instructions.extend(body.iter().flat_map(convert_instruction));
    {
        FunctionDefinition {
            name: name.to_string(),
            body: instructions,
        }
    }
}

pub fn generate(ast: &tacky::Program) -> Program {
    let mut functions = vec![];
    for function in ast.functions.as_slice() {
        functions.push(convert_function(&function))
    }
    let mut p = Program {
        functions,
    };
    let stack_offsets = replace_pseudo_registers(&mut p);
    fix_invalid(&mut p, &stack_offsets);
    coalesce_labels(&mut p);
    p
}

struct StackMapping {
    stack_mapping: std::collections::HashMap<String, i32>,
    offset: i32,
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

pub fn replace_pseudo_registers(program: &mut Program) -> HashMap<String, i32> {
    let mut offsets = HashMap::new();
    for FunctionDefinition { name, body } in program.functions.iter_mut() {
        let mut stack_mapping = StackMapping::new();

        for ins in body.iter_mut() {
            match ins {
                Instruction::Mov { src, dst } | Instruction::Binary { op: _, src, dst } => {
                    *src = stack_mapping.replace_pseudo(src);
                    *dst = stack_mapping.replace_pseudo(dst);
                }
                Instruction::Unary { op: _, dst } => *dst = stack_mapping.replace_pseudo(dst),
                Instruction::Idiv(src) => {
                    *src = stack_mapping.replace_pseudo(src);
                }
                Instruction::Cmp { v1, v2 } => {
                    *v1 = stack_mapping.replace_pseudo(v1);
                    *v2 = stack_mapping.replace_pseudo(v2);
                }
                Instruction::SetCC { op, .. } | Instruction::Push(op) => {
                    *op = stack_mapping.replace_pseudo(op);
                }
                _ => {}
            }
        }
        offsets.insert(name.to_string(), stack_mapping.offset);
    }
    offsets
}

pub fn fix_invalid(program: &mut Program, stack_offsets: &HashMap<String, i32>) {
    for FunctionDefinition { name, body } in program.functions.iter_mut() {
        // stack_offset is negative, so convert to positive, round up to 16, then negate for AllocateStack
        let mut positive_offset = -stack_offsets[name];
        if positive_offset % 16 != 0{
            positive_offset = ((positive_offset / 16) + 1) * 16;
        }
        let mut new_ins = vec![Instruction::AllocateStack(positive_offset)];
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
}

/// Coalesces consecutive labels by mapping subsequent labels to the first one
pub fn coalesce_labels(program: &mut Program) {
    for FunctionDefinition{name: _name, body} in program.functions.iter_mut() {
        let mut label_map: std::collections::HashMap<String, String> = std::collections::HashMap::new();
        let mut new_ins = Vec::new();
        let mut current_label: Option<String> = None;
        // First pass: build label mapping
        for ins in body.iter() {
            match ins {
                Instruction::Label(Identifier(label)) => {
                    if let Some(first_label) = &current_label {
                        // Map this label to the first label in the sequence
                        // Do not push to new_ins
                        label_map.insert(label.clone(), first_label.clone());
                    } else {
                        // This is the first label in a potential sequence
                        current_label = Some(label.clone());
                        new_ins.push(ins.clone());
                    }
                }
                _ => {
                    // Non-label instruction, reset the sequence
                    current_label = None;
                    new_ins.push(ins.clone());
                }
            }
        }

        // Second pass: update jump targets
        for ins in new_ins.iter_mut() {
            match ins {
                Instruction::Jmp(Identifier(label)) => {
                    if let Some(new_label) = label_map.get(label) {
                        *label = new_label.clone();
                    }
                }
                Instruction::JmpCC {
                    label: Identifier(label),
                    ..
                } => {
                    if let Some(new_label) = label_map.get(label) {
                        *label = new_label.clone();
                    }
                }
                _ => {}
            }
        }
        *body = new_ins;
    }
}
