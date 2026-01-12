use crate::parser;
use crate::parser::Identifier;
use crate::tacky;
use crate::tacky::{BinOp, StaticVariable, Val};
use std::collections::{HashMap, HashSet};

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
    Data(String),
}

impl Operand {
    pub fn is_memory(&self) -> bool {
        matches!(self, Operand::Data(_) | Operand::Stack(_))
    }
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
    pub fn ins_suffix(&self) -> &'static str {
        match self {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
    pub static_vars: Vec<StaticVariable>,
}

fn convert_function_call(fun_name: &Identifier, args: &[Val], dst: &Val) -> Vec<Instruction> {
    let arg_registers = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
    let (register_args, stack_args) = args.split_at(args.len().min(6));
    let mut instructions = Vec::new();
    let stack_padding = if stack_args.len() % 2 != 0 {
        instructions.push(Instruction::AllocateStack(8));
        8
    } else {
        0
    };
    for (tacky_arg, reg) in register_args.iter().zip(arg_registers) {
        let assembly_arg = convert_val(tacky_arg);
        instructions.push(Instruction::Mov {
            src: assembly_arg,
            dst: Operand::Reg(reg),
        });
    }
    for tacky_arg in stack_args.iter().rev() {
        let assembly_arg = convert_val(tacky_arg);
        match assembly_arg {
            Operand::Imm(_) | Operand::Reg(_) => instructions.push(Instruction::Push(assembly_arg)),
            Operand::Stack(_) | Operand::Pseudo(_) | Operand::Data(_) => {
                instructions.push(Instruction::Mov {
                    src: assembly_arg,
                    dst: Operand::Reg(Reg::AX),
                });
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
    instructions.push(Instruction::Mov {
        src: Operand::Reg(Reg::AX),
        dst: assembly_dst,
    });

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
                BinOp::Divide | BinOp::Remainder => {
                    let result_reg = if *op == BinOp::Divide { Reg::AX } else { Reg::DX };
                    vec![
                        Instruction::Mov {
                            src: src1,
                            dst: Operand::Reg(Reg::AX),
                        },
                        Instruction::Cdq,
                        Instruction::Idiv(src2),
                        Instruction::Mov {
                            src: Operand::Reg(result_reg),
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
        tacky::Instruction::FunCall { fun_name, args, dst } => convert_function_call(fun_name, args, dst),
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
            BinOp::Equal
            | BinOp::NotEqual
            | BinOp::LessThan
            | BinOp::LessOrEqual
            | BinOp::GreaterThan
            | BinOp::GreaterOrEqual => unreachable!("Removed in convert_instruction"),
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
    let tacky::FunctionDefinition {
        name,
        params,
        body,
        global,
    } = ast;
    let arg_registers = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
    let mut instructions = vec![];

    for (Identifier(param), reg) in params.iter().zip(arg_registers) {
        instructions.push(Instruction::Mov {
            src: Operand::Reg(reg),
            dst: Operand::Pseudo(param.clone()),
        });
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
            global: *global,
        }
    }
}

/// Converts TACKY IR to x86-64 assembly AST.
///
/// Performs several passes: converts TACKY to assembly instructions,
/// replaces pseudo-registers with stack slots (or Data operands for statics),
/// fixes invalid instruction operand combinations, and coalesces consecutive labels.
pub fn generate(ast: &tacky::Program) -> Program {
    let mut functions = vec![];
    let mut static_vars = vec![];

    for decl in &ast.top_level {
        match decl {
            tacky::TopLevel::Function(function) => functions.push(convert_function(function)),
            tacky::TopLevel::StaticVariable(s) => static_vars.push(s.clone()),
        }
    }
    let mut p = Program { functions, static_vars };
    let stack_offsets = replace_pseudo_registers(&mut p);
    fix_invalid(&mut p, &stack_offsets);
    coalesce_labels(&mut p);
    p
}

struct StackMapping<'a> {
    stack_mapping: HashMap<String, i32>,
    offset: i32,
    /// Names of variables that should use Data operands (RIP-relative addressing).
    /// Includes both static variables defined in this file and extern variables.
    data_vars: &'a HashSet<String>,
}

impl<'a> StackMapping<'a> {
    fn from_data_vars(data_vars: &'a HashSet<String>) -> Self {
        StackMapping {
            stack_mapping: HashMap::new(),
            offset: 0,
            data_vars,
        }
    }

    fn get_stack_location(&mut self, pseudo: &str) -> Operand {
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

    /// Replaces pseudo-register operands with their actual locations.
    ///
    /// Static/extern variables become `Data` operands (RIP-relative addressing).
    /// Local variables become `Stack` operands (RBP-relative addressing).
    fn replace_pseudo(&mut self, operand: &Operand) -> Operand {
        match operand {
            Operand::Pseudo(pseudo) => {
                if self.data_vars.contains(pseudo) {
                    Operand::Data(pseudo.clone())
                } else {
                    self.get_stack_location(pseudo)
                }
            }
            _ => operand.clone(),
        }
    }
}

/// Replaces all pseudo-register operands with concrete locations.
///
/// For each function, assigns stack slots to local variables and converts
/// static variable references to Data operands. Returns a map of function
/// names to their total stack space used (as negative offsets from RBP).
pub fn replace_pseudo_registers(program: &mut Program) -> HashMap<String, i32> {
    // Collect data vars (static + extern) upfront to avoid borrow conflict in the loop
    let data_vars: HashSet<String> = program.static_vars.iter().map(|v| v.name.clone()).collect();

    let mut offsets = HashMap::new();
    for FunctionDefinition { name, body, global: _ } in program.functions.iter_mut() {
        let mut stack_mapping = StackMapping::from_data_vars(&data_vars);

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

/// Fixes invalid x86-64 instruction operand combinations.
///
/// Rewrites instructions that violate x86-64 encoding rules:
/// - Memory-to-memory moves (uses R10 as intermediate)
/// - Immediate operand to idiv (moves to R10 first)
/// - imul with memory destination (uses R11 as intermediate)
/// - Binary ops with both operands in memory (uses R10)
/// - Shift with memory source (moves count to CX)
/// - Compare with both operands in memory or immediate second operand
///
/// Also inserts stack allocation at the start of each function.
pub fn fix_invalid(program: &mut Program, stack_offsets: &HashMap<String, i32>) {
    for FunctionDefinition { name, body, global: _ } in program.functions.iter_mut() {
        // stack_offset is negative, so convert to positive, round up to 16, then negate for AllocateStack
        let mut positive_offset = -stack_offsets[name];
        if positive_offset % 16 != 0 {
            positive_offset = ((positive_offset / 16) + 1) * 16;
        }
        let mut new_ins = vec![Instruction::AllocateStack(positive_offset)];
        for ins in body.iter() {
            match ins {
                Instruction::Mov { src, dst } if src.is_memory() && dst.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Reg::R10),
                    });
                    new_ins.push(Instruction::Mov {
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
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
                    dst,
                } if dst.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src: dst.clone(),
                        dst: Operand::Reg(Reg::R11),
                    });
                    new_ins.push(Instruction::Binary {
                        op: BinaryOp::Mult,
                        src: src.clone(),
                        dst: Operand::Reg(Reg::R11),
                    });
                    new_ins.push(Instruction::Mov {
                        src: Operand::Reg(Reg::R11),
                        dst: dst.clone(),
                    });
                }
                Instruction::Binary {
                    op: op @ (BinaryOp::Add | BinaryOp::Sub | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXOr),
                    src,
                    dst,
                } if src.is_memory() && dst.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Reg::R10),
                    });
                    new_ins.push(Instruction::Binary {
                        op: op.clone(),
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
                    });
                }
                Instruction::Binary {
                    op: op @ (BinaryOp::BitShl | BinaryOp::BitSar),
                    src,
                    dst,
                } if src.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Reg::CX),
                    });
                    new_ins.push(Instruction::Binary {
                        op: op.clone(),
                        src: Operand::Reg(Reg::CX),
                        dst: dst.clone(),
                    });
                }
                Instruction::Cmp { v1, v2 } if v1.is_memory() && v2.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src: v1.clone(),
                        dst: Operand::Reg(Reg::R10),
                    });
                    new_ins.push(Instruction::Cmp {
                        v1: Operand::Reg(Reg::R10),
                        v2: v2.clone(),
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
    for FunctionDefinition {
        name: _name,
        body,
        global: _,
    } in program.functions.iter_mut()
    {
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
