use std::collections::HashMap;
use crate::parser;
use crate::parser::{Identifier, UnaryOp};
use crate::pretty::{ItfDisplay, simple_node, Node};
use colored::Colorize;

#[derive(Clone, Debug)]
pub enum Val {
    Constant(i64),
    Var(String)
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Subtract,
    Add,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOr,
    BitwiseLeftShift,
    BitwiseRightShift,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

impl From<&parser::BinOp> for BinOp {
    fn from(op: &parser::BinOp) -> Self {
        match op {
            parser::BinOp::Add => BinOp::Add,
            parser::BinOp::Subtract => BinOp::Subtract,
            parser::BinOp::Multiply => BinOp::Multiply,
            parser::BinOp::Divide => BinOp::Divide,
            parser::BinOp::Remainder => BinOp::Remainder,
            parser::BinOp::BitwiseAnd => BinOp::BitwiseAnd,
            parser::BinOp::BitwiseOr => BinOp::BitwiseOr,
            parser::BinOp::BitwiseXOr => BinOp::BitwiseXOr,
            parser::BinOp::BitwiseLeftShift => BinOp::BitwiseLeftShift,
            parser::BinOp::BitwiseRightShift => BinOp::BitwiseRightShift,
            parser::BinOp::Equal => BinOp::Equal,
            parser::BinOp::NotEqual => BinOp::NotEqual,
            parser::BinOp::LessThan => BinOp::LessThan,
            parser::BinOp::LessOrEqual => BinOp::LessOrEqual,
            parser::BinOp::GreaterThan => BinOp::GreaterThan,
            parser::BinOp::GreaterOrEqual => BinOp::GreaterOrEqual,
            _ => unreachable!("Unsupported binary operator {:#?} in tacky conversion", op),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Return(Val),
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Val,
    },
    Binary {
        op: BinOp,
        src1: Val,
        src2: Val,
        dst: Val,
    },
    Copy {
        src: Val,
        dst: Val,
    },
    Jump {
        target: Identifier
    },
    JumpIfZero {
        condition: Val,
        target: Identifier
    },
    JumpIfNotZero {
        condition: Val,
        target: Identifier
    },
    Label(Identifier),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Program {
    pub function: FunctionDefinition,
}

pub struct NameGenerator {
    counts: HashMap<String, usize>,
}

impl NameGenerator {
    pub fn new() -> NameGenerator { NameGenerator {counts: HashMap::new()}}
    
    pub fn next(&mut self, base: &str) -> String {
        let count = self.counts.entry(base.to_string()).or_insert(0);
        *count += 1;
        format!("{}.{}", base, count)
    }
}

fn tackify_expr(e: &parser::Expr, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator) -> Val {
    match e {
        parser::Expr::Constant(c) => Val::Constant(*c),
        parser::Expr::Unary(op, inner) => {
            let src = tackify_expr(inner, instructions, name_generator);
            let dst = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Unary {op: op.clone(), src, dst: dst.clone()});
            dst
        }
        parser::Expr::Binary(parser::BinOp::And, e1, e2) => {
            let src1 = tackify_expr(e1, instructions, name_generator);
            let false_label = Identifier(name_generator.next("and_false"));
            instructions.push(Instruction::JumpIfZero {condition: src1, target: false_label.clone()});
            let src2 = tackify_expr(e2, instructions, name_generator);
            instructions.push(Instruction::JumpIfZero {condition: src2, target: false_label.clone()});
            let result = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Copy {src: Val::Constant(1), dst: result.clone()});
            let end_label = Identifier(name_generator.next("end"));
            instructions.push(Instruction::Jump {target: end_label.clone()});
            instructions.push(Instruction::Label(false_label));
            instructions.push(Instruction::Copy {src: Val::Constant(0), dst: result.clone()});
            instructions.push(Instruction::Label(end_label));
            result
        }
        parser::Expr::Binary(parser::BinOp::Or, e1, e2) => {
            let src1 = tackify_expr(e1, instructions, name_generator);
            let false_label = Identifier(name_generator.next("or_false"));
            instructions.push(Instruction::JumpIfNotZero {condition: src1, target: false_label.clone()});
            let src2 = tackify_expr(e2, instructions, name_generator);
            instructions.push(Instruction::JumpIfNotZero {condition: src2, target: false_label.clone()});
            let result = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Copy {src: Val::Constant(0), dst: result.clone()});
            let end_label = Identifier(name_generator.next("end"));
            instructions.push(Instruction::Jump {target: end_label.clone()});
            instructions.push(Instruction::Label(false_label));
            instructions.push(Instruction::Copy {src: Val::Constant(1), dst: result.clone()});
            instructions.push(Instruction::Label(end_label));
            result
        }
        parser::Expr::Binary(op, e1, e2) => {
            // C doesn't mandate order of src1 and src2 eval leading to undefined behavior
            let src1 = tackify_expr(e1, instructions, name_generator);
            let src2 = tackify_expr(e2, instructions, name_generator);
            let dst = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Binary {op: BinOp::from(op), src1, src2, dst: dst.clone()});
            dst
        }
    }
}

fn tackify_stmt(stmt: &parser::Stmt, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator) {
    match stmt {
        parser::Stmt::Return(expr) => {
            let val = tackify_expr(expr, instructions, name_generator);
            instructions.push(Instruction::Return(val));
        }
    }
}

fn tackify_function(func: &parser::Function, name_generator: &mut NameGenerator) -> FunctionDefinition { 
    let mut instructions = Vec::new();
    let parser::Identifier(name) = &func.name;
    
    tackify_stmt(&func.body, &mut instructions, name_generator);

    FunctionDefinition {
        name: name.clone(),
        body: instructions,
    }
}

pub fn tackify_program(program: &parser::Program) -> Program {
    let mut name_generator = NameGenerator::new();
    
    let function = tackify_function(&program.function, &mut name_generator);
    
    Program { function }
}
simple_node!(BinOp);

impl ItfDisplay for Val {
    fn itf_node(&self) -> Node {
        match self {
            Val::Constant(c) => Node::branch("Const".cyan().to_string(), vec![c.itf_node()]),
            Val::Var(s) => Node::branch("Var".cyan().to_string(), vec![s.itf_node()]),
        }
    }
}

impl ItfDisplay for Instruction {
    fn itf_node(&self) -> Node {
        match self {
            Instruction::Return(v) => Node::branch("Return".cyan().to_string(), vec![v.itf_node()]),
            Instruction::Unary { op, src, dst } => Node::branch(
                "Unary".cyan().to_string(),
                vec![op.itf_node(), src.itf_node(), dst.itf_node()],
            ),
            Instruction::Binary { op, src1, src2, dst } => Node::branch(
                "Binary".cyan().to_string(),
                vec![op.itf_node(), src1.itf_node(), src2.itf_node(), dst.itf_node()],
            ),
            Instruction::Copy { src, dst } => Node::branch(
                "Copy".cyan().to_string(),
                vec![src.itf_node(), dst.itf_node()],
            ),
            Instruction::Jump { target } => Node::branch("Jump".cyan().to_string(), vec![target.itf_node()]),
            Instruction::JumpIfZero { condition, target } => Node::branch(
                "JumpIfZero".cyan().to_string(),
                vec![condition.itf_node(), target.itf_node()],
            ),
            Instruction::JumpIfNotZero { condition, target } => Node::branch(
                "JumpIfNotZero".cyan().to_string(),
                vec![condition.itf_node(), target.itf_node()],
            ),
            Instruction::Label(lbl) => Node::branch("Label".cyan().to_string(), vec![lbl.itf_node()]),
        }
    }
}

impl ItfDisplay for FunctionDefinition {
    fn itf_node(&self) -> Node {
        let name_line = Node::leaf(format!("name: {}", self.name.itf_node().text));
        let body_children: Vec<Node> = self.body.iter().map(|i| i.itf_node()).collect();
        let body_node = Node::branch("body".cyan().to_string(), body_children);
        Node::branch("FunctionDefinition".cyan().to_string(), vec![name_line, body_node])
    }
}

impl ItfDisplay for Program {
    fn itf_node(&self) -> Node {
        Node::branch("Program".cyan().to_string(), vec![self.function.itf_node()])
    }
}
