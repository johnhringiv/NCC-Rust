use std::collections::HashMap;
use crate::parser;

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Complement,
    Negate,
}

#[derive(Clone, Debug)]
pub enum Val {
    Constant(i64),
    Var(String)
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Return(Val),
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Val,
    }
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
            let tacky_op = match op {
                parser::UnaryOp::BitwiseComplement => UnaryOp::Complement,
                parser::UnaryOp::Negate => UnaryOp::Negate,
            };
            instructions.push(Instruction::Unary {op: tacky_op, src, dst: dst.clone()});
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