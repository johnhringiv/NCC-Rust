/*
   We allow undefined behavior that is valid c like int foo = foo + 1;
*/
use crate::parser::{BlockItem, Declaration, Expr, Identifier, Program, Stmt};
use std::collections::HashMap;
use std::fmt;

pub struct SemanticError {
    message: String,
}

impl fmt::Debug for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SemanticError: {}", self.message)
    }
}

pub struct NameGenerator {
    counts: HashMap<String, usize>,
}

impl NameGenerator {
    pub fn new() -> NameGenerator {
        NameGenerator { counts: HashMap::new() }
    }

    pub fn next(&mut self, base: &str) -> String {
        let count = self.counts.entry(base.to_string()).or_insert(0);
        *count += 1;
        format!("{base}.{count}")
    }
}

fn resolve_exp(exp: &mut Expr, variable_map: &HashMap<String, String>) -> Result<(), SemanticError> {
    match exp {
        Expr::Assignment(e1, e2) => match &**e1 {
            Expr::Var(_) => {
                resolve_exp(e1, variable_map)?;
                resolve_exp(e2, variable_map)?;
                Ok(())
            }
            _ => Err(SemanticError {
                message: "Left-hand side of assignment must be a variable".to_string(),
            }),
        },
        Expr::Var(Identifier(name)) => match variable_map.get(name) {
            Some(resolved_name) => {
                *name = resolved_name.clone();
                Ok(())
            }
            None => Err(SemanticError {
                message: format!("Variable \"{name}\" not defined"),
            }),
        },
        Expr::Unary(_, e) => {
            resolve_exp(e, variable_map)
        }
        Expr::Binary(_, left, right) => {
            resolve_exp(left, variable_map)?;
            resolve_exp(right, variable_map)
        }
        Expr::Constant(_) => Ok(()),
    }
}

fn resolve_decoration(
    dec: &mut Declaration,
    variable_map: &mut HashMap<String, String>,
    name_gen: &mut NameGenerator,
) -> Result<(), SemanticError> {
    let Identifier(name) = &dec.name;
    if let std::collections::hash_map::Entry::Vacant(e) = variable_map.entry(name.clone()) {
        let unique_name = name_gen.next(name);
        e.insert(unique_name.clone());
        dec.name = Identifier(unique_name);
        
        // Resolve the initializer if present
        if let Some(init_expr) = &mut dec.init {
            resolve_exp(init_expr, variable_map)?;
        }
        Ok(())
    } else {
        Err(SemanticError {
            message: format!("Variable \"{name}\" already defined"),
        })
    }
}

fn resolve_statement(statement: &mut Stmt, variable_map: &HashMap<String, String>) -> Result<(), SemanticError> {
    match statement {
        Stmt::Return(e) => resolve_exp(e, variable_map),
        Stmt::Expression(e) => resolve_exp(e, variable_map),
        Stmt::Null => Ok(()),
    }
}

pub fn resolve_program(program: &mut Program) -> Result<NameGenerator, SemanticError> {
    let mut variable_map = HashMap::new();
    let mut name_gen = NameGenerator::new();
    
    for bi in &mut program.function.body {
        match bi {
            BlockItem::Statement(s) => {
                resolve_statement(s, &variable_map)?;
            }
            BlockItem::Declaration(d) => {
                resolve_decoration(d, &mut variable_map, &mut name_gen)?;
            }
        }
    }
    
    Ok(name_gen)
}
