/*
   We allow undefined behavior that is valid c like int foo = foo + 1;

   todo: If we had a token mapping, we could provide better error messages
*/
use crate::parser::{BlockItem, Declaration, Expr, Function, Identifier, Program, Stmt};
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

// todo just mut init
fn resolve_exp(exp: &Expr, variable_map: &HashMap<String, String>) -> Result<Expr, SemanticError> {
    match exp {
        Expr::Assignment(e1, e2) => match &**e1 {
            Expr::Var(_) => {
                let resolved_e1 = resolve_exp(e1, variable_map)?;
                let resolved_e2 = resolve_exp(e2, variable_map)?;
                Ok(Expr::Assignment(Box::new(resolved_e1), Box::new(resolved_e2)))
            }
            _ => Err(SemanticError {
                message: "Left-hand side of assignment must be a variable".to_string(),
            }),
        },
        Expr::Var(Identifier(name)) => match variable_map.get(name) {
            Some(resolved_name) => Ok(Expr::Var(Identifier(resolved_name.clone()))),
            None => Err(SemanticError {
                message: format!("Variable \"{name}\" not defined"),
            }),
        },
        Expr::Unary(op, e) => {
            let resolved_e = resolve_exp(e, variable_map)?;
            Ok(Expr::Unary(op.clone(), Box::new(resolved_e)))
        }
        Expr::Binary(op, left, right) => {
            let resolved_left = resolve_exp(left, variable_map)?;
            let resolved_right = resolve_exp(right, variable_map)?;
            Ok(Expr::Binary(
                op.clone(),
                Box::new(resolved_left),
                Box::new(resolved_right),
            ))
        }
        Expr::Constant(n) => Ok(Expr::Constant(*n)),
    }
}

fn resolve_decoration(
    dec: &Declaration,
    variable_map: &mut HashMap<String, String>,
    name_gen: &mut NameGenerator,
) -> Result<Declaration, SemanticError> {
    let Declaration {
        name: Identifier(name),
        init,
    } = dec;
    if let std::collections::hash_map::Entry::Vacant(e) = variable_map.entry(name.clone()) {
        let unique_name = name_gen.next(name);
        e.insert(unique_name.clone());
        let new_init = match init {
            Some(e) => Some(resolve_exp(e, variable_map)?),
            None => None,
        };
        Ok(Declaration {
            name: Identifier(unique_name),
            init: new_init,
        })
    } else {
        Err(SemanticError {
            message: format!("Variable \"{name}\" already defined"),
        })
    }
}

fn resolve_statement(statement: &Stmt, variable_map: &HashMap<String, String>) -> Result<Stmt, SemanticError> {
    match statement {
        Stmt::Return(e) => Ok(Stmt::Return(resolve_exp(e, variable_map)?)),
        Stmt::Expression(e) => Ok(Stmt::Expression(resolve_exp(e, variable_map)?)),
        Stmt::Null => Ok(Stmt::Null),
    }
}

pub fn resolve_program(program: &Program) -> Result<(Program, NameGenerator), SemanticError> {
    let Program {
        function: Function { name, body },
    } = program;
    let mut new_body = Vec::with_capacity(body.len());
    let mut variable_map = HashMap::new();
    let mut name_gen = NameGenerator::new();
    for bi in body {
        let new_item = match bi {
            BlockItem::Statement(s) => BlockItem::Statement(resolve_statement(s, &variable_map)?),
            BlockItem::Declaration(d) => {
                BlockItem::Declaration(resolve_decoration(d, &mut variable_map, &mut name_gen)?)
            }
        };
        new_body.push(new_item);
    }
    Ok((
        Program {
            function: Function {
                name: name.clone(),
                body: new_body,
            },
        },
        name_gen,
    ))
}
