/*
   We allow undefined behavior that is valid c like int foo = foo + 1;
   We can add a warning for this in the future.

   We also don't do scope validation another case that deserves a warning
*/
use crate::parser::{Block, BlockItem, Declaration, Expr, Identifier, Program, Span, Stmt};
use std::collections::{HashMap, HashSet};
use std::fmt;

pub struct SemanticError {
    message: String,
    span: Option<Span>,
}

impl SemanticError {
    fn with_span(message: String, span: Span) -> Self {
        SemanticError {
            message,
            span: Some(span),
        }
    }
}

impl fmt::Debug for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.span {
            Some(span) => write!(
                f,
                "SemanticError at line {}, column {}: {}",
                span.line, span.column, self.message
            ),
            None => write!(f, "SemanticError: {}", self.message),
        }
    }
}

#[derive(Clone)]
struct VarInfo {
    renamed: String,
    span: Span,
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

fn resolve_exp(exp: &mut Expr, variable_map: &HashMap<String, VarInfo>) -> Result<(), SemanticError> {
    match exp {
        Expr::Assignment(e1, e2, span) | Expr::CompoundAssignment(_, e1, e2, span) => match &**e1 {
            Expr::Var(_, _) => {
                resolve_exp(e1, variable_map)?;
                resolve_exp(e2, variable_map)?;
                Ok(())
            }
            _ => Err(SemanticError::with_span(
                "Left-hand side of assignment must be a variable".to_string(),
                *span,
            )),
        },
        Expr::Var(Identifier(name), span) => match variable_map.get(name) {
            Some(var_info) => {
                *name = var_info.renamed.clone();
                Ok(())
            }
            None => Err(SemanticError::with_span(
                format!("Variable \"{name}\" not defined"),
                *span,
            )),
        },
        Expr::Unary(_, e) => resolve_exp(e, variable_map),
        Expr::Binary(_, left, right) => {
            resolve_exp(left, variable_map)?;
            resolve_exp(right, variable_map)
        }
        Expr::Constant(_) => Ok(()),
        Expr::PostFixOp(_op, e, span) | Expr::PreFixOp(_op, e, span) => match &**e {
            Expr::Var(_, _) => {
                resolve_exp(e, variable_map)?;
                Ok(())
            }
            _ => Err(SemanticError::with_span(
                "Prefix or Postfix operator applied to non lvalue".to_string(),
                *span,
            )),
        },
        Expr::Conditional(e, then_exp, else_exp) => {
            resolve_exp(e, variable_map)?;
            resolve_exp(then_exp, variable_map)?;
            resolve_exp(else_exp, variable_map)?;
            Ok(())
        }
    }
}

fn resolve_decoration(
    dec: &mut Declaration,
    variable_map: &mut HashMap<String, VarInfo>,
    name_gen: &mut NameGenerator,
    block_vars: &mut HashSet<String>
) -> Result<(), SemanticError> {
    let Identifier(name) = &dec.name;

    // if the value doesn't exist in block_vars we want to insert or update
    if block_vars.insert(name.clone()) {
        let unique_name = name_gen.next(name);
        let shadow = variable_map.insert(name.clone(), VarInfo {
            renamed: unique_name.clone(),
            span: dec.span,
        });
        if let Some(shadow) = shadow {
            eprintln!("Warning: Variable {name} at {} shadows {name} declared at {}. [WShadow]", dec.span, shadow.span);
        }
        dec.name = Identifier(unique_name);

        // Resolve the initializer if present
        if let Some(init_expr) = &mut dec.init {
            resolve_exp(init_expr, variable_map)?;
        }
        Ok(())
    } else {
        let original_def = variable_map.get(name).unwrap();
        Err(SemanticError::with_span(
            format!(
                "Variable \"{name}\" already defined at line {}, column {}",
                original_def.span.line, original_def.span.column
            ),
            dec.span,
        ))
    }
}

fn resolve_statement(
    statement: &mut Stmt,
    variable_map: &mut HashMap<String, VarInfo>,
    labels: &mut HashSet<String>,
    jumps: &mut HashSet<String>,
    name_gen: &mut NameGenerator
) -> Result<(), SemanticError> {
    match statement {
        Stmt::Return(e) => resolve_exp(e, variable_map),
        Stmt::Expression(e) => resolve_exp(e, variable_map),
        Stmt::Null => Ok(()),
        Stmt::If(e, then_stmt, else_stmt) => {
            resolve_exp(e, variable_map)?;
            resolve_statement(then_stmt, variable_map, labels, jumps, name_gen)?;
            match &mut **else_stmt {
                Some(else_stmt) => resolve_statement(else_stmt, variable_map, labels, jumps, name_gen),
                None => Ok(()),
            }
        }
        Stmt::Labeled(label_name, stmt) => {
            if !labels.insert(label_name.to_string()) {
                return Err(SemanticError {
                    message: format!("Label {label_name:} already defined"),
                    span: None,
                });
            }
            resolve_statement(stmt, variable_map, labels, jumps, name_gen)
        }
        Stmt::Goto(label_name) => {
            jumps.insert(label_name.to_string());
            Ok(())
        }
        Stmt::Compound(block) => {
            let mut shadow_map = variable_map.clone();
            resolve_block(block, &mut shadow_map, labels, jumps, name_gen)?;
            Ok(())
        }
    }
}

fn resolve_block(block: &mut Block,
                 mut variable_map: &mut HashMap<String, VarInfo>,
                 mut labels: &mut HashSet<String>,
                 mut jumps: &mut HashSet<String>,
                 mut name_gen: &mut NameGenerator) -> Result<(), SemanticError> {
    let mut block_vars = HashSet::new();
    for bi in block.iter_mut() {
        match bi {
            BlockItem::Statement(s) => {
                resolve_statement(s, variable_map, &mut labels, &mut jumps, name_gen)?;
            }
            BlockItem::Declaration(d) => {
                resolve_decoration(d, &mut variable_map, &mut name_gen, &mut block_vars)?;
            }
        }
    }
    Ok(())
}

pub fn resolve_program(program: &mut Program) -> Result<NameGenerator, SemanticError> {
    let mut variable_map = HashMap::new();
    let mut labels = HashSet::new();
    let mut jumps = HashSet::new();
    let mut name_gen = NameGenerator::new();

    resolve_block(&mut program.function.body, &mut variable_map, &mut labels, &mut jumps, &mut name_gen)?;

    let diff: Vec<_> = jumps.difference(&labels).collect();
    if diff.is_empty() {
        Ok(name_gen)
    } else {
        Err(SemanticError {
            message: format!("Jumps to nonexisting labels: {diff:#?}"),
            span: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{Stage, get_sandler_dirs, run_tests};

    #[test]
    fn test_conditional_valid() {
        let dirs = vec!["c_programs/conditional/valid/".to_string()];
        let (passed, failed) = run_tests(&dirs, true, &Stage::Validate);
        assert_eq!(failed.len(), 0, "Failed to validate valid files: {failed:?}");
        println!("Passed: {passed}");
    }

    #[test]
    fn sandler_tests_valid() {
        let dirs = get_sandler_dirs(true, &Stage::Validate);
        let (passed, failed) = run_tests(&dirs, true, &Stage::Validate);
        assert_eq!(failed.len(), 0, "Failed to parse valid files: {failed:?}");
        println!("Passed: {passed}");
    }

    #[test]
    fn sandler_tests_invalid() {
        let dirs = get_sandler_dirs(false, &Stage::Validate);
        let (passed, failed) = run_tests(&dirs, false, &Stage::Validate);
        assert_eq!(failed.len(), 0, "Should have rejected invalid files: {failed:?}");
        println!("Passed: {passed}");
    }
}
