/*
   We allow undefined behavior that is valid c like int foo = foo + 1;
   We can add a warning for this in the future.

   We also don't do scope validation another case that deserves a warning
*/
use crate::lexer::Span;
use crate::parser::{Block, BlockItem, Declaration, Expr, ForInit, Identifier, Program, Stmt};
use colored::*;
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
            Some(span) => write!(f, "{}: {}: {}", span, "SemanticError".red(), self.message),
            None => write!(f, "{}: {}", "error".red(), self.message),
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
                format!("Variable \'{}\' not defined", name.bold()),
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
    block_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    let Identifier(name) = &dec.name;

    // if the value doesn't exist in block_vars we want to insert or update
    if block_vars.insert(name.clone()) {
        let unique_name = name_gen.next(name);
        let shadow = variable_map.insert(
            name.clone(),
            VarInfo {
                renamed: unique_name.clone(),
                span: dec.span,
            },
        );
        if let Some(shadow) = shadow {
            eprintln!(
                "{}: {}: variable {} shadows previous declaration {}",
                dec.span,
                "warning".purple(),
                format!("'{}'", name).bold(),
                "[-Wshadow]".purple()
            );
            eprintln!(
                "{}: {}: previous declaration of {} was here",
                shadow.span,
                "note".cyan(),
                format!("'{}'", name).bold()
            );
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
                "variable {} already declared\n{}: {}: previous decoration was here",
                format!("'{}'", name).bold(),
                original_def.span,
                "note".to_string().cyan(),
            ),
            dec.span,
        ))
    }
}

fn resolve_statement(
    statement: &mut Stmt,
    variable_map: &mut HashMap<String, VarInfo>,
    labels: &mut HashSet<String>,
    jumps: &mut HashMap<String, Span>,
    name_gen: &mut NameGenerator,
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
        Stmt::Labeled(label_name, stmt, span) => {
            if !labels.insert(label_name.to_string()) {
                return Err(SemanticError {
                    message: format!("Label {label_name:} already defined at {span}"),
                    span: None,
                });
            }
            resolve_statement(stmt, variable_map, labels, jumps, name_gen)
        }
        Stmt::Goto(label_name, span) => {
            jumps.insert(label_name.to_string(), *span);
            Ok(())
        }
        Stmt::Compound(block) => {
            let mut shadow_map = variable_map.clone();
            resolve_block(block, &mut shadow_map, labels, jumps, name_gen)
        }
        Stmt::While(exp, stmt, _) => {
            resolve_exp(exp, variable_map)?;
            resolve_statement(stmt, variable_map, labels, jumps, name_gen)
        }
        Stmt::DoWhile(stmt, exp, _) => {
            resolve_statement(stmt, variable_map, labels, jumps, name_gen)?;
            resolve_exp(exp, variable_map)
        }
        Stmt::For(init, e1, e2, stmt, _) => {
            let mut shadow_map = variable_map.clone();
            match init {
                ForInit::InitExp(exp) => {
                    if let Some(e) = exp {
                        resolve_exp(e, &shadow_map)?;
                    }
                }
                ForInit::InitDecl(dec) => resolve_decoration(dec, &mut shadow_map, name_gen, &mut HashSet::new())?,
            }
            if let Some(e1) = e1 {
                resolve_exp(e1, &shadow_map)?;
            }
            if let Some(e2) = e2 {
                resolve_exp(e2, &shadow_map)?;
            }
            resolve_statement(stmt, &mut shadow_map, labels, jumps, name_gen)
        }
        Stmt::Break(..) | Stmt::Continue(..) => Ok(()),
    }
}

fn label_statement(stmt: &mut Stmt, cur_label: &mut Option<u64>, next_label: &mut u64) -> Result<(), SemanticError> {
    match stmt {
        // terminating statements
        Stmt::Return(_) | Stmt::Expression(_) | Stmt::Goto(..) | Stmt::Null => Ok(()),
        Stmt::If(_, then_stmt, else_stmt) => {
            label_statement(then_stmt, cur_label, next_label)?;
            if let Some(c) = &mut **else_stmt {
                label_statement(c, cur_label, next_label)
            } else {
                Ok(())
            }
        }
        Stmt::Labeled(_, stmt, _) => label_statement(stmt, cur_label, next_label),
        Stmt::Compound(block) => {
            for bi in block.iter_mut() {
                match bi {
                    BlockItem::Statement(stmt) => label_statement(stmt, cur_label, next_label)?,
                    BlockItem::Declaration(_) => {}
                }
            }
            Ok(())
        }
        Stmt::Break(Identifier(s), span) => {
            if let Some(label) = cur_label {
                *s = format!("break_loop.{label}");
                Ok(())
            } else {
                Err(SemanticError::with_span(
                    "Break statement outside loop".to_string(),
                    *span,
                ))
            }
        }
        Stmt::Continue(Identifier(s), span) => {
            if let Some(label) = cur_label {
                *s = format!("continue_loop.{label}");
                Ok(())
            } else {
                Err(SemanticError::with_span(
                    "Continue statement outside loop".to_string(),
                    *span,
                ))
            }
        }
        Stmt::While(_, body, loop_num) | Stmt::DoWhile(body, _, loop_num) | Stmt::For(_, _, _, body, loop_num) => {
            let new_label = *next_label;
            *next_label += 1;  // Increment for next loop
            let saved_label = *cur_label;  // Save the current label
            *cur_label = Some(new_label);
            *loop_num = new_label;
            let result = label_statement(body, cur_label, next_label);
            *cur_label = saved_label;  // Restore the previous label
            result
        }
    }
}

fn label_block(block: &mut Block, cur_label: &mut Option<u64>, next_label: &mut u64) -> Result<(), SemanticError> {
    for bi in block.iter_mut() {
        if let BlockItem::Statement(s) = bi {
            label_statement(s, cur_label, next_label)?;
        }
    }
    Ok(())
}

fn resolve_block(
    block: &mut Block,
    variable_map: &mut HashMap<String, VarInfo>,
    labels: &mut HashSet<String>,
    jumps: &mut HashMap<String, Span>,
    name_gen: &mut NameGenerator,
) -> Result<(), SemanticError> {
    let mut block_vars = HashSet::new();

    for bi in block.iter_mut() {
        match bi {
            BlockItem::Statement(s) => {
                resolve_statement(s, variable_map, labels, jumps, name_gen)?;
            }
            BlockItem::Declaration(d) => {
                resolve_decoration(d, variable_map, name_gen, &mut block_vars)?;
            }
        }
    }
    Ok(())
}

pub fn resolve_program(program: &mut Program) -> Result<NameGenerator, SemanticError> {
    let mut variable_map = HashMap::new();
    let mut labels = HashSet::new();
    let mut jumps: HashMap<String, Span> = HashMap::new();
    let mut name_gen = NameGenerator::new();

    let mut cur_label = None;
    let mut next_label = 1;
    label_block(&mut program.function.body, &mut cur_label, &mut next_label)?;

    resolve_block(
        &mut program.function.body,
        &mut variable_map,
        &mut labels,
        &mut jumps,
        &mut name_gen,
    )?;

    // Find jumps to non-existent labels and report with spans
    let mut undefined_jumps = Vec::new();
    for (label, span) in jumps.iter() {
        if !labels.contains(label) {
            undefined_jumps.push((label.clone(), *span));
        }
    }

    if undefined_jumps.is_empty() {
        Ok(name_gen)
    } else {
        let (label, span) = &undefined_jumps[0];
        Err(SemanticError::with_span(
            format!("Jump to undefined label '{}'", label.bold()),
            *span,
        ))
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
