/*
   We allow undefined behavior that is valid c like int foo = foo + 1;
   We can add a warning for this in the future.

   We also don't do scope validation another case that deserves a warning
*/
use crate::lexer::Span;
use crate::parser::{Block, BlockItem, Declaration, Expr, ForInit, Identifier, Program, Stmt, UnaryOp, BinOp, SwitchIntType};
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
        Stmt::While(exp, stmt, _) | Stmt::Switch(exp, stmt, ..) | Stmt::Case(exp, stmt, ..) => {
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
        Stmt::Default(stmt, ..) => resolve_statement(stmt, variable_map, labels, jumps, name_gen)
    }
}

/// Evaluates a constant expression to an i32 value
/// Returns None if the expression is not a compile-time constant
fn eval_constant_expr(expr: &Expr) -> Option<i32> {
    match expr {
        Expr::Constant(val) => Some(*val),
        Expr::Unary(op, inner) => {
            let inner_val = eval_constant_expr(inner)?;
            match op {
                UnaryOp::Negate => Some(-inner_val),
                UnaryOp::BitwiseComplement => Some(!inner_val),
                UnaryOp::Not => Some(if inner_val == 0 { 1 } else { 0 }),
            }
        }
        Expr::Binary(op, left, right) => {
            let left_val = eval_constant_expr(left)?;
            let right_val = eval_constant_expr(right)?;
            match op {
                BinOp::Add => Some(left_val.wrapping_add(right_val)),
                BinOp::Subtract => Some(left_val.wrapping_sub(right_val)),
                BinOp::Multiply => Some(left_val.wrapping_mul(right_val)),
                BinOp::Divide => {
                    if right_val == 0 {
                        None // Division by zero
                    } else {
                        Some(left_val / right_val)
                    }
                }
                BinOp::Remainder => {
                    if right_val == 0 {
                        None // Division by zero
                    } else {
                        Some(left_val % right_val)
                    }
                }
                BinOp::BitwiseAnd => Some(left_val & right_val),
                BinOp::BitwiseOr => Some(left_val | right_val),
                BinOp::BitwiseXOr => Some(left_val ^ right_val),
                BinOp::BitwiseLeftShift => Some(left_val << (right_val as u32)),
                BinOp::BitwiseRightShift => Some(left_val >> (right_val as u32)),
                BinOp::Equal => Some(if left_val == right_val { 1 } else { 0 }),
                BinOp::NotEqual => Some(if left_val != right_val { 1 } else { 0 }),
                BinOp::LessThan => Some(if left_val < right_val { 1 } else { 0 }),
                BinOp::LessOrEqual => Some(if left_val <= right_val { 1 } else { 0 }),
                BinOp::GreaterThan => Some(if left_val > right_val { 1 } else { 0 }),
                BinOp::GreaterOrEqual => Some(if left_val >= right_val { 1 } else { 0 }),
                BinOp::And => Some(if left_val != 0 && right_val != 0 { 1 } else { 0 }),
                BinOp::Or => Some(if left_val != 0 || right_val != 0 { 1 } else { 0 }),
                BinOp::Assignment | BinOp::CompoundAssignment | BinOp::Conditional => unreachable!("only used for parsing")
            }
        }
        Expr::Conditional(cond, true_expr, false_expr) => {
            let cond_val = eval_constant_expr(cond)?;
            if cond_val != 0 {
                eval_constant_expr(true_expr)
            } else {
                eval_constant_expr(false_expr)
            }
        }
        // Variables, assignments, function calls etc. are not constant
        _ => None,
    }
}

enum LabelTag {
    Loop(u64),
    Switch(u64)
}

struct LabelTracker {
    cur_label: Vec<LabelTag>,
    next_loop_label: u64,
    next_switch_label: u64,
    switch_to_cases: HashMap<u64, HashSet<SwitchIntType>> // switch -> case e
}

impl LabelTracker {
    fn new() -> Self {
        LabelTracker {
            cur_label: vec![],
            next_loop_label: 0,
            next_switch_label: 0,
            switch_to_cases: HashMap::new(),
        }
    }

    fn get_break_label(&self) -> Option<String> {
        if let Some(label) = self.cur_label.last() {
            let s = match label {
                LabelTag::Switch(l) => format!("break_switch.{l}"),
                LabelTag::Loop(l) => format!("break_loop.{l}"),
            };
            Some(s)
        } else {
            None
        }
    }

    fn get_continue_label(&self) -> Option<String> {
        if let Some(LabelTag::Loop(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => false,
            LabelTag::Loop(..) => true
        }) {
            Some(format!("continue_loop.{label}"))
        } else {
            None
        }
    }

    fn get_switch_case(&mut self, c: i32, span: &Span) -> Result<String, SemanticError> {
        // get the active switch id
        if let Some(LabelTag::Switch(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => true,
            LabelTag::Loop(..) => false
        }) {
            let case_label = format!("switch.{label}_case.{c}");
            if self.switch_to_cases.get_mut(label).unwrap().insert(SwitchIntType::Int(c)) {
                Ok(case_label)
            } else {
                Err(SemanticError::with_span(format!("Duplicate Case '{}' found in switch.", format!("{}", c).bold()), *span))
            }
        } else {
            Err(SemanticError::with_span("Case outside of switch".to_string(), *span))
        }
    }

    fn get_switch_default(&mut self, span: &Span) -> Result<String, SemanticError> {
        // get the active switch id
        if let Some(LabelTag::Switch(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => true,
            LabelTag::Loop(..) => false
        }) {
            let switch_label = format!("switch.{label}_default");
            if self.switch_to_cases.get_mut(label).unwrap().insert(SwitchIntType::Default) {
                Ok(switch_label)
            } else {
                Err(SemanticError::with_span(format!("Duplicate '{}' found in switch.", "default".bold()), *span))
            }
        } else {
            Err(SemanticError::with_span("Default outside of switch".to_string(), *span))
        }
    }
    fn next_loop_label(&mut self) -> u64 {
        let res = self.next_loop_label;
        self.next_loop_label += 1;
        self.cur_label.push(LabelTag::Loop(res));
        res
    }

    fn next_switch_label(&mut self) -> u64 {
        let res = self.next_switch_label;
        self.next_switch_label += 1;
        self.switch_to_cases.insert(res, HashSet::new());
        self.cur_label.push(LabelTag::Switch(res));
        res
    }

    fn pop(&mut self) {
        self.cur_label.pop();
    }

    fn pop_switch(&mut self, switch_id: u64) -> HashSet<SwitchIntType> {
        self.cur_label.pop();
        self.switch_to_cases.get(&switch_id).unwrap().clone()
    }

}

fn label_statement(stmt: &mut Stmt, label_tracker: &mut LabelTracker) -> Result<(), SemanticError> {
    match stmt {
        // terminating statements
        Stmt::Return(_) | Stmt::Expression(_) | Stmt::Goto(..) | Stmt::Null => Ok(()),
        Stmt::If(_, then_stmt, else_stmt) => {
            label_statement(then_stmt, label_tracker)?;
            if let Some(c) = &mut **else_stmt {
                label_statement(c, label_tracker)
            } else {
                Ok(())
            }
        }
        Stmt::Labeled(_, stmt, _) => label_statement(stmt, label_tracker),
        Stmt::Compound(block) => {
            for bi in block.iter_mut() {
                match bi {
                    BlockItem::Statement(stmt) => label_statement(stmt, label_tracker)?,
                    BlockItem::Declaration(_) => {}
                }
            }
            Ok(())
        }
        Stmt::Break(Identifier(s), span) => {
            if let Some(break_label) = label_tracker.get_break_label() {
                *s = break_label;
                Ok(())
            } else {
                Err(SemanticError::with_span(
                    "Break statement outside loop".to_string(),
                    *span,
                ))
            }
        }
        Stmt::Continue(Identifier(s), span) => {
            if let Some(label) = label_tracker.get_continue_label() {
                *s = label;
                Ok(())
            } else {
                Err(SemanticError::with_span(
                    "Continue statement outside loop".to_string(),
                    *span,
                ))
            }
        }
        Stmt::While(_, body, loop_num) | Stmt::DoWhile(body, _, loop_num) | Stmt::For(_, _, _, body, loop_num) => {
            *loop_num = label_tracker.next_loop_label();
            let result = label_statement(body, label_tracker);
            label_tracker.pop();
            result
        }
        Stmt::Switch(exp, stmt, switch_num, cases) => {
            //switch.1 (e) -> goto switch.1_case_e
            *switch_num = label_tracker.next_switch_label();
            let result = label_statement(stmt, label_tracker);
            *cases = label_tracker.pop_switch(*switch_num);
            result
        }
        Stmt::Case(exp, stmt, Identifier(s), span) => {
            if let Some(exp) = eval_constant_expr(exp) {
                *s = label_tracker.get_switch_case(exp as i32, span)?;
                label_statement(stmt, label_tracker)
            } else {
                Err(SemanticError::with_span("Expression is not an integer constant expression".to_string(), *span))
            }
        }
        Stmt::Default(stmt, Identifier(s), span) => {
            *s = label_tracker.get_switch_default(span)?;
            label_statement(stmt, label_tracker)
        }
    }
}

fn label_block(block: &mut Block, label_tracker: &mut LabelTracker) -> Result<(), SemanticError> {
    for bi in block.iter_mut() {
        if let BlockItem::Statement(s) = bi {
            label_statement(s, label_tracker)?;
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

    let mut label_tracker = LabelTracker::new();
    label_block(&mut program.function.body, &mut label_tracker)?;

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
