/*
   We allow undefined behavior that is valid c like int foo = foo + 1;
   We can add a warning for this in the future.

   We also don't do scope validation another case that deserves a warning
*/
use crate::lexer::Span;
use crate::parser::{
    BinOp, Block, BlockItem, Declaration, Expr, ForInit, FunDeclaration, Identifier, Program, SpannedStmt, Stmt,
    SwitchIntType, UnaryOp, VarDeclaration,
};
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

#[derive(Clone, PartialEq, Debug)]
pub enum SymbolType {
    Int,
    FunType(u64 /*param count*/),
}

#[derive(Clone)]
struct Symbol {
    symbol_type: SymbolType,
    initialized: bool,
    span: Span,
}

type SymbolTable = HashMap<String, Symbol>;

#[derive(Clone)]
struct VarInfo {
    renamed: String,
    span: Span,
    ext_link: bool,
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

fn typecheck_variable_declaration(decl: &VarDeclaration, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    let sym = Symbol {
        symbol_type: SymbolType::Int,
        initialized: decl.init.is_some(),
        span: decl.span,
    };
    symbols.insert(decl.name.0.clone(), sym);
    if let Some(init) = &decl.init {
        typecheck_exp(init, symbols)
    } else {
        Ok(())
    }
}

fn typecheck_exp(exp: &Expr, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    match exp {
        Expr::Constant(_) => Ok(()),
        Expr::Var(Identifier(name), span) => {
            if let Some(expected) = symbols.get(name) {
                match expected.symbol_type {
                    SymbolType::FunType(_) => Err(SemanticError::with_span(
                        format!("cannot use function '{}' as a variable", name.bold()),
                        *span,
                    )),
                    SymbolType::Int => Ok(()),
                }
            } else {
                unreachable!("Undefined variable in typechecking")
            }
        }
        Expr::Unary(_, e1) | Expr::PostFixOp(_, e1, _) | Expr::PreFixOp(_, e1, _) => typecheck_exp(e1, symbols),
        Expr::Binary(_, e1, e2) | Expr::Assignment(e1, e2, _) | Expr::CompoundAssignment(_, e1, e2, _) => {
            typecheck_exp(e1, symbols)?;
            typecheck_exp(e2, symbols)
        }
        Expr::Conditional(e1, e2, e3) => {
            typecheck_exp(e1, symbols)?;
            typecheck_exp(e2, symbols)?;
            typecheck_exp(e3, symbols)
        }
        Expr::FunctionCall(Identifier(name), args, span) => {
            if let Some(expected) = symbols.get(name) {
                match expected.symbol_type {
                    SymbolType::FunType(param_cnt) => {
                        if param_cnt != args.len() as u64 {
                            Err(SemanticError::with_span(
                                format!(
                                    "function '{}' expects {} argument{} but {} {} provided",
                                    name.bold(),
                                    param_cnt,
                                    if param_cnt == 1 { "" } else { "s" },
                                    args.len(),
                                    if args.len() == 1 { "was" } else { "were" }
                                ),
                                *span,
                            ))
                        } else {
                            for arg in args {
                                typecheck_exp(arg, symbols)?
                            }
                            Ok(())
                        }
                    }
                    SymbolType::Int => Err(SemanticError::with_span(
                        format!("cannot call '{}' as a function: identifier is a variable", name.bold()),
                        *span,
                    )),
                }
            } else {
                unreachable!("Undefined function call in typechecking")
            }
        }
    }
}

fn typecheck_function_declaration(decl: &FunDeclaration, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    let mut insert = true;
    let sym = Symbol {
        symbol_type: SymbolType::FunType(decl.params.len() as u64),
        initialized: decl.body.is_some(),
        span: decl.span,
    };
    if let Some(old_dec) = symbols.get(&decl.name.0) {
        if old_dec.symbol_type != sym.symbol_type {
            return Err(SemanticError::with_span(
                format!(
                    "incompatible declaration of function '{}'\n{}: {}: previous declaration was here",
                    decl.name.0.bold(),
                    old_dec.span,
                    "note".cyan()
                ),
                decl.span,
            ));
        }
        if old_dec.initialized && decl.body.is_some() {
            return Err(SemanticError::with_span(
                format!(
                    "redefinition of function '{}'\n{}: {}: previous definition was here",
                    decl.name.0.bold(),
                    old_dec.span,
                    "note".cyan()
                ),
                decl.span,
            ));
        }
        insert = !old_dec.initialized && sym.initialized;
    }

    if insert {
        symbols.insert(decl.name.0.clone(), sym);
    }

    if let Some(body) = decl.body.as_ref() {
        for Identifier(name) in &decl.params {
            symbols.insert(
                name.clone(),
                Symbol {
                    symbol_type: SymbolType::Int,
                    initialized: true,
                    span: decl.span,
                },
            );
        }
        typecheck_block(body, symbols)
    } else {
        Ok(())
    }
}

fn typecheck_block(block: &Block, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    for item in block {
        match item {
            BlockItem::Declaration(Declaration::VarDeclaration(var)) => {
                typecheck_variable_declaration(var, symbols)?;
            }
            BlockItem::Declaration(Declaration::FunDeclaration(fun)) => {
                typecheck_function_declaration(fun, symbols)?;
            }
            BlockItem::Statement(stmt) => {
                typecheck_stmt(&stmt.stmt, symbols)?;
            }
        }
    }
    Ok(())
}

fn typecheck_stmt(stmt: &Stmt, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    match stmt {
        Stmt::Return(e1) | Stmt::Expression(e1) => typecheck_exp(e1, symbols),
        Stmt::If(cond, then_s, else_s) => {
            typecheck_exp(cond, symbols)?;
            typecheck_stmt(&then_s.stmt, symbols)?;
            if let Some(e) = else_s.as_ref() {
                typecheck_stmt(&e.stmt, symbols)?;
            }
            Ok(())
        }
        Stmt::Goto(_) | Stmt::Null | Stmt::Break(_) | Stmt::Continue(_) => Ok(()),
        Stmt::Labeled(_, s) | Stmt::Default(s, _) => typecheck_stmt(&s.stmt, symbols),
        Stmt::Compound(block) => typecheck_block(block, symbols),
        Stmt::While(e, s, _) | Stmt::DoWhile(s, e, _) | Stmt::Switch(e, s, ..) | Stmt::Case(e, s, _) => {
            typecheck_stmt(&s.stmt, symbols)?;
            typecheck_exp(e, symbols)
        }
        Stmt::For(init, cond, post, body, _) => {
            match init {
                ForInit::InitExp(Some(e)) => typecheck_exp(e, symbols)?,
                ForInit::InitExp(None) => {}
                ForInit::InitDecl(decl) => typecheck_variable_declaration(decl, symbols)?,
            }
            if let Some(cond) = cond {
                typecheck_exp(cond, symbols)?;
            }
            if let Some(post) = post {
                typecheck_exp(post, symbols)?;
            }
            typecheck_stmt(&body.stmt, symbols)
        }
    }
}

fn resolve_exp(
    exp: &mut Expr,
    variable_map: &HashMap<String, VarInfo>,
    used_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    match exp {
        Expr::Assignment(e1, e2, span) | Expr::CompoundAssignment(_, e1, e2, span) => match &**e1 {
            Expr::Var(_, _) => {
                resolve_exp(e1, variable_map, used_vars)?;
                resolve_exp(e2, variable_map, used_vars)?;
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
                used_vars.insert(name.clone());
                Ok(())
            }
            None => Err(SemanticError::with_span(
                format!("Variable \'{}\' not defined", name.bold()),
                *span,
            )),
        },
        Expr::Unary(_, e) => resolve_exp(e, variable_map, used_vars),
        Expr::Binary(_, left, right) => {
            resolve_exp(left, variable_map, used_vars)?;
            resolve_exp(right, variable_map, used_vars)
        }
        Expr::Constant(_) => Ok(()),
        Expr::PostFixOp(_op, e, span) | Expr::PreFixOp(_op, e, span) => match &**e {
            Expr::Var(_, _) => {
                resolve_exp(e, variable_map, used_vars)?;
                Ok(())
            }
            _ => Err(SemanticError::with_span(
                "Prefix or Postfix operator applied to non lvalue".to_string(),
                *span,
            )),
        },
        Expr::Conditional(e, then_exp, else_exp) => {
            resolve_exp(e, variable_map, used_vars)?;
            resolve_exp(then_exp, variable_map, used_vars)?;
            resolve_exp(else_exp, variable_map, used_vars)?;
            Ok(())
        }
        Expr::FunctionCall(Identifier(name), args, span) => match variable_map.get(name) {
            Some(var_info) => {
                // checking if new name matches old tells us if we have a variable shadowing
                if name == &var_info.renamed {
                    for arg in args {
                        resolve_exp(arg, variable_map, used_vars)?;
                    }
                    Ok(())
                } else {
                    Err(SemanticError::with_span(
                        format!(
                            "cannot call '{}' as a function: identifier is shadowed by variable declared at {}",
                            name.bold(),
                            var_info.span
                        ),
                        *span,
                    ))
                }
            }
            None => Err(SemanticError::with_span(
                format!("Function \'{}\' not defined", name.bold()),
                *span,
            )),
        },
    }
}

fn resolve_fun_decoration(
    dec: &mut FunDeclaration,
    variable_map: &mut HashMap<String, VarInfo>,
    block_vars: &mut HashSet<String>,
    labels: &mut HashSet<String>,
    jumps: &mut HashMap<String, Span>,
    name_gen: &mut NameGenerator,
    used_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    let Identifier(name) = &dec.name;

    // shadow is some when replacing an existing entry
    let shadow = variable_map.insert(
        name.clone(),
        VarInfo {
            renamed: name.clone(),
            span: dec.span,
            ext_link: true,
        },
    );
    if let Some(shadow) = shadow
        && !shadow.ext_link
        && block_vars.contains(name)
    {
        // declared in current scope and not externally linked
        return Err(SemanticError::with_span(
            format!(
                "cannot declare function '{}': conflicts with non-function identifier\n{}: {}: previous declaration was here",
                name.bold(),
                shadow.span,
                "note".cyan(),
            ),
            dec.span,
        ));
    }
    block_vars.insert(name.clone());

    let mut inner_map = variable_map.clone();
    let mut inner_block_vars = HashSet::new();
    let original_params: Vec<String> = dec.params.iter().map(|Identifier(name)| name.clone()).collect();

    for param in &mut dec.params {
        resolve_param(param, &mut inner_map, name_gen, &mut inner_block_vars, dec.span)?;
    }

    if let Some(block) = &mut dec.body {
        resolve_block(
            block,
            &mut inner_block_vars,
            &mut inner_map,
            labels,
            jumps,
            name_gen,
            used_vars,
        )?;
    }
    if dec.body.is_some() {
        for (Identifier(param), org_name) in dec.params.iter().zip(original_params) {
            if !used_vars.contains(param) {
                eprintln!(
                    "{}: {}: unused parameter '{}' {}",
                    dec.span,
                    "warning".purple(),
                    org_name.bold(),
                    "[-Wunused-parameter]".purple()
                );
            }
        }
    }
    Ok(())
}

fn resolve_param(
    param: &mut Identifier,
    variable_map: &mut HashMap<String, VarInfo>,
    name_gen: &mut NameGenerator,
    block_vars: &mut HashSet<String>,
    span: Span,
) -> Result<(), SemanticError> {
    let Identifier(name) = param.clone();

    // if the value doesn't exist in block_vars we want to insert or update
    if block_vars.insert(name.clone()) {
        let unique_name = name_gen.next(&name);
        let shadow = variable_map.insert(
            name.clone(),
            VarInfo {
                renamed: unique_name.clone(),
                span,
                ext_link: false,
            },
        );
        if let Some(shadow) = shadow {
            eprintln!(
                "{}: {}: variable {} shadows previous declaration {}",
                span,
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
        *param = Identifier(unique_name);
        Ok(())
    } else {
        let original_def = variable_map.get(&name).unwrap();
        Err(SemanticError::with_span(
            format!(
                "variable {} already declared\n{}: {}: previous decoration was here",
                format!("'{}'", name).bold(),
                original_def.span,
                "note".to_string().cyan(),
            ),
            span,
        ))
    }
}

fn resolve_var_decoration(
    dec: &mut VarDeclaration,
    variable_map: &mut HashMap<String, VarInfo>,
    name_gen: &mut NameGenerator,
    block_vars: &mut HashSet<String>,
    used_vars: &mut HashSet<String>,
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
                ext_link: false,
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
            resolve_exp(init_expr, variable_map, used_vars)?;
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
    statement: &mut SpannedStmt,
    variable_map: &mut HashMap<String, VarInfo>,
    labels: &mut HashSet<String>,
    jumps: &mut HashMap<String, Span>,
    name_gen: &mut NameGenerator,
    used_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    match &mut statement.stmt {
        Stmt::Return(e) => resolve_exp(e, variable_map, used_vars),
        Stmt::Expression(e) => resolve_exp(e, variable_map, used_vars),
        Stmt::Null => Ok(()),
        Stmt::If(e, then_stmt, else_stmt) => {
            resolve_exp(e, variable_map, used_vars)?;
            resolve_statement(then_stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            match else_stmt.as_mut() {
                Some(else_stmt) => resolve_statement(else_stmt, variable_map, labels, jumps, name_gen, used_vars),
                None => Ok(()),
            }
        }
        Stmt::Labeled(label_name, stmt) => {
            if !labels.insert(label_name.0.clone()) {
                return Err(SemanticError {
                    message: format!("Label {label_name:} already defined at {}", statement.span),
                    span: None,
                });
            }
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)
        }
        Stmt::Goto(label_name) => {
            jumps.insert(label_name.0.clone(), statement.span);
            Ok(())
        }
        Stmt::Compound(block) => {
            let mut shadow_map = variable_map.clone();
            Ok(resolve_block(
                block,
                &mut HashSet::new(),
                &mut shadow_map,
                labels,
                jumps,
                name_gen,
                used_vars,
            )?)
        }
        Stmt::While(exp, stmt, _) | Stmt::Switch(exp, stmt, ..) | Stmt::Case(exp, stmt, ..) => {
            resolve_exp(exp, variable_map, used_vars)?;
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)
        }
        Stmt::DoWhile(stmt, exp, _) => {
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            resolve_exp(exp, variable_map, used_vars)?;
            Ok(())
        }
        Stmt::For(init, e1, e2, stmt, _) => {
            let mut shadow_map = variable_map.clone();
            match init {
                ForInit::InitExp(exp) => {
                    if let Some(e) = exp {
                        resolve_exp(e, &shadow_map, used_vars)?;
                    }
                }
                ForInit::InitDecl(dec) => {
                    resolve_var_decoration(dec, &mut shadow_map, name_gen, &mut HashSet::new(), used_vars)?
                }
            }
            if let Some(e1) = e1 {
                resolve_exp(e1, &shadow_map, used_vars)?;
            }
            if let Some(e2) = e2 {
                resolve_exp(e2, &shadow_map, used_vars)?;
            }
            resolve_statement(stmt, &mut shadow_map, labels, jumps, name_gen, used_vars)?;
            Ok(())
        }
        Stmt::Break(..) | Stmt::Continue(..) => Ok(()),
        Stmt::Default(stmt, ..) => resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars),
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
                BinOp::Assignment | BinOp::CompoundAssignment | BinOp::Conditional => {
                    unreachable!("only used for parsing")
                }
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
    Switch(u64),
}

struct LabelTracker {
    cur_label: Vec<LabelTag>,
    next_loop_label: u64,
    next_switch_label: u64,
    switch_to_cases: HashMap<u64, HashSet<SwitchIntType>>, // switch -> case e
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
            LabelTag::Loop(..) => true,
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
            LabelTag::Loop(..) => false,
        }) {
            let case_exp = SwitchIntType::Int(c);
            if self.switch_to_cases.get_mut(label).unwrap().insert(case_exp.clone()) {
                Ok(case_exp.label_str(*label))
            } else {
                Err(SemanticError::with_span(
                    format!("Duplicate Case '{}' found in switch.", format!("{}", c).bold()),
                    *span,
                ))
            }
        } else {
            Err(SemanticError::with_span("Case outside of switch".to_string(), *span))
        }
    }

    fn get_switch_default(&mut self, span: &Span) -> Result<String, SemanticError> {
        // get the active switch id
        if let Some(LabelTag::Switch(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => true,
            LabelTag::Loop(..) => false,
        }) {
            let default_case = SwitchIntType::Default;
            if self
                .switch_to_cases
                .get_mut(label)
                .unwrap()
                .insert(default_case.clone())
            {
                Ok(default_case.label_str(*label))
            } else {
                Err(SemanticError::with_span(
                    format!("Duplicate '{}' found in switch.", "default".bold()),
                    *span,
                ))
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

fn label_statement(stmt: &mut SpannedStmt, label_tracker: &mut LabelTracker) -> Result<(), SemanticError> {
    match &mut stmt.stmt {
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
        Stmt::Labeled(_, stmt) => label_statement(stmt, label_tracker),
        Stmt::Compound(block) => {
            for bi in block.iter_mut() {
                match bi {
                    BlockItem::Statement(stmt) => {
                        label_statement(stmt, label_tracker)?;
                    }
                    BlockItem::Declaration(_) => {}
                }
            }
            Ok(())
        }
        Stmt::Break(Identifier(s)) => {
            if let Some(break_label) = label_tracker.get_break_label() {
                *s = break_label;
                Ok(())
            } else {
                Err(SemanticError::with_span(
                    "Break statement outside loop".to_string(),
                    stmt.span,
                ))
            }
        }
        Stmt::Continue(Identifier(s)) => {
            if let Some(label) = label_tracker.get_continue_label() {
                *s = label;
                Ok(())
            } else {
                Err(SemanticError::with_span(
                    "Continue statement outside loop".to_string(),
                    stmt.span,
                ))
            }
        }
        Stmt::While(_, body, loop_num) | Stmt::DoWhile(body, _, loop_num) | Stmt::For(_, _, _, body, loop_num) => {
            *loop_num = label_tracker.next_loop_label();
            let result = label_statement(body, label_tracker);
            label_tracker.pop();
            result
        }
        Stmt::Switch(_, stmt, switch_num, cases) => {
            *switch_num = label_tracker.next_switch_label();
            let result = label_statement(stmt, label_tracker);
            *cases = label_tracker.pop_switch(*switch_num);
            result
        }
        Stmt::Case(exp, s, Identifier(label)) => {
            if let Some(exp) = eval_constant_expr(exp) {
                *label = label_tracker.get_switch_case(exp, &stmt.span)?;
                label_statement(s, label_tracker)
            } else {
                Err(SemanticError::with_span(
                    "Expression is not an integer constant expression".to_string(),
                    stmt.span,
                ))
            }
        }
        Stmt::Default(s, Identifier(label)) => {
            *label = label_tracker.get_switch_default(&stmt.span)?;
            label_statement(s, label_tracker)
        }
    }
}

fn label_block(block: &mut Block, label_tracker: &mut LabelTracker) -> Result<(), SemanticError> {
    for bi in block.iter_mut() {
        if let BlockItem::Statement(stmt) = bi {
            label_statement(stmt, label_tracker)?;
        }
    }
    Ok(())
}

fn resolve_block(
    block: &mut Block,
    block_vars: &mut HashSet<String>,
    variable_map: &mut HashMap<String, VarInfo>,
    labels: &mut HashSet<String>,
    jumps: &mut HashMap<String, Span>,
    name_gen: &mut NameGenerator,
    used_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    for bi in block.iter_mut() {
        match bi {
            BlockItem::Statement(stmt) => {
                resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            }
            BlockItem::Declaration(d) => match d {
                Declaration::VarDeclaration(var) => {
                    resolve_var_decoration(var, variable_map, name_gen, block_vars, used_vars)?;
                }
                Declaration::FunDeclaration(fun) => {
                    if fun.body.is_some() {
                        return Err(SemanticError::with_span(
                            "function definition is not allowed here".to_string(),
                            fun.span,
                        ));
                    }
                    resolve_fun_decoration(fun, variable_map, block_vars, labels, jumps, name_gen, used_vars)?;
                }
            },
        }
    }
    Ok(())
}

pub fn resolve_program(program: &mut Program) -> Result<NameGenerator, SemanticError> {
    let mut variable_map = HashMap::new();
    let mut name_gen = NameGenerator::new();
    let mut undefined_jumps = Vec::new();

    let mut label_tracker = LabelTracker::new();
    for function in program.functions.iter_mut() {
        let mut used_vars = HashSet::new();
        let mut labels = HashSet::new();
        let mut jumps: HashMap<String, Span> = HashMap::new();
        if let Some(body) = &mut function.body {
            label_block(body, &mut label_tracker)?;
        }
        let mut block_vars = HashSet::new();
        resolve_fun_decoration(
            function,
            &mut variable_map,
            &mut block_vars,
            &mut labels,
            &mut jumps,
            &mut name_gen,
            &mut used_vars,
        )?;

        // Find jumps to non-existent labels and report with spans
        for (label, span) in jumps.iter() {
            if !labels.contains(label) {
                undefined_jumps.push((label.clone(), *span));
            }
        }
    }

    if undefined_jumps.is_empty() {
        typecheck_program(program)?;
        Ok(name_gen)
    } else {
        let (label, span) = &undefined_jumps[0];
        Err(SemanticError::with_span(
            format!("Jump to undefined label '{}'", label.bold()),
            *span,
        ))
    }
}

pub fn typecheck_program(program: &Program) -> Result<(), SemanticError> {
    let mut symbols = SymbolTable::new();

    for decl in &program.functions {
        typecheck_function_declaration(decl, &mut symbols)?;
    }

    Ok(())
}
