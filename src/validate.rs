use crate::lexer::Span;
use crate::parser::{
    BinOp, Block, BlockItem, Declaration, Expr, ForInit, FunDeclaration, Identifier, Program, SpannedStmt, Stmt,
    StorageClass, SwitchIntType, UnaryOp, VarDeclaration,
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

#[derive(Clone)]
pub enum InitialValue {
    Tentative,
    Initial(i32),
    NoInitializer,
}

pub enum SymbolType {
    Int(InitialValue),
    FunType { param_cnt: usize, defined: bool },
}

pub struct Symbol {
    pub(crate) symbol_type: SymbolType,
    pub(crate) global: bool,
    span: Span,
}

pub type SymbolTable = HashMap<String, Symbol>;

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

/// Validates a local (block-scope) variable declaration and updates the symbol table.
///
/// Handles three cases based on storage class:
///
/// **extern**: References an external variable
/// - Cannot have an initializer
/// - If not already in symbol table, adds with `NoInitializer` and `global: true`
/// - If already exists, must be a variable (not a function)
///
/// **static**: Local variable with static storage duration
/// - Initializer must be a constant expression (or defaults to 0)
/// - Added to symbol table with `global: false`
///
/// **none (automatic)**: Regular local variable
/// - Added to symbol table, initializer typechecked if present
fn typecheck_local_variable_declaration(decl: &VarDeclaration, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    if decl.storage_class == Some(StorageClass::Extern) {
        if decl.init.is_some() {
            return Err(SemanticError::with_span(
                format!("'{}' has both 'extern' and initializer", decl.name.0.bold()),
                decl.span,
            ));
        }
        if let Some(old_dec) = symbols.get(&decl.name.0) {
            if !matches!(old_dec.symbol_type, SymbolType::Int(_)) {
                return Err(SemanticError::with_span(
                    format!(
                        "'{}' redeclared as different kind of symbol\n{}: {}: previous declaration was here",
                        decl.name.0.bold(),
                        old_dec.span,
                        "note".cyan()
                    ),
                    decl.span,
                ));
            }
        } else {
            symbols.insert(
                decl.name.0.clone(),
                Symbol {
                    symbol_type: SymbolType::Int(InitialValue::NoInitializer),
                    global: true,
                    span: decl.span,
                },
            );
        }
    } else if decl.storage_class == Some(StorageClass::Static) {
        let initial_value = if let Some(expr) = decl.init.as_ref() {
            match eval_constant_expr(expr) {
                None => {
                    return Err(SemanticError::with_span(
                        format!(
                            "initializer for static variable '{}' is not a constant expression",
                            decl.name.0.bold()
                        ),
                        decl.span,
                    ));
                }
                Some(c) => InitialValue::Initial(c),
            }
        } else {
            InitialValue::Initial(0)
        };
        symbols.insert(
            decl.name.0.clone(),
            Symbol {
                symbol_type: SymbolType::Int(initial_value),
                global: false,
                span: decl.span,
            },
        );
    } else {
        // Automatic storage (no storage class specifier)
        symbols.insert(
            decl.name.0.clone(),
            Symbol {
                symbol_type: SymbolType::Int(InitialValue::NoInitializer),
                global: false,
                span: decl.span,
            },
        );
        if let Some(init) = &decl.init {
            typecheck_exp(init, symbols)?;
        }
    }
    Ok(())
}

/// Validates a file-scope variable declaration and updates the symbol table.
///
/// Enforces the following rules:
/// - Initializers must be constant expressions
/// - A function cannot be redeclared as a variable
/// - Variable linkage (static vs non-static) must be consistent across declarations
/// - Only one definition (with initializer) is allowed per variable
///
/// Initial value resolution:
/// - Explicit constant initializer → `Initial(value)`
/// - No initializer + `extern` → `NoInitializer`
/// - No initializer + not `extern` → `Tentative`
///
/// The span stored is the location of the definition, or most recent declaration if never defined.
fn typecheck_file_variable_declaration(decl: &VarDeclaration, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    let mut initial_value = if let Some(expr) = &decl.init {
        match eval_constant_expr(expr) {
            None => {
                return Err(SemanticError::with_span(
                    format!("initializer for '{}' is not a constant expression", decl.name.0.bold()),
                    decl.span,
                ));
            }
            Some(c) => InitialValue::Initial(c),
        }
    } else if decl.storage_class == Some(StorageClass::Extern) {
        InitialValue::NoInitializer
    } else {
        InitialValue::Tentative
    };

    let mut global = decl.storage_class != Some(StorageClass::Static);
    let mut saved_span = decl.span;

    if let Some(old_dec) = symbols.get(&decl.name.0) {
        let old_init = match &old_dec.symbol_type {
            SymbolType::FunType { .. } => {
                return Err(SemanticError::with_span(
                    format!(
                        "'{}' redeclared as different kind of symbol\n{}: {}: previous declaration was here",
                        decl.name.0.bold(),
                        old_dec.span,
                        "note".cyan()
                    ),
                    decl.span,
                ));
            }
            SymbolType::Int(old_init) => old_init.clone(),
        };

        if decl.storage_class == Some(StorageClass::Extern) {
            global = old_dec.global;
        } else if old_dec.global != global {
            return Err(SemanticError::with_span(
                format!(
                    "conflicting linkage for '{}'\n{}: {}: previous declaration was here",
                    decl.name.0.bold(),
                    old_dec.span,
                    "note".cyan()
                ),
                decl.span,
            ));
        }

        if matches!(old_init, InitialValue::Initial(_)) {
            if matches!(initial_value, InitialValue::Initial(_)) {
                return Err(SemanticError::with_span(
                    format!(
                        "redefinition of '{}'\n{}: {}: previous definition was here",
                        decl.name.0.bold(),
                        old_dec.span,
                        "note".cyan()
                    ),
                    decl.span,
                ));
            } else {
                initial_value = old_init;
                saved_span = old_dec.span;
            }
        } else if !matches!(initial_value, InitialValue::Initial(_)) && matches!(old_init, InitialValue::Tentative) {
            initial_value = InitialValue::Tentative;
        }
    }

    symbols.insert(
        decl.name.0.clone(),
        Symbol {
            symbol_type: SymbolType::Int(initial_value),
            global,
            span: saved_span,
        },
    );
    Ok(())
}

fn typecheck_exp(exp: &Expr, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    match exp {
        Expr::Constant(_) => Ok(()),
        Expr::Var(Identifier(name), span) => {
            if let Some(expected) = symbols.get(name) {
                match expected.symbol_type {
                    SymbolType::FunType { .. } => Err(SemanticError::with_span(
                        format!("cannot use function '{}' as a variable", name.bold()),
                        *span,
                    )),
                    SymbolType::Int(_) => Ok(()),
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
                    SymbolType::FunType { param_cnt, defined: _ } => {
                        if param_cnt != args.len() {
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
                    SymbolType::Int(_) => Err(SemanticError::with_span(
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

/// Validates a function declaration and updates the symbol table.
///
/// Enforces the following rules:
/// - Parameter count must match any prior declaration
/// - A function can only be defined once (have a body)
/// - A `static` declaration cannot follow a non-static declaration
/// - Global linkage is inherited from the first declaration
///
/// The span stored in the symbol table is the location of the function's
/// definition (body), or the most recent declaration if never defined.
fn typecheck_function_declaration(decl: &FunDeclaration, symbols: &mut SymbolTable) -> Result<(), SemanticError> {
    let param_cnt = decl.params.len();
    let mut global = decl.storage_class != Some(StorageClass::Static);
    let mut defined = decl.body.is_some();
    let mut saved_span = decl.span;
    if let Some(old_dec) = symbols.get(&decl.name.0) {
        match old_dec.symbol_type {
            SymbolType::FunType {
                param_cnt: old_param_cnt,
                defined: old_defined,
            } => {
                if old_param_cnt != param_cnt {
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
                if old_defined {
                    if decl.body.is_some() {
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
                    saved_span = old_dec.span;
                }
                if old_dec.global && decl.storage_class == Some(StorageClass::Static) {
                    return Err(SemanticError::with_span(
                        "Static function declaration follows non-static".to_string(),
                        decl.span,
                    ));
                }
                global = old_dec.global;
                defined = defined || old_defined;
            }
            SymbolType::Int(_) => {
                return Err(SemanticError::with_span(
                    format!(
                        "redeclaration of '{}' as a function\n{}: {}: previous declaration was here",
                        decl.name.0.bold(),
                        old_dec.span,
                        "note".cyan()
                    ),
                    decl.span,
                ));
            }
        }
    }

    symbols.insert(
        decl.name.0.clone(),
        Symbol {
            symbol_type: SymbolType::FunType { param_cnt, defined },
            global,
            span: saved_span,
        },
    );

    if let Some(body) = decl.body.as_ref() {
        for Identifier(name) in &decl.params {
            symbols.insert(
                name.clone(),
                Symbol {
                    symbol_type: SymbolType::Int(InitialValue::NoInitializer),
                    global: false,
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
                typecheck_local_variable_declaration(var, symbols)?;
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
                ForInit::InitDecl(decl) => {
                    if decl.storage_class.is_some() {
                        return Err(SemanticError::with_span(
                            "declaration in for loop initializer cannot have storage class".to_string(),
                            decl.span,
                        ));
                    }
                    typecheck_local_variable_declaration(decl, symbols)?
                }
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

/// Registers a file-scope variable declaration in the variable map.
///
/// File-scope variables:
/// - Keep their original name (no renaming needed - no shadowing at file scope)
/// - Are marked with `ext_link: true` (external linkage by default)
///
/// This is the resolve pass counterpart to `typecheck_file_variable_declaration`.
fn resolve_file_var_declaration(dec: &mut VarDeclaration, variable_map: &mut HashMap<String, VarInfo>) {
    let Identifier(name) = &dec.name;
    variable_map.insert(
        name.clone(),
        VarInfo {
            renamed: name.clone(),
            span: dec.span,
            ext_link: true,
        },
    );
}

/// Resolves a local (block-scope) variable declaration.
///
/// Handles three cases based on storage class:
///
/// **Conflict detection**: If a variable with the same name exists in the current
/// block scope, it's an error unless both have external linkage (extern).
///
/// **extern**: References an external variable
/// - Keeps original name (no renaming)
/// - Marked with `ext_link: true`
///
/// **static or automatic**: Local variable
/// - Gets a unique renamed identifier (e.g., `x` → `x.1`)
/// - Marked with `ext_link: false`
/// - Emits `-Wshadow` warning if shadowing an outer scope variable
/// - Initializer expression is resolved if present
///
/// This is the resolve pass counterpart to `typecheck_local_variable_declaration`.
fn resolve_local_var_decoration(
    dec: &mut VarDeclaration,
    variable_map: &mut HashMap<String, VarInfo>,
    name_gen: &mut NameGenerator,
    block_vars: &mut HashSet<String>,
    used_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    let Identifier(name) = &dec.name.clone();

    if let Some(original_def) = variable_map.get(name)
        && block_vars.contains(name)
        && !(original_def.ext_link && (dec.storage_class == Some(StorageClass::Extern)))
    {
        return Err(SemanticError::with_span(
            format!(
                "Conflicting local declarations for variable {}\n{}: {}: previous decoration was here",
                format!("'{}'", name).bold(),
                original_def.span,
                "note".to_string().cyan(),
            ),
            dec.span,
        ));
    }
    if dec.storage_class == Some(StorageClass::Extern) {
        variable_map.insert(
            name.clone(),
            VarInfo {
                renamed: name.clone(),
                span: dec.span,
                ext_link: true,
            },
        );
    } else {
        let unique_name = name_gen.next(name);
        dec.name = Identifier(unique_name.clone());
        if let Some(shadow) = variable_map.insert(
            name.clone(),
            VarInfo {
                renamed: unique_name.clone(),
                span: dec.span,
                ext_link: false,
            },
        ) {
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
        if let Some(init_expr) = &mut dec.init {
            resolve_exp(init_expr, variable_map, used_vars)?;
        }
    }
    block_vars.insert(name.clone());
    Ok(())
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
                    resolve_local_var_decoration(dec, &mut shadow_map, name_gen, &mut HashSet::new(), used_vars)?
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
                    resolve_local_var_decoration(var, variable_map, name_gen, block_vars, used_vars)?;
                }
                Declaration::FunDeclaration(fun) => {
                    if fun.body.is_some() {
                        return Err(SemanticError::with_span(
                            "function definition is not allowed here".to_string(),
                            fun.span,
                        ));
                    }
                    if fun.storage_class == Some(StorageClass::Static) {
                        return Err(SemanticError::with_span(
                            "static function decloration not allowed in block scope".to_string(),
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

pub fn resolve_program(program: &mut Program) -> Result<(NameGenerator, SymbolTable), SemanticError> {
    let mut variable_map = HashMap::new();
    let mut name_gen = NameGenerator::new();
    let mut undefined_jumps = Vec::new();

    let mut label_tracker = LabelTracker::new();
    for decl in program.declarations.iter_mut() {
        match decl {
            Declaration::VarDeclaration(var) => resolve_file_var_declaration(var, &mut variable_map),
            Declaration::FunDeclaration(function) => {
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
        }
    }

    if undefined_jumps.is_empty() {
        let symbols = typecheck_program(program)?;
        Ok((name_gen, symbols))
    } else {
        let (label, span) = &undefined_jumps[0];
        Err(SemanticError::with_span(
            format!("Jump to undefined label '{}'", label.bold()),
            *span,
        ))
    }
}

pub fn typecheck_program(program: &Program) -> Result<SymbolTable, SemanticError> {
    let mut symbols = SymbolTable::new();

    for decl in &program.declarations {
        match decl {
            Declaration::VarDeclaration(dec) => typecheck_file_variable_declaration(dec, &mut symbols)?,
            Declaration::FunDeclaration(func) => typecheck_function_declaration(func, &mut symbols)?,
        };
    }
    Ok(symbols)
}
