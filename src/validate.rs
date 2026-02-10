use crate::lexer::Span;
use crate::parser::{BinOp, Block as ParserBlock, BlockItem as ParserBlockItem, Declaration, Expr as ParserExpr, ForInit as ParserForInit, FunDeclaration, Identifier, Program, SpannedStmt, Stmt as ParserStmt, StorageClass, SwitchIntType, UnaryOp, VarDeclaration, Const, Type, AssignOp, IncDec};
use colored::*;
use std::collections::{HashMap, HashSet};
use std::fmt;

pub struct SemanticError {
    message: String,
    span: Option<Span>,
}

#[derive(Clone)]
pub struct TypedExpression {
    pub exp_type: Type,
    pub exp: Expr
}

#[derive(Clone)]
pub enum Expr {
    Const(Const),
    Var(Identifier),
    Cast(Type, Box<TypedExpression>),
    Unary(UnaryOp, Box<TypedExpression>),
    Binary(BinOp, Box<TypedExpression>, Box<TypedExpression>),
    Assignment(Box<TypedExpression>, Box<TypedExpression>),
    CompoundAssignment(AssignOp, Box<TypedExpression>, Box<TypedExpression>, Type),
    PostFixOp(IncDec, Box<TypedExpression>),
    PreFixOp(IncDec, Box<TypedExpression>),
    Conditional(Box<TypedExpression>, Box<TypedExpression>, Box<TypedExpression>),
    FunctionCall(Identifier, Vec<TypedExpression>),
}

pub enum Stmt {
    Return(TypedExpression),
    Expression(TypedExpression),
    If(TypedExpression, Box<Stmt>, Option<Box<Stmt>>),
    Goto(Identifier),
    Labeled(Identifier, Box<Stmt>),
    Compound(Block),
    Break(Identifier),
    Continue(Identifier),
    While(TypedExpression, Box<Stmt>, u64),
    DoWhile(Box<Stmt>, TypedExpression, u64),
    For(ForInit, Option<TypedExpression>, Option<TypedExpression>, Box<Stmt>, u64),
    Switch(TypedExpression, Box<Stmt>, u64, HashSet<SwitchIntType>),
    Case(TypedExpression, Box<Stmt>, Identifier),
    Default(Box<Stmt>, Identifier),
    Null,
}

pub type Block = Vec<BlockItem>;

pub enum BlockItem {
    Statement(Stmt),
    Declaration(TypedDeclaration),
}

pub enum ForInit {
    InitDecl(TypedVarDeclaration),
    InitExp(Option<TypedExpression>),
}

impl Expr {
    pub fn with_type(self, exp_type: Type) -> TypedExpression {
        TypedExpression { exp_type, exp: self }
    }
}

pub struct TypedFunction {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<Block>,
    pub fun_type: Type,
    pub global: bool,
}

pub enum TypedDeclaration {
    Function(TypedFunction),
    Variable(TypedVarDeclaration),
}

pub struct TypedProgram {
    pub declarations: Vec<TypedDeclaration>,
}

pub struct TypedVarDeclaration {
    pub name: Identifier,
    pub init: Option<TypedExpression>,
    pub var_type: Type,
    pub storage_class: Option<StorageClass>,
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

#[derive(Clone, Copy)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInt),
    NoInitializer,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StaticInt {
    IntInit(i32),
    LongInit(i64)
}

impl StaticInt {
    fn get_type(&self) -> Type {
        match self {
            StaticInt::IntInit(_) => Type::Int,
            StaticInt::LongInit(_) => Type::Long,
        }
    }

    fn to_const(&self) -> Const {
        match self {
            StaticInt::IntInit(i) => Const::ConstInt(*i),
            StaticInt::LongInit(l) => Const::ConstLong(*l),
        }
    }

    pub fn get_common(self, other: Self) -> (Self, Self) {
        let common_type = get_common_type(&self.get_type(), &other.get_type());
        let left = convert_to_type(self, &common_type);
        let right = convert_to_type(other, &common_type);
        (left, right)
    }

    fn wrapping_add(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a.wrapping_add(b)),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a.wrapping_add(b)),
            _ => unreachable!(),
        }
    }

    fn wrapping_sub(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a.wrapping_sub(b)),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a.wrapping_sub(b)),
            _ => unreachable!(),
        }
    }

    fn wrapping_mul(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a.wrapping_mul(b)),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a.wrapping_mul(b)),
            _ => unreachable!(),
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            StaticInt::IntInit(v) => *v == 0,
            StaticInt::LongInit(v) => *v == 0,
        }
    }

    fn div(self, other: Self) -> Option<Self> {
        if other.is_zero() {
            return None;
        }
        let (left, right) = self.get_common(other);
        Some(match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a / b),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a / b),
            _ => unreachable!(),
        })
    }

    fn rem(self, other: Self) -> Option<Self> {
        if other.is_zero() {
            return None;
        }
        let (left, right) = self.get_common(other);
        Some(match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a % b),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a % b),
            _ => unreachable!(),
        })
    }

    fn bitwise_and(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a & b),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a & b),
            _ => unreachable!(),
        }
    }

    fn bitwise_or(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a | b),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a | b),
            _ => unreachable!(),
        }
    }

    fn bitwise_xor(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(a ^ b),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::LongInit(a ^ b),
            _ => unreachable!(),
        }
    }

    /// Left shift. Shift amount is masked (& 31 for int, & 63 for long) to prevent
    /// undefined behavior, matching x86 hardware semantics.
    fn shl(self, other: Self) -> Self {
        let shift_amount = match other {
            StaticInt::IntInit(v) => v as u32,
            StaticInt::LongInit(v) => v as u32,
        };

        match self {
            StaticInt::IntInit(a) => StaticInt::IntInit(a << (shift_amount & 31)),
            StaticInt::LongInit(a) => StaticInt::LongInit(a << (shift_amount & 63)),
        }
    }

    /// Right shift (arithmetic). Shift amount is masked (& 31 for int, & 63 for long)
    /// to prevent undefined behavior, matching x86 hardware semantics.
    fn shr(self, other: Self) -> Self {
        let shift_amount = match other {
            StaticInt::IntInit(v) => v as u32,
            StaticInt::LongInit(v) => v as u32,
        };

        match self {
            StaticInt::IntInit(a) => StaticInt::IntInit(a >> (shift_amount & 31)),
            StaticInt::LongInit(a) => StaticInt::LongInit(a >> (shift_amount & 63)),
        }
    }

    fn eq(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(if a == b { 1 } else { 0 }),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::IntInit(if a == b { 1 } else { 0 }),
            _ => unreachable!(),
        }
    }

    fn ne(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(if a == b { 0 } else { 1 }),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::IntInit(if a == b { 0 } else { 1 }),
            _ => unreachable!(),
        }
    }

    fn lt(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(if a < b { 1 } else { 0 }),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::IntInit(if a < b { 1 } else { 0 }),
            _ => unreachable!(),
        }
    }

    fn le(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(if a <= b { 1 } else { 0 }),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::IntInit(if a <= b { 1 } else { 0 }),
            _ => unreachable!(),
        }
    }

    fn gt(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(if a > b { 1 } else { 0 }),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::IntInit(if a > b { 1 } else { 0 }),
            _ => unreachable!(),
        }
    }

    fn ge(self, other: Self) -> Self {
        let (left, right) = self.get_common(other);
        match (left, right) {
            (StaticInt::IntInit(a), StaticInt::IntInit(b)) => StaticInt::IntInit(if a >= b { 1 } else { 0 }),
            (StaticInt::LongInit(a), StaticInt::LongInit(b)) => StaticInt::IntInit(if a >= b { 1 } else { 0 }),
            _ => unreachable!(),
        }
    }

    fn and(self, other: Self) -> Self {
        StaticInt::IntInit(if !self.is_zero() && !other.is_zero() { 1 } else { 0 })
    }

    fn or(self, other: Self) -> Self {
        StaticInt::IntInit(if !self.is_zero() || !other.is_zero() { 1 } else { 0 })
    }
}

/// Symbol table entry for variables and functions.
///
/// # Fields
///
/// - `symbol_type`: The type of the symbol (Int, Long, or FunType)
///   - For functions, `FunType.defined` tracks if the function has a body
///
/// - `global`: Linkage scope
///   - `true` = external linkage (visible across translation units)
///   - `false` = internal linkage (static) or no linkage (local variables)
///
/// - `val`: Initial value for **variables only**
///   - `Initial(value)` = has a constant initializer (definition)
///   - `Tentative` = file-scope without initializer (tentative definition)
///   - `NoInitializer` = extern declaration or local variable without initializer
///   - Unused for functions
///
/// - `span`: Source location of the definition, or most recent declaration if never defined
pub struct Symbol {
    pub symbol_type: Type,
    pub global: bool,
    pub val: InitialValue,
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
/// - If not already in symbol table, adds with `NoInitializer`, `global: true`
/// - If already exists, must be a variable (not a function)
/// - `defined: false` (no local storage allocated)
///
/// **static**: Local variable with static storage duration
/// - Initializer evaluated as constant expression (not typechecked) and converted to variable's type
/// - If no initializer, defaults to 0 with appropriate type (int → IntInit(0), long → LongInit(0))
/// - Added to symbol table with `global: false`
/// - `defined: true` (allocates static storage)
///
/// **none (automatic)**: Regular local variable with automatic storage
/// - Initializer is typechecked and converted to variable's type
/// - Added to symbol table with `global: false`
/// - `defined: true` (allocates stack storage)
///
/// The `defined` field is set to `decl.storage_class != Some(StorageClass::Extern)`.
fn typecheck_local_variable_declaration(decl: &VarDeclaration, symbols: &mut SymbolTable) -> Result<TypedVarDeclaration, SemanticError> {
    if decl.storage_class == Some(StorageClass::Extern) {
        if decl.init.is_some() {
            return Err(SemanticError::with_span(
                format!("'{}' has both 'extern' and initializer", decl.name.0.bold()),
                decl.span,
            ));
        }
        if let Some(old_dec) = symbols.get(&decl.name.0) {
            if old_dec.symbol_type != decl.var_type {
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
                    symbol_type: decl.var_type.clone(),
                    global: true,
                    val: InitialValue::NoInitializer,
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
                Some(c) => InitialValue::Initial(convert_to_type(c, &decl.var_type)),
            }
        } else {
            let zero = match decl.var_type {
                Type::Int => StaticInt::IntInit(0),
                Type::Long => StaticInt::LongInit(0),
                _ => unreachable!("static variable must be int or long"),
            };
            InitialValue::Initial(zero)
        };
        symbols.insert(
            decl.name.0.clone(),
            Symbol {
                symbol_type: decl.var_type.clone(),
                global: false,
                val: initial_value,
                span: decl.span,
            },
        );
    } else {
        // Automatic storage (no storage class specifier)
        symbols.insert(
            decl.name.0.clone(),
            Symbol {
                symbol_type: decl.var_type.clone(),
                global: false,
                val: InitialValue::NoInitializer,
                span: decl.span,
            },
        );
    }
    // Convert init expression to typed form (typecheck happens once here for all branches)
    let typed_init = decl.init.as_ref()
        .map(|e| typecheck_exp(e.clone(), symbols)) // TODO: expensive clone of ParserExpr - refactor typecheck_exp to take &ParserExpr
        .transpose()?.map(|exp| convert_to(exp, &decl.var_type));

    Ok(TypedVarDeclaration {
        name: decl.name.clone(),
        init: typed_init,
        var_type: decl.var_type.clone(),
        storage_class: decl.storage_class.clone(),
    })
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
fn typecheck_file_variable_declaration(decl: &VarDeclaration, symbols: &mut SymbolTable) -> Result<TypedVarDeclaration, SemanticError> {
    let mut initial_value = if let Some(expr) = &decl.init {
        match eval_constant_expr(expr) {
            None => {
                return Err(SemanticError::with_span(
                    format!("initializer for '{}' is not a constant expression", decl.name.0.bold()),
                    decl.span,
                ));
            }
            Some(c) => InitialValue::Initial(convert_to_type(c, &decl.var_type)),
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
            Type::FunType { .. } => {
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
            Type::Int | Type::Long => {
                if old_dec.symbol_type != decl.var_type {
                    return Err(SemanticError::with_span(
                        format!(
                            "conflicting types for '{}'\n{}: {}: previous declaration was here",
                            decl.name.0.bold(),
                            old_dec.span,
                            "note".cyan()
                        ),
                        decl.span,
                    ));
                }
                &old_dec.val
            },
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
                initial_value = *old_init;
                saved_span = old_dec.span;
            }
        } else if !matches!(initial_value, InitialValue::Initial(_)) && matches!(old_init, InitialValue::Tentative) {
            initial_value = InitialValue::Tentative;
        }
    }

    symbols.insert(
        decl.name.0.clone(),
        Symbol {
            symbol_type: decl.var_type.clone(),
            global,
            val: initial_value,
            span: saved_span,
        },
    );

    let typed_init = match initial_value {
        InitialValue::Initial(static_val) => {
            let const_val = static_val.to_const();
            Some(TypedExpression {
                exp_type: decl.var_type.clone(),
                exp: Expr::Const(const_val),
            })
        }
        InitialValue::NoInitializer | InitialValue::Tentative => None,
    };

    Ok(TypedVarDeclaration {
        name: decl.name.clone(),
        init: typed_init,
        var_type: decl.var_type.clone(),
        storage_class: decl.storage_class.clone(),
    })
}

pub(crate) fn get_common_type(t1: &Type, t2: &Type) -> Type {
    if t1 == t2 {
        t1.clone()
    } else {
        Type::Long
    }
}

fn convert_to(exp: TypedExpression, t: &Type) -> TypedExpression {
    if exp.exp_type == *t {
        exp
    } else {
        let cast_exp = Expr::Cast(t.clone(), Box::new(exp));
        cast_exp.with_type(t.clone())
    }
}

fn typecheck_exp(exp: ParserExpr, symbols: &mut SymbolTable) -> Result<TypedExpression, SemanticError> {
    match exp {
        ParserExpr::Var(Identifier(name), span) => {
            let v_type = &symbols.get(&name).expect("Undefined variable in typechecking").symbol_type;
            match v_type {
                Type::FunType {..} => Err(SemanticError::with_span(
                    format!("cannot use function '{}' as a variable", name.bold()),
                    span,
                )),
                _ => {
                    let typed_var = Expr::Var(Identifier(name));
                    Ok(typed_var.with_type(v_type.clone()))
                }
            }
        }
        ParserExpr::Constant(c) => match c {
            Const::ConstInt(_) => Ok(TypedExpression {exp_type: Type::Int, exp: Expr::Const(c)}),
            Const::ConstLong(_) => Ok(TypedExpression {exp_type: Type::Long, exp: Expr::Const(c)}),
        }
        ParserExpr::Cast(t, inner) => {
            let typed_inner = typecheck_exp(*inner, symbols)?;
            let cast_exp = Expr::Cast(t.clone(), Box::new(typed_inner));
            Ok(cast_exp.with_type(t))
        }
        ParserExpr::Unary(op, inner) => {
            let typed_inner = typecheck_exp(*inner, symbols)?;
            let inner_type = typed_inner.exp_type.clone();
            let unary_exp = Expr::Unary(op, Box::new(typed_inner));
            match op {
                UnaryOp::Not => Ok(unary_exp.with_type(Type::Int)),
                _ => Ok(unary_exp.with_type(inner_type)),
            }
        }
        ParserExpr::PreFixOp(incdec, inner, _) => {
            let typed_inner = typecheck_exp(*inner, symbols)?;
            let inner_type = typed_inner.exp_type.clone();
            Ok(Expr::PreFixOp(incdec, Box::new(typed_inner)).with_type(inner_type))
        }
        ParserExpr::PostFixOp(incdec, inner, _) => {
            let typed_inner = typecheck_exp(*inner, symbols)?;
            let inner_type = typed_inner.exp_type.clone();
            Ok(Expr::PostFixOp(incdec, Box::new(typed_inner)).with_type(inner_type))
        }
        ParserExpr::Binary(op, lhs, rhs) => {
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            let typed_rhs = typecheck_exp(*rhs, symbols)?;
            if matches!(op, BinOp::And | BinOp::Or) {
                let binary_exp = Expr::Binary(op, Box::new(typed_lhs), Box::new(typed_rhs));
                return Ok(binary_exp.with_type(Type::Int))
            };

            // Bitshift: result type is the type of left operand (not common type)
            if matches!(op, BinOp::BitwiseLeftShift | BinOp::BitwiseRightShift) {
                let result_type = typed_lhs.exp_type.clone();
                let binary_exp = Expr::Binary(op, Box::new(typed_lhs), Box::new(typed_rhs));
                return Ok(binary_exp.with_type(result_type));
            }

            let common_type = get_common_type(&typed_lhs.exp_type, &typed_rhs.exp_type);
            let converted_lhs = convert_to(typed_lhs, &common_type);
            let converted_rhs = convert_to(typed_rhs, &common_type);
            let binary_exp = Expr::Binary(op, Box::new(converted_lhs), Box::new(converted_rhs));
            let result_type = match op {
                BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide | BinOp::Remainder
                | BinOp::BitwiseAnd | BinOp::BitwiseOr | BinOp::BitwiseXOr => common_type,
                BinOp::Equal | BinOp::NotEqual | BinOp::LessThan | BinOp::LessOrEqual | BinOp::GreaterThan | BinOp::GreaterOrEqual => Type::Int,
                _ => unreachable!(),
            };
            Ok(binary_exp.with_type(result_type))
        }
        ParserExpr::Assignment(lhs, rhs, _) => {
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            let typed_rhs = typecheck_exp(*rhs, symbols)?;
            let left_type = typed_lhs.exp_type.clone();
            let converted_rhs = convert_to(typed_rhs, &left_type);
            let assign_exp = Expr::Assignment(Box::new(typed_lhs), Box::new(converted_rhs));
            Ok(assign_exp.with_type(left_type))
        }
        ParserExpr::Conditional(condition, then_exp, else_exp) => {
            let typed_condition = typecheck_exp(*condition, symbols)?;
            let typed_then_exp = typecheck_exp(*then_exp, symbols)?;
            let typed_else_exp = typecheck_exp(*else_exp, symbols)?;
            let common_type = get_common_type(&typed_then_exp.exp_type, &typed_else_exp.exp_type);
            let converted_then = convert_to(typed_then_exp, &common_type);
            let converted_else = convert_to(typed_else_exp, &common_type);
            let conditional = Expr::Conditional(Box::new(typed_condition), Box::new(converted_then), Box::new(converted_else));
            Ok(conditional.with_type(common_type))
        }
        ParserExpr::CompoundAssignment(op, lhs, rhs, _) => {
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            let typed_rhs = typecheck_exp(*rhs, symbols)?;
            let left_type = typed_lhs.exp_type.clone();
            let op_type = match op {
                AssignOp::BitwiseLeftShift | AssignOp::BitwiseRightShift => left_type.clone(),
                _ => get_common_type(&typed_lhs.exp_type, &typed_rhs.exp_type),
            };
            let compound = Expr::CompoundAssignment(op, Box::new(typed_lhs), Box::new(typed_rhs), op_type);
            Ok(compound.with_type(left_type))
        }
        ParserExpr::FunctionCall(Identifier(name), param_exps, span) => {
            let f_type = &symbols.get(&name).expect("Undefined function in typechecking").symbol_type;
            let (param_types, ret_type) = match f_type {
                Type::FunType { params, ret, .. } => (params.clone(), ret.clone()), //todo check clone
                _ => return Err(SemanticError::with_span(
                    format!("'{}' is not a function", name.bold()),
                    span,
                ))
            };

            if param_types.len() != param_exps.len() {
                return Err(SemanticError::with_span(
                    format!(
                        "function '{}' expects {} argument{} but {} {} provided",
                        name.bold(),
                        param_types.len(),
                        if param_types.len() == 1 { "" } else { "s" },
                        param_exps.len(),
                        if param_exps.len() == 1 { "was" } else { "were" }
                    ),
                    span,
                ))
            }

            let mut converted_args = Vec::with_capacity(param_exps.len());
            for (arg, param_type) in param_exps.iter().zip(param_types.iter()) {
                let typed_arg = typecheck_exp(arg.clone(), symbols)?; // todo check clone
                let converted_arg = convert_to(typed_arg, param_type);
                converted_args.push(converted_arg);
            }
            let call_exp = Expr::FunctionCall(Identifier(name), converted_args);
            Ok(call_exp.with_type(*ret_type))
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
/// Returns `Some(TypedFunction)` if the function has a body (definition),
/// or `None` if it's just a declaration.
///
/// The span stored in the symbol table is the location of the function's
/// definition (body), or the most recent declaration if never defined.
fn typecheck_function_declaration(decl: &FunDeclaration, symbols: &mut SymbolTable) -> Result<Option<TypedFunction>, SemanticError> {
    let mut global = decl.storage_class != Some(StorageClass::Static);
    let mut defined = decl.body.is_some();
    let mut saved_span = decl.span;
    let mut fun_type = decl.fun_type.clone();

    let (new_param_types, new_ret_type) = match &fun_type {
        Type::FunType { params, ret, .. } => (params.clone(), ret.clone()), // todo check clone
        _ => unreachable!("Function declaration must have FunType"),
    };

    if let Some(old_dec) = symbols.get(&decl.name.0) {
        match &old_dec.symbol_type {
            Type::FunType {
                params: old_params,
                ret: old_ret_type,
                defined: old_defined,
            } => {
                if old_params != &new_param_types || old_ret_type != &new_ret_type {
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
                if *old_defined {
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
                defined = defined || *old_defined;
            }
            Type::Int | Type::Long => {
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

    // Update the function type's defined field based on redeclarations
    if let Type::FunType { defined: ref mut d, .. } = fun_type {
        *d = defined;
    }

    symbols.insert(
        decl.name.0.clone(),
        Symbol {
            symbol_type: fun_type.clone(),
            global,
            val: InitialValue::NoInitializer,
            span: saved_span,
        },
    );

    // If function has a body, typecheck it and return TypedFunction
    if let Some(body) = &decl.body {
        for (Identifier(name), param_type) in decl.params.iter().zip(new_param_types.iter()) {
            symbols.insert(
                name.clone(),
                Symbol {
                    symbol_type: param_type.clone(),
                    global: false,
                    val: InitialValue::NoInitializer,
                    span: decl.span,
                },
            );
        }
        let typed_body = typecheck_block(body.clone(), symbols, &new_ret_type)?; // TODO: VERY expensive clone of entire Block - refactor typecheck_block to take &Block

        Ok(Some(TypedFunction {
            name: decl.name.clone(),
            params: decl.params.clone(), //todo check clone
            body: Some(typed_body),
            fun_type,
            global,
        }))
    } else {
        // Just a declaration, no body
        Ok(None)
    }
}

fn typecheck_block(block: ParserBlock, symbols: &mut SymbolTable, ret_type: &Type) -> Result<Block, SemanticError> {
    let mut typed_items = Vec::with_capacity(block.len());
    for item in block {
        match item {
            ParserBlockItem::Declaration(Declaration::VarDeclaration(var)) => {
                let typed_var = typecheck_local_variable_declaration(&var, symbols)?;
                typed_items.push(BlockItem::Declaration(TypedDeclaration::Variable(typed_var)));
            }
            ParserBlockItem::Declaration(Declaration::FunDeclaration(fun)) => {
                // Function declarations in blocks cannot have bodies (already validated in resolve phase)
                typecheck_function_declaration(&fun, symbols)?;
            }
            ParserBlockItem::Statement(stmt) => {
                let typed_stmt = typecheck_stmt(stmt.stmt, symbols, ret_type)?;
                typed_items.push(BlockItem::Statement(typed_stmt));
            }
        }
    }
    Ok(typed_items)
}

fn typecheck_stmt(stmt: ParserStmt, symbols: &mut SymbolTable, ret_type: &Type) -> Result<Stmt, SemanticError> {
    match stmt {
        ParserStmt::Return(e1) => {
            let typed_e1 = typecheck_exp(e1, symbols)?;
            let converted = convert_to(typed_e1, ret_type);
            Ok(Stmt::Return(converted))
        }
        ParserStmt::Expression(e1) => {
            let typed_e1 = typecheck_exp(e1, symbols)?;
            Ok(Stmt::Expression(typed_e1))
        },
        ParserStmt::If(cond, then_s, else_s) => {
            let typed_cond = typecheck_exp(cond, symbols)?;
            let converted_then = typecheck_stmt(then_s.stmt, symbols, ret_type)?;
            let typed_else = (*else_s).map(|e| typecheck_stmt(e.stmt, symbols, ret_type)).transpose()?.map(Box::new);
            Ok(Stmt::If(typed_cond, Box::new(converted_then), typed_else))
        }
        ParserStmt::Goto(lbl) => Ok(Stmt::Goto(lbl)),
        ParserStmt::Null => Ok(Stmt::Null),
        ParserStmt::Break(lbl) => Ok(Stmt::Break(lbl)),
        ParserStmt::Continue(lbl) => Ok(Stmt::Continue(lbl)),
        ParserStmt::Labeled(lbl, s) => {
            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            Ok(Stmt::Labeled(lbl, Box::new(typed_s)))
        },
        ParserStmt::Default(s, lbl) => {
            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            Ok(Stmt::Default(Box::new(typed_s), lbl))
        },
        ParserStmt::Compound(block) => {
            let typed_block = typecheck_block(block, symbols, ret_type)?;
            Ok(Stmt::Compound(typed_block))
        },
        ParserStmt::While(e, s, lbl) => {
            let typed_e = typecheck_exp(e, symbols)?;
            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            Ok(Stmt::While(typed_e, Box::new(typed_s), lbl))
        }
        ParserStmt::DoWhile(s, e, lbl) => {
            let typed_e = typecheck_exp(e, symbols)?;
            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            Ok(Stmt::DoWhile(Box::new(typed_s), typed_e, lbl))
        }
        ParserStmt::Switch(e, s, lbl, cases) => {
            let typed_e = typecheck_exp(e, symbols)?;
            let switch_type = &typed_e.exp_type;

            // Normalize cases and check for duplicates
            let mut normalized: HashMap<Option<i64>, Span> = HashMap::new();
            for (case, case_span) in cases.iter() {
                let norm_value = case.as_i64(switch_type);
                if let Some(previous_span) = normalized.insert(norm_value, *case_span) {
                    let dup_value = match norm_value {
                        Some(v) => v.to_string(),
                        None => "default".to_string(),
                    };
                    return Err(SemanticError::with_span(
                        format!(
                            "duplicate case value '{}'\n{}: {}: previous case was here",
                            dup_value.bold(),
                            previous_span,
                            "note".cyan()
                        ),
                        *case_span,
                    ));
                }
            }

            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            // Convert Vec to HashSet for output (tacky doesn't need spans)
            let cases_set = cases.iter().map(|(c, _span)| *c).collect();
            Ok(Stmt::Switch(typed_e, Box::new(typed_s), lbl, cases_set))
        }
        ParserStmt::Case(e, s, lbl) => {
            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            let typed_e = typecheck_exp(e, symbols)?;
            Ok(Stmt::Case(typed_e, Box::new(typed_s), lbl))
        }
        ParserStmt::For(init, cond, post, body, lbl) => {
            let typed_init = match init {
                ParserForInit::InitExp(Some(e)) => {
                    let typed_e = typecheck_exp(e, symbols)?;
                    ForInit::InitExp(Some(typed_e))
                },
                ParserForInit::InitExp(None) => ForInit::InitExp(None),
                ParserForInit::InitDecl(decl) => {
                    if decl.storage_class.is_some() {
                        return Err(SemanticError::with_span(
                            "declaration in for loop initializer cannot have storage class".to_string(),
                            decl.span,
                        ));
                    }
                    let typed_var = typecheck_local_variable_declaration(&decl, symbols)?;
                    ForInit::InitDecl(typed_var)
                }
            };
            let typed_cond = cond.map(|e| typecheck_exp(e, symbols)).transpose()?;
            let typed_post = post.map(|e| typecheck_exp(e, symbols)).transpose()?;
            let typed_body = typecheck_stmt(body.stmt, symbols, ret_type)?;
            Ok(Stmt::For(typed_init, typed_cond, typed_post, Box::new(typed_body), lbl))
        }
    }
}

fn resolve_exp(
    exp: &mut ParserExpr,
    variable_map: &HashMap<String, VarInfo>,
    used_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    match exp {
        ParserExpr::Assignment(e1, e2, span) | ParserExpr::CompoundAssignment(_, e1, e2, span) => match &**e1 {
            ParserExpr::Var(_, _) => {
                resolve_exp(e1, variable_map, used_vars)?;
                resolve_exp(e2, variable_map, used_vars)?;
                Ok(())
            }
            _ => Err(SemanticError::with_span(
                "Left-hand side of assignment must be a variable".to_string(),
                *span,
            )),
        },
        ParserExpr::Var(Identifier(name), span) => match variable_map.get(name) {
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
        ParserExpr::Unary(_, e) => resolve_exp(e, variable_map, used_vars),
        ParserExpr::Binary(_, left, right) => {
            resolve_exp(left, variable_map, used_vars)?;
            resolve_exp(right, variable_map, used_vars)
        }
        ParserExpr::Constant(_) => Ok(()),
        ParserExpr::PostFixOp(_op, e, span) | ParserExpr::PreFixOp(_op, e, span) => match &**e {
            ParserExpr::Var(_, _) => {
                resolve_exp(e, variable_map, used_vars)?;
                Ok(())
            }
            _ => Err(SemanticError::with_span(
                "Prefix or Postfix operator applied to non lvalue".to_string(),
                *span,
            )),
        },
        ParserExpr::Conditional(e, then_exp, else_exp) => {
            resolve_exp(e, variable_map, used_vars)?;
            resolve_exp(then_exp, variable_map, used_vars)?;
            resolve_exp(else_exp, variable_map, used_vars)?;
            Ok(())
        }
        ParserExpr::FunctionCall(Identifier(name), args, span) => match variable_map.get(name) {
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
        ParserExpr::Cast(_, e) => resolve_exp(e, variable_map, used_vars),
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

    let mut inner_map = variable_map.clone(); // TODO: expensive clone of HashMap for nested function - necessary for scoping but consider Arc/Rc approach
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
        ParserStmt::Return(e) => resolve_exp(e, variable_map, used_vars),
        ParserStmt::Expression(e) => resolve_exp(e, variable_map, used_vars),
        ParserStmt::Null => Ok(()),
        ParserStmt::If(e, then_stmt, else_stmt) => {
            resolve_exp(e, variable_map, used_vars)?;
            resolve_statement(then_stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            match else_stmt.as_mut() {
                Some(else_stmt) => resolve_statement(else_stmt, variable_map, labels, jumps, name_gen, used_vars),
                None => Ok(()),
            }
        }
        ParserStmt::Labeled(label_name, stmt) => {
            if !labels.insert(label_name.0.clone()) {
                return Err(SemanticError {
                    message: format!("Label {label_name:} already defined at {}", statement.span),
                    span: None,
                });
            }
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)
        }
        ParserStmt::Goto(label_name) => {
            jumps.insert(label_name.0.clone(), statement.span);
            Ok(())
        }
        ParserStmt::Compound(block) => {
            let mut shadow_map = variable_map.clone(); // TODO: expensive clone of HashMap per compound block - happens frequently in nested code
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
        ParserStmt::While(exp, stmt, _) | ParserStmt::Switch(exp, stmt, ..) | ParserStmt::Case(exp, stmt, ..) => {
            resolve_exp(exp, variable_map, used_vars)?;
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)
        }
        ParserStmt::DoWhile(stmt, exp, _) => {
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            resolve_exp(exp, variable_map, used_vars)?;
            Ok(())
        }
        ParserStmt::For(init, e1, e2, stmt, _) => {
            let mut shadow_map = variable_map.clone(); // TODO: expensive clone of HashMap per for loop - same issue as compound blocks
            match init {
                ParserForInit::InitExp(exp) => {
                    if let Some(e) = exp {
                        resolve_exp(e, &shadow_map, used_vars)?;
                    }
                }
                ParserForInit::InitDecl(dec) => {
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
        ParserStmt::Break(..) | ParserStmt::Continue(..) => Ok(()),
        ParserStmt::Default(stmt, ..) => resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars),
    }
}

fn convert_to_type(val: StaticInt, target_type: &Type) -> StaticInt {
    match (val, target_type) {
        (StaticInt::IntInit(v), Type::Int) => StaticInt::IntInit(v),
        (StaticInt::LongInit(v), Type::Long) => StaticInt::LongInit(v),

        (StaticInt::IntInit(v), Type::Long) => StaticInt::LongInit(v as i64),
        (StaticInt::LongInit(v), Type::Int) => StaticInt::IntInit(v as i32),

        (_, Type::FunType { .. }) => unreachable!("Cannot cast to function type in constant expression"),
    }
}

/// Evaluates a compile-time constant expression from the parser AST.
///
/// Handles all arithmetic, bitwise, comparison, and logical operations with proper
/// type promotion (Int + Long = Long). Type conversions follow implementation-defined
/// behavior documented in README (wrapping arithmetic, shift masking, etc.).
///
/// Used for static initializers, case labels, and other contexts requiring constant
/// expressions. Does NOT call typecheck_exp to avoid mixing with general expression
/// transformations that will be added in later chapters.
///
/// Returns None if the expression contains non-constant elements (variables, function
/// calls, assignments, etc.) or division by zero.
fn eval_constant_expr(expr: &ParserExpr) -> Option<StaticInt> {
    match expr {
        ParserExpr::Constant(Const::ConstInt(val)) => Some(StaticInt::IntInit(*val)),
        ParserExpr::Constant(Const::ConstLong(val)) => Some(StaticInt::LongInit(*val)),
        ParserExpr::Cast(target, val) => eval_constant_expr(val).map(|e| convert_to_type(e, &target)),
        ParserExpr::Unary(op, inner) => {
            let inner_val = eval_constant_expr(inner)?;
            match op {
                UnaryOp::Negate => Some(match inner_val {
                    StaticInt::IntInit(v) => StaticInt::IntInit(-v),
                    StaticInt::LongInit(v) => StaticInt::LongInit(-v)
                }),
                UnaryOp::BitwiseComplement => Some(match inner_val {
                    StaticInt::IntInit(v) => StaticInt::IntInit(!v),
                    StaticInt::LongInit(v) => StaticInt::LongInit(!v)
                }),
                UnaryOp::Not => Some(match inner_val {
                    StaticInt::IntInit(v) => StaticInt::IntInit(if v == 0 { 1 } else { 0 }),
                    StaticInt::LongInit(v) => StaticInt::IntInit(if v == 0 { 1 } else { 0 })
                })
            }
        }
        ParserExpr::Binary(op, left, right) => {
            let left_val = eval_constant_expr(left)?;
            let right_val = eval_constant_expr(right)?;
            match op {
                BinOp::Add => Some(left_val.wrapping_add(right_val)),
                BinOp::Subtract => Some(left_val.wrapping_sub(right_val)),
                BinOp::Multiply => Some(left_val.wrapping_mul(right_val)),
                BinOp::Divide => left_val.div(right_val),
                BinOp::Remainder => left_val.rem(right_val),
                BinOp::BitwiseAnd => Some(left_val.bitwise_and(right_val)),
                BinOp::BitwiseOr => Some(left_val.bitwise_or(right_val)),
                BinOp::BitwiseXOr => Some(left_val.bitwise_xor(right_val)),
                BinOp::BitwiseLeftShift => Some(left_val.shl(right_val)),
                BinOp::BitwiseRightShift => Some(left_val.shr(right_val)),
                BinOp::Equal => Some(left_val.eq(right_val)),
                BinOp::NotEqual => Some(left_val.ne(right_val)),
                BinOp::LessThan => Some(left_val.lt(right_val)),
                BinOp::LessOrEqual => Some(left_val.le(right_val)),
                BinOp::GreaterThan => Some(left_val.gt(right_val)),
                BinOp::GreaterOrEqual => Some(left_val.ge(right_val)),
                BinOp::And => Some(left_val.and(right_val)),
                BinOp::Or => Some(left_val.or(right_val)),
                BinOp::Assignment | BinOp::CompoundAssignment | BinOp::Conditional => {
                    unreachable!("only used for parsing")
                }
            }
        }
        ParserExpr::Conditional(cond, true_expr, false_expr) => {
            let cond_val = eval_constant_expr(cond)?;
            if cond_val.is_zero() {
                eval_constant_expr(false_expr)
            } else {
                eval_constant_expr(true_expr)
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
    switch_to_cases: HashMap<u64, Vec<(SwitchIntType, Span)>>, // switch -> (case, span) for error reporting
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

    fn get_switch_case(&mut self, c: StaticInt, span: &Span) -> Result<String, SemanticError> {
        // get the active switch id
        if let Some(LabelTag::Switch(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => true,
            LabelTag::Loop(..) => false,
        }) {
            let case_exp = match c {
                StaticInt::IntInit(v) => SwitchIntType::Int(v),
                StaticInt::LongInit(v) => SwitchIntType::Long(v),
            };
            // Just collect cases with spans - duplicate checking happens during typecheck
            self.switch_to_cases.get_mut(label).unwrap().push((case_exp, *span));
            Ok(case_exp.label_str(*label))
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
            // Just collect with span - duplicate checking happens during typecheck
            self.switch_to_cases.get_mut(label).unwrap().push((default_case, *span));
            Ok(default_case.label_str(*label))
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
        self.switch_to_cases.insert(res, Vec::new());
        self.cur_label.push(LabelTag::Switch(res));
        res
    }

    fn pop(&mut self) {
        self.cur_label.pop();
    }

    fn pop_switch(&mut self, switch_id: u64) -> Vec<(SwitchIntType, Span)> {
        self.cur_label.pop();
        self.switch_to_cases.get(&switch_id).unwrap().clone() // todo check clone
    }
}

fn label_statement(stmt: &mut SpannedStmt, label_tracker: &mut LabelTracker) -> Result<(), SemanticError> {
    match &mut stmt.stmt {
        // terminating statements
        ParserStmt::Return(_) | ParserStmt::Expression(_) | ParserStmt::Goto(..) | ParserStmt::Null => Ok(()),
        ParserStmt::If(_, then_stmt, else_stmt) => {
            label_statement(then_stmt, label_tracker)?;
            if let Some(c) = &mut **else_stmt {
                label_statement(c, label_tracker)
            } else {
                Ok(())
            }
        }
        ParserStmt::Labeled(_, stmt) => label_statement(stmt, label_tracker),
        ParserStmt::Compound(block) => {
            for bi in block.iter_mut() {
                match bi {
                    ParserBlockItem::Statement(stmt) => {
                        label_statement(stmt, label_tracker)?;
                    }
                    ParserBlockItem::Declaration(_) => {}
                }
            }
            Ok(())
        }
        ParserStmt::Break(Identifier(s)) => {
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
        ParserStmt::Continue(Identifier(s)) => {
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
        ParserStmt::While(_, body, loop_num) | ParserStmt::DoWhile(body, _, loop_num) | ParserStmt::For(_, _, _, body, loop_num) => {
            *loop_num = label_tracker.next_loop_label();
            let result = label_statement(body, label_tracker);
            label_tracker.pop();
            result
        }
        ParserStmt::Switch(_, stmt, switch_num, cases) => {
            *switch_num = label_tracker.next_switch_label();
            let result = label_statement(stmt, label_tracker);
            *cases = label_tracker.pop_switch(*switch_num);
            result
        }
        ParserStmt::Case(exp, s, Identifier(label)) => {
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
        ParserStmt::Default(s, Identifier(label)) => {
            *label = label_tracker.get_switch_default(&stmt.span)?;
            label_statement(s, label_tracker)
        }
    }
}

fn label_block(block: &mut ParserBlock, label_tracker: &mut LabelTracker) -> Result<(), SemanticError> {
    for bi in block.iter_mut() {
        if let ParserBlockItem::Statement(stmt) = bi {
            label_statement(stmt, label_tracker)?;
        }
    }
    Ok(())
}

fn resolve_block(
    block: &mut ParserBlock,
    block_vars: &mut HashSet<String>,
    variable_map: &mut HashMap<String, VarInfo>,
    labels: &mut HashSet<String>,
    jumps: &mut HashMap<String, Span>,
    name_gen: &mut NameGenerator,
    used_vars: &mut HashSet<String>,
) -> Result<(), SemanticError> {
    for bi in block.iter_mut() {
        match bi {
            ParserBlockItem::Statement(stmt) => {
                resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            }
            ParserBlockItem::Declaration(d) => match d {
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

pub fn resolve_program(program: &mut Program) -> Result<(NameGenerator, TypedProgram, SymbolTable), SemanticError> {
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
        let (typed_program, symbols) = typecheck_program(program)?;
        Ok((name_gen, typed_program, symbols))
    } else {
        let (label, span) = &undefined_jumps[0];
        Err(SemanticError::with_span(
            format!("Jump to undefined label '{}'", label.bold()),
            *span,
        ))
    }
}

pub fn typecheck_program(program: &Program) -> Result<(TypedProgram, SymbolTable), SemanticError> {
    let mut symbols = SymbolTable::new();
    let mut typed_declarations = Vec::with_capacity(program.declarations.len());

    for decl in &program.declarations {
        match decl {
            Declaration::VarDeclaration(dec) => {
                let typed_dec = typecheck_file_variable_declaration(dec, &mut symbols)?;
                typed_declarations.push(TypedDeclaration::Variable(typed_dec));
            }
            Declaration::FunDeclaration(func) => {
                if let Some(typed_func) = typecheck_function_declaration(func, &mut symbols)? {
                    // Function has a body (definition)
                    typed_declarations.push(TypedDeclaration::Function(typed_func));
                }
                // If None, it's just a declaration - don't add to typed program
            }
        };
    }
    Ok((TypedProgram { declarations: typed_declarations }, symbols))
}
