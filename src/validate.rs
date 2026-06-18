//! # Validator — Semantic Analysis (Resolution + Type Checking)
//!
//! Two-pass semantic analysis that transforms the untyped parser AST into a
//! [`TypedProgram`] with fully resolved names and types.
//!
//! ## Technical Approach
//!
//! The validator runs two sequential passes over the AST, both orchestrated by
//! [`resolve_program`] (the single public entry point called from `main`):
//!
//! **Pass 1 — Resolution** mutates the parser AST in place:
//! - Renames local variables to unique identifiers (`x` -> `x.1`) via [`NameGenerator`]
//! - Validates lvalue requirements (assignment/increment targets must be variables)
//! - Labels loops and switches with unique IDs for break/continue/case
//! - Validates goto targets exist, detects duplicate labels
//! - Emits `-Wshadow`, `-Wunused-parameter`, and `-Wsequence-point` warnings
//!
//! **Pass 2 — Type Checking** consumes the mutated AST and produces typed output:
//! - Builds the [`SymbolTable`] mapping identifiers to types, linkage, and initial values
//! - Inserts implicit casts via common-type promotion (`int` + `long` -> `long`)
//! - Validates function call arity and argument types
//! - Evaluates constant expressions for static initializers and case labels (double->int folding
//!   mirrors x86 `cvttsd2si`, not Rust's saturating `as` — see [`double_to_i32`])
//! - Emits `-Wdiv-by-zero` for `/` or `%` with a constant zero divisor
//! - Emits `-Wshift-count-overflow` / `-Wshift-count-negative` for an out-of-range constant shift count
//! - Emits `-Woverflow` when a constant fold leaves the result type (in static initializers / case labels)
//! - Emits `-Wconstant-conversion` when an implicit narrowing of a constant initializer changes its value
//! - Enforces declaration consistency (linkage, types, single-definition rule)
//!
//! ## What This Pass Accomplishes
//!
//! - Produces a [`TypedProgram`] where every expression carries its resolved type
//! - Produces a [`SymbolTable`] consumed by tackifier and codegen
//! - Produces a [`NameGenerator`] with counters for the tackifier to continue from
//! - Detects semantic errors and exits with code 30
//!
//! ## Call Order
//!
//! ```text
//! resolve_program()                              — public entry point (Pass 1 + Pass 2)
//!   │
//!   │  ── Pass 1: Resolution ──
//!   ├─ label_block()                             — assign loop/switch IDs
//!   │    └─ label_statement()                    — break/continue/case label generation
//!   ├─ resolve_file_var_declaration()            — register file-scope vars (no rename)
//!   ├─ resolve_fun_decoration()                  — process each function
//!   │    ├─ resolve_param()                      — rename parameters
//!   │    └─ resolve_block()                      — process function body
//!   │         ├─ resolve_local_var_decoration()  — rename locals, check shadowing
//!   │         └─ resolve_statement()             — resolve expressions in statements
//!   │              └─ resolve_exp()              — replace var names with unique renames
//!   │
//!   │  ── Pass 2: Type Checking ──
//!   └─ typecheck_program()                       — called after resolution succeeds
//!        ├─ typecheck_file_variable_declaration() — file-scope var rules
//!        └─ typecheck_function_declaration()      — function type consistency
//!             └─ typecheck_block()                — process function body
//!                  ├─ typecheck_local_variable_declaration()
//!                  └─ typecheck_stmt()            — recursive statement typing
//!                       └─ typecheck_exp()        — core: assign types, insert casts
//! ```

use crate::lexer::Span;
use crate::parser::{
    AssignOp, BinOp, Block as ParserBlock, BlockItem as ParserBlockItem, Const, Declaration, Expr as ParserExpr,
    ForInit as ParserForInit, FunDeclaration, Identifier, IncDec, Program, SpannedStmt, Stmt as ParserStmt,
    StorageClass, SwitchIntType, Type, UnaryOp, VarDeclaration,
};
use colored::*;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

pub struct SemanticError {
    message: String,
    span: Option<Span>,
}

#[derive(Clone)]
pub struct TypedExpression {
    pub exp_type: Type,
    pub exp: Expr,
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
    For(
        Box<ForInit>,
        Option<TypedExpression>,
        Option<TypedExpression>,
        Box<Stmt>,
        u64,
    ),
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
            Some(span) => write!(f, "{}: {}: {}", span, "error".red(), self.message),
            None => write!(f, "{}: {}", "error".red(), self.message),
        }
    }
}

#[derive(Clone, Copy)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInitializer,
}

/// A compile-time constant value (integer or `double`) for static variable initializers.
///
/// Carries both the value and its type so that zero-initialized `.bss` vs
/// initialized `.data` placement can be decided later in emission.
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(clippy::enum_variant_names)] // `Init` suffix = static-initializer value
pub enum StaticInit {
    IntInit(i32),
    LongInit(i64),
    UIntInit(u32),
    ULongInit(u64),
    DoubleInit(f64),
}

macro_rules! checked_op {
    // unary:  checked_op!(self, overflowing_neg, -; IntInit, LongInit, UIntInit, ULongInit)
    ($self:expr, $method:ident; $op:tt; $($V:ident),+ $(,)?) => {{
        match $self {
            StaticInit::DoubleInit(v) => (StaticInit::DoubleInit($op v), false),
            $( StaticInit::$V(v) => { let (r, o) = v.$method(); (StaticInit::$V(r), o) } )+
        }
    }};
    // binary: checked_op!(self, other, overflowing_add, +; IntInit, LongInit, UIntInit, ULongInit)
    ($self:expr, $other:expr, $method:ident; $op:tt; $($V:ident),+ $(,)?) => {{
        let (left, right) = $self.get_common($other);
        match (left, right) {
            (StaticInit::DoubleInit(a), StaticInit::DoubleInit(b)) => (StaticInit::DoubleInit(a $op b), false),
            $( (StaticInit::$V(a), StaticInit::$V(b)) => { let (v, o) = a.$method(b); (StaticInit::$V(v), o) } )+
            _ => unreachable!("get_common guarantees matching variants"),
        }
    }};
}

macro_rules! bitwise_op {
    ($self:expr, $other:expr, $op:tt; $($V:ident),+ $(,)?) => {{
        let (left, right) = $self.get_common($other);
        match (left, right) {
            $( (StaticInit::$V(a), StaticInit::$V(b)) => StaticInit::$V(a $op b), )+
            _ => unreachable!("get_common guarantees matching variants"),
        }
    }};
}

macro_rules! compare_op {
    ($self:expr, $other:expr, $op:tt; $($V:ident),+ $(,)?) => {{
        let (left, right) = $self.get_common($other);
        match (left, right) {
            (StaticInit::DoubleInit(a), StaticInit::DoubleInit(b)) => StaticInit::IntInit((a $op b) as i32),
            $( (StaticInit::$V(a), StaticInit::$V(b)) => StaticInit::IntInit((a $op b) as i32), )+
            _ => unreachable!("get_common guarantees matching variants"),
        }
    }};
}

impl StaticInit {
    fn get_type(&self) -> Type {
        match self {
            StaticInit::IntInit(_) => Type::Int,
            StaticInit::LongInit(_) => Type::Long,
            StaticInit::UIntInit(_) => Type::UInt,
            StaticInit::ULongInit(_) => Type::ULong,
            StaticInit::DoubleInit(_) => Type::Double,
        }
    }

    fn to_const(self) -> Const {
        match self {
            StaticInit::IntInit(i) => Const::ConstInt(i),
            StaticInit::LongInit(l) => Const::ConstLong(l),
            StaticInit::UIntInit(i) => Const::ConstUInt(i),
            StaticInit::ULongInit(l) => Const::ConstULong(l),
            StaticInit::DoubleInit(d) => Const::ConstDouble(d),
        }
    }

    fn to_string(self) -> String {
        match self {
            StaticInit::IntInit(n) => n.to_string(),
            StaticInit::LongInit(n) => n.to_string(),
            StaticInit::UIntInit(n) => n.to_string(),
            StaticInit::ULongInit(n) => n.to_string(),
            StaticInit::DoubleInit(n) => n.to_string(),
        }
    }

    /// Exact value of this constant as a signedness-split wide integer (see [`Wide`]).
    /// INVARIANT: `Wide`'s arms must stay >= the widest StaticInt variant — widen them to
    /// i128/u128 in the same change that adds a 128-bit type, or constants silently truncate.
    fn wide(self) -> Wide {
        match self {
            StaticInit::IntInit(v) => Wide::Signed(v as i64),
            StaticInit::LongInit(v) => Wide::Signed(v),
            StaticInit::UIntInit(v) => Wide::Unsigned(v as u64),
            StaticInit::ULongInit(v) => Wide::Unsigned(v),
            StaticInit::DoubleInit(_) => unreachable!("only used on integer types"),
        }
    }

    pub(crate) fn to_le_bytes(self) -> Vec<u8> {
        match self {
            StaticInit::UIntInit(v) => v.to_le_bytes().to_vec(),
            StaticInit::IntInit(v) => v.to_le_bytes().to_vec(),
            StaticInit::LongInit(v) => v.to_le_bytes().to_vec(),
            StaticInit::ULongInit(v) => v.to_le_bytes().to_vec(),
            StaticInit::DoubleInit(v) => v.to_le_bytes().to_vec(),
        }
    }

    /// Assembler directive (`.long`/`.quad`) and decimal value for a `.data` initializer
    /// (text emitter). Width follows the type; the value uses the variant's signedness.
    pub(crate) fn data_directive(&self) -> (&'static str, String) {
        match self {
            StaticInit::IntInit(v) => (".long", v.to_string()),
            StaticInit::UIntInit(v) => (".long", v.to_string()),
            StaticInit::LongInit(v) => (".quad", v.to_string()),
            StaticInit::ULongInit(v) => (".quad", v.to_string()),
            StaticInit::DoubleInit(v) => (".double", v.to_string()),
        }
    }

    pub fn get_common(self, other: Self) -> (Self, Self) {
        let common_type = self.get_type().common_with(&other.get_type());
        // Promotion only widens to the common type, so it never truncates — ignore the flag.
        let (left, _) = convert_to_type(self, &common_type);
        let (right, _) = convert_to_type(other, &common_type);
        (left, right)
    }

    fn neg(self) -> (Self, bool) {
        checked_op!(self, overflowing_neg; -; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn add(self, other: Self) -> (Self, bool) {
        checked_op!(self, other, overflowing_add; +; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn sub(self, other: Self) -> (Self, bool) {
        checked_op!(self, other, overflowing_sub; -; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn mul(self, other: Self) -> (Self, bool) {
        checked_op!(self, other, overflowing_mul; *; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn is_zero(&self) -> bool {
        if let StaticInit::DoubleInit(val) = *self {
            val == 0.0
        } else {
            self.as_i64() == 0
        }
    }

    fn as_i64(&self) -> i64 {
        match *self {
            StaticInit::IntInit(v) => v as i64,
            StaticInit::LongInit(v) => v,
            StaticInit::UIntInit(v) => v as i64,
            StaticInit::ULongInit(v) => v as i64,
            StaticInit::DoubleInit(_) => unreachable!("Integer only helper"),
        }
    }

    fn as_u32(&self) -> u32 {
        match *self {
            StaticInit::IntInit(v) => v as u32,
            StaticInit::LongInit(v) => v as u32,
            StaticInit::UIntInit(v) => v,
            StaticInit::ULongInit(v) => v as u32,
            StaticInit::DoubleInit(_) => unreachable!("Integer only helper"),
        }
    }

    fn div(self, other: Self) -> Result<(Self, bool), ConstEvalError> {
        if !matches!(other, StaticInit::DoubleInit(_)) && other.is_zero() {
            return Err(ConstEvalError::DivByZero);
        }
        Ok(checked_op!(self, other, overflowing_div; /; IntInit, LongInit, UIntInit, ULongInit))
    }

    fn rem(self, other: Self) -> Result<(Self, bool), ConstEvalError> {
        if other.is_zero() {
            return Err(ConstEvalError::DivByZero);
        }
        // invalid for floating point operations put passing so I can reuse the helper
        Ok(checked_op!(self, other, overflowing_rem; /; IntInit, LongInit, UIntInit, ULongInit))
    }

    fn bitwise_and(self, other: Self) -> Self {
        bitwise_op!(self, other, &; IntInit, LongInit, UIntInit, ULongInit)
    }
    fn bitwise_or(self, other: Self) -> Self {
        bitwise_op!(self, other, |; IntInit, LongInit, UIntInit, ULongInit)
    }
    fn bitwise_xor(self, other: Self) -> Self {
        bitwise_op!(self, other, ^; IntInit, LongInit, UIntInit, ULongInit)
    }

    /// Left shift. Shift amount is masked (& 31 for int, & 63 for long) to prevent
    /// undefined behavior, matching x86 hardware semantics.
    fn shl(self, other: Self) -> Self {
        let shift_amount = other.as_u32();

        match self {
            StaticInit::IntInit(a) => StaticInit::IntInit(a << (shift_amount & 31)),
            StaticInit::LongInit(a) => StaticInit::LongInit(a << (shift_amount & 63)),
            StaticInit::UIntInit(a) => StaticInit::UIntInit(a << (shift_amount & 31)),
            StaticInit::ULongInit(a) => StaticInit::ULongInit(a << (shift_amount & 63)),
            StaticInit::DoubleInit(_) => unreachable!("Integer only operation"),
        }
    }

    /// Right shift: arithmetic for signed operands, logical for unsigned (Rust's `>>`
    /// follows the operand's signedness, matching x86 SAR vs SHR). Shift amount is masked
    /// (& 31 for 32-bit, & 63 for 64-bit) to prevent undefined behavior, matching x86 hardware.
    fn shr(self, other: Self) -> Self {
        let shift_amount = other.as_u32();

        match self {
            StaticInit::IntInit(a) => StaticInit::IntInit(a >> (shift_amount & 31)),
            StaticInit::LongInit(a) => StaticInit::LongInit(a >> (shift_amount & 63)),
            StaticInit::UIntInit(a) => StaticInit::UIntInit(a >> (shift_amount & 31)),
            StaticInit::ULongInit(a) => StaticInit::ULongInit(a >> (shift_amount & 63)),
            StaticInit::DoubleInit(_) => unreachable!("Integer only operation"),
        }
    }

    fn eq(self, other: Self) -> Self {
        compare_op!(self, other, ==; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn ne(self, other: Self) -> Self {
        compare_op!(self, other, !=; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn lt(self, other: Self) -> Self {
        compare_op!(self, other, <; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn le(self, other: Self) -> Self {
        compare_op!(self, other, <=; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn gt(self, other: Self) -> Self {
        compare_op!(self, other, >; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn ge(self, other: Self) -> Self {
        compare_op!(self, other, >=; IntInit, LongInit, UIntInit, ULongInit)
    }

    fn and(self, other: Self) -> Self {
        StaticInit::IntInit(if !self.is_zero() && !other.is_zero() { 1 } else { 0 })
    }

    fn or(self, other: Self) -> Self {
        StaticInit::IntInit(if !self.is_zero() || !other.is_zero() { 1 } else { 0 })
    }
}

/// Symbol table entry for variables and functions.
///
/// # Fields
///
/// - `symbol_type`: The type of the symbol (Int, Long, UInt, ULong, or FunType)
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

pub type SymbolTable = HashMap<Rc<str>, Symbol>;

#[derive(Clone)]
struct VarInfo {
    renamed: Rc<str>,
    span: Span,
    ext_link: bool,
}

pub struct NameGenerator {
    counts: HashMap<Rc<str>, usize>,
}

impl NameGenerator {
    pub fn new() -> NameGenerator {
        NameGenerator { counts: HashMap::new() }
    }

    pub fn next(&mut self, base: &str) -> Rc<str> {
        let count = self.counts.entry(Rc::from(base)).or_insert(0);
        *count += 1;
        Rc::from(format!("{base}.{count}"))
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
fn typecheck_local_variable_declaration(
    decl: &VarDeclaration,
    symbols: &mut SymbolTable,
) -> Result<TypedVarDeclaration, SemanticError> {
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
            eval_static_initializer(expr, &decl.var_type, &decl.name, decl.span)?
        } else {
            let zero = match decl.var_type {
                Type::Int => StaticInit::IntInit(0),
                Type::Long => StaticInit::LongInit(0),
                Type::UInt => StaticInit::UIntInit(0),
                Type::ULong => StaticInit::ULongInit(0),
                Type::Double => StaticInit::DoubleInit(0.0),
                _ => unreachable!("static variable must be numeric"),
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
    let typed_init = decl
        .init
        .as_ref()
        .map(|e| typecheck_exp(e.clone(), symbols)) // TODO: expensive clone of ParserExpr - refactor typecheck_exp to take &ParserExpr
        .transpose()?
        .map(|exp| convert_to(exp, &decl.var_type));

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
fn typecheck_file_variable_declaration(
    decl: VarDeclaration,
    symbols: &mut SymbolTable,
) -> Result<TypedVarDeclaration, SemanticError> {
    let mut initial_value = if let Some(expr) = &decl.init {
        eval_static_initializer(expr, &decl.var_type, &decl.name, decl.span)?
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
            Type::Int | Type::Long | Type::UInt | Type::ULong | Type::Double => {
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
            }
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
        name: decl.name,
        init: typed_init,
        var_type: decl.var_type,
        storage_class: decl.storage_class,
    })
}

fn convert_to(exp: TypedExpression, t: &Type) -> TypedExpression {
    if exp.exp_type == *t {
        exp
    } else {
        let cast_exp = Expr::Cast(t.clone(), Box::new(exp));
        cast_exp.with_type(t.clone())
    }
}

/// `-Wsequence-point`: entry point for one *full* expression. Warns when the same object is
/// modified more than once with no sequence point in between — undefined in standard C (NCC itself
/// evaluates left-to-right, so the result is well-defined but the code is non-portable).
///
/// Call once per full expression (expression statements, conditions, `return`, initializers) — NOT
/// on sub-expressions, or nested cases would be reported twice.
fn check_sequence_points(expr: &ParserExpr) {
    let mut region: HashMap<Rc<str>, Span> = HashMap::new();
    walk_region(expr, &mut region);
}

/// Walks `expr` within the current unsequenced region, recording modifications into `region`.
/// Sequence-point operators (`&&`, `||`, `?:`, and function-call arguments) start fresh regions
/// via [`check_sequence_points`]; everything else shares the region with its operands.
fn walk_region(expr: &ParserExpr, region: &mut HashMap<Rc<str>, Span>) {
    match expr {
        ParserExpr::Assignment(lhs, rhs, span) | ParserExpr::CompoundAssignment(_, lhs, rhs, span) => {
            warn_sequence_point(lhs, *span, region);
            walk_region(rhs, region);
        }
        ParserExpr::PreFixOp(_, inner, span) | ParserExpr::PostFixOp(_, inner, span) => {
            warn_sequence_point(inner, *span, region);
        }
        ParserExpr::Binary(op, lhs, rhs, _) => {
            if matches!(op, BinOp::And | BinOp::Or) {
                // sequence points
                check_sequence_points(lhs);
                check_sequence_points(rhs);
            } else {
                walk_region(lhs, region);
                walk_region(rhs, region);
            }
        }
        ParserExpr::Unary(_, e, _) | ParserExpr::Cast(_, e) => walk_region(e, region),
        ParserExpr::Conditional(c, t, f) => {
            check_sequence_points(c);
            check_sequence_points(t);
            check_sequence_points(f);
        }
        ParserExpr::FunctionCall(_, args, _) => {
            // args have no sequence points between them
            let mut arg_region = HashMap::new();
            for a in args {
                walk_region(a, &mut arg_region);
            }
        }
        ParserExpr::Var(..) | ParserExpr::Constant(..) => {}
    }
}

/// Records a modification of the object named by `target` (an lvalue, always a `Var` in this
/// subset). A second modification of the same object in the current region is an unsequenced
/// double-modification — emit `-Wsequence-point`.
fn warn_sequence_point(target: &ParserExpr, span: Span, region: &mut HashMap<Rc<str>, Span>) {
    if let ParserExpr::Var(Identifier(name), _) = target
        && region.insert(name.clone(), span).is_some()
    {
        eprintln!(
            "{}: {}: '{}' modified more than once between sequence points (undefined in standard C; NCC evaluates left-to-right) {}",
            span,
            "warning".purple(),
            name,
            "[-Wsequence-point]".purple()
        )
    }
}

/// `-Wdiv-by-zero`: warn if a `/` or `%` (or `/=` / `%=`) divisor folds to a constant 0.
/// `is_division` selects the message wording (division vs remainder).
fn warn_div_by_zero(is_division: bool, rhs: &ParserExpr, span: Span) {
    // The double exclusion lives here, not upstream, because `/` legitimately accepts double
    // operands: `1.0 / 0.0` is `inf` (well-defined), not the SIGFPE this warning is about. So a
    // double divisor really does reach this point — and `is_zero()` is true for `0.0` — so without
    // this guard we'd warn on correct floating-point division.
    if let Ok((v, _)) = eval_constant_expr(rhs)
        && !matches!(v, StaticInit::DoubleInit(_))
        && v.is_zero()
    {
        eprintln!(
            "{}: {}: {} by zero {}",
            span,
            "warning".purple(),
            if is_division { "division" } else { "remainder" },
            "[-Wdiv-by-zero]".purple()
        );
    }
}

/// `-Wshift-count-overflow` / `-Wshift-count-negative`: warn if a shift's constant count is
/// negative or `>=` the width of the left operand's type (`int` -> 32, `long` -> 64).
fn warn_shift_count(left_type: &Type, rhs: &ParserExpr, span: Span) {
    // The double exclusion lives here for two reasons: this runs *before* the rhs is typechecked
    // (so the "shift count must be integer" type error hasn't fired yet — see the caller), and
    // `as_i64()` just below panics on a `DoubleInit`. A double shift count is rejected as a type
    // error elsewhere; here we only need to avoid the meaningless warning and the panic.
    if let Ok((v, _)) = eval_constant_expr(rhs)
        && !matches!(v, StaticInit::DoubleInit(_))
    {
        let width: i64 = match left_type {
            Type::Long => 64,
            _ => 32,
        };
        let count = v.as_i64();
        if count < 0 {
            eprintln!(
                "{}: {}: shift count is negative {}",
                span,
                "warning".purple(),
                "[-Wshift-count-negative]".purple()
            );
        } else if count >= width {
            eprintln!(
                "{}: {}: shift count >= width of type ({}) {}",
                span,
                "warning".purple(),
                width,
                "[-Wshift-count-overflow]".purple()
            );
        }
    }
}

/// `-Woverflow`: a constant fold whose result left the result type. NCC wraps deterministically
/// (two's complement), so this flags non-portable code rather than reporting UB. Only emitted in
/// constant contexts (static initializers, case labels), where `eval_constant_expr` folds.
fn warn_overflow(overflowed: bool, span: Span) {
    if overflowed {
        eprintln!(
            "{}: {}: integer overflow in constant expression (result wraps) {}",
            span,
            "warning".purple(),
            "[-Woverflow]".purple()
        );
    }
}

/// `-Wconstant-conversion`: an implicit narrowing conversion of a constant that changed its value
/// (e.g. `int x = 0x1FFFFFFFF;`). `changed` is the truncation flag from `convert_to_type`. Explicit
/// casts suppress this — the caller only invokes it for implicit conversions.
///
/// Conversions involving `double` never warn here (float↔int narrowing is gcc's separate, deferred
/// `-Wfloat-conversion`); the guard also keeps `as_i64` — an integer-only helper — off double values.
fn warn_constant_conversion(changed: bool, from: &StaticInit, to: &StaticInit, span: Span) {
    if changed {
        eprintln!(
            "{}: {}: implicit conversion changes constant value from {} to {} {}",
            span,
            "warning".purple(),
            from.to_string(),
            to.to_string(),
            "[-Wconstant-conversion]".purple()
        );
    }
}

/// `-Wshadow`: a declaration shadows one in an outer scope. `prev` is the shadowed entry's span
/// (the value returned when inserting into the scope map), or `None` if nothing was shadowed.
/// Emits the warning plus a note pointing at the previous declaration.
fn warn_shadow(name: &str, span: Span, prev: Option<Span>) {
    if let Some(prev_span) = prev {
        eprintln!(
            "{}: {}: variable {} shadows previous declaration {}",
            span,
            "warning".purple(),
            format!("'{}'", name).bold(),
            "[-Wshadow]".purple()
        );
        eprintln!(
            "{}: {}: previous declaration of {} was here",
            prev_span,
            "note".cyan(),
            format!("'{}'", name).bold()
        );
    }
}

/// `-Wunused-parameter`: a function parameter (in a function with a body) is never referenced.
/// `used` is whether the parameter appears in the body; `name` is the original source name.
fn warn_unused_parameter(used: bool, name: &str, span: Span) {
    if !used {
        eprintln!(
            "{}: {}: unused parameter '{}' {}",
            span,
            "warning".purple(),
            name.bold(),
            "[-Wunused-parameter]".purple()
        );
    }
}

/// Type-checks an expression, returning a `TypedExpression` with a resolved type.
///
/// Inserts implicit casts where operand types differ (using the common-type rule),
/// with special cases: logical ops always produce `Int`, bitshifts keep the left operand's
/// type, comparisons produce `Int`, and assignments convert the RHS to the LHS type.
/// Validates function call arity and argument types against the symbol table.
///
/// Also emits warnings for constant operands that are valid but suspect: `-Wdiv-by-zero`
/// (constant zero divisor) and `-Wshift-count-overflow` / `-Wshift-count-negative`
/// (constant shift count outside `0..width`, where width comes from the left operand's type).
fn typecheck_exp(exp: ParserExpr, symbols: &mut SymbolTable) -> Result<TypedExpression, SemanticError> {
    match exp {
        ParserExpr::Var(Identifier(name), span) => {
            let v_type = &symbols
                .get(&name)
                .expect("Undefined variable in typechecking")
                .symbol_type;
            match v_type {
                Type::FunType { .. } => Err(SemanticError::with_span(
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
            Const::ConstInt(_) => Ok(TypedExpression {
                exp_type: Type::Int,
                exp: Expr::Const(c),
            }),
            Const::ConstLong(_) => Ok(TypedExpression {
                exp_type: Type::Long,
                exp: Expr::Const(c),
            }),
            Const::ConstUInt(_) => Ok(TypedExpression {
                exp_type: Type::UInt,
                exp: Expr::Const(c),
            }),
            Const::ConstULong(_) => Ok(TypedExpression {
                exp_type: Type::ULong,
                exp: Expr::Const(c),
            }),
            Const::ConstDouble(_) => Ok(TypedExpression {
                exp_type: Type::Double,
                exp: Expr::Const(c),
            }),
        },
        ParserExpr::Cast(t, inner) => {
            let typed_inner = typecheck_exp(*inner, symbols)?;
            let cast_exp = Expr::Cast(t.clone(), Box::new(typed_inner));
            Ok(cast_exp.with_type(t))
        }
        ParserExpr::Unary(op, inner, span) => {
            let typed_inner = typecheck_exp(*inner, symbols)?;
            let inner_type = typed_inner.exp_type.clone();
            let unary_exp = Expr::Unary(op, Box::new(typed_inner));
            match op {
                UnaryOp::Not => Ok(unary_exp.with_type(Type::Int)),
                UnaryOp::BitwiseComplement if !inner_type.is_integer() => Err(SemanticError::with_span(
                    "bitwise complement '~' requires an integer operand".to_string(),
                    span,
                )),
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
        ParserExpr::Binary(op, lhs, rhs, span) => {
            // -Wdiv-by-zero: constant divisor of 0 (compiles, but SIGFPEs at runtime)
            if matches!(op, BinOp::Divide | BinOp::Remainder) {
                warn_div_by_zero(matches!(op, BinOp::Divide), &rhs, span);
            }
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            // -Wshift-count-*: width comes from the left operand's type, so this must run after
            // typecheck_lhs but before `rhs` is moved into typecheck_exp below.
            if matches!(op, BinOp::BitwiseLeftShift | BinOp::BitwiseRightShift) {
                warn_shift_count(&typed_lhs.exp_type, &rhs, span);
            }
            let typed_rhs = typecheck_exp(*rhs, symbols)?;
            if (!typed_lhs.exp_type.is_integer() || !typed_rhs.exp_type.is_integer())
                && matches!(
                    op,
                    BinOp::Remainder
                        | BinOp::BitwiseAnd
                        | BinOp::BitwiseLeftShift
                        | BinOp::BitwiseOr
                        | BinOp::BitwiseRightShift
                        | BinOp::BitwiseXOr
                )
            {
                return Err(SemanticError::with_span(
                    "bitwise, shift, and remainder operators require integer operands".to_string(),
                    span,
                ));
            }
            if matches!(op, BinOp::And | BinOp::Or) {
                let binary_exp = Expr::Binary(op, Box::new(typed_lhs), Box::new(typed_rhs));
                return Ok(binary_exp.with_type(Type::Int));
            };

            // Bitshift: result type is the type of left operand (not common type)
            if matches!(op, BinOp::BitwiseLeftShift | BinOp::BitwiseRightShift) {
                let result_type = typed_lhs.exp_type.clone();
                let binary_exp = Expr::Binary(op, Box::new(typed_lhs), Box::new(typed_rhs));
                return Ok(binary_exp.with_type(result_type));
            }

            let common_type = typed_lhs.exp_type.common_with(&typed_rhs.exp_type);
            let converted_lhs = convert_to(typed_lhs, &common_type);
            let converted_rhs = convert_to(typed_rhs, &common_type);
            let binary_exp = Expr::Binary(op, Box::new(converted_lhs), Box::new(converted_rhs));
            let result_type = match op {
                BinOp::Add
                | BinOp::Subtract
                | BinOp::Multiply
                | BinOp::Divide
                | BinOp::Remainder
                | BinOp::BitwiseAnd
                | BinOp::BitwiseOr
                | BinOp::BitwiseXOr => common_type,
                BinOp::Equal
                | BinOp::NotEqual
                | BinOp::LessThan
                | BinOp::LessOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterOrEqual => Type::Int,
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
            let common_type = typed_then_exp.exp_type.common_with(&typed_else_exp.exp_type);
            let converted_then = convert_to(typed_then_exp, &common_type);
            let converted_else = convert_to(typed_else_exp, &common_type);
            let conditional = Expr::Conditional(
                Box::new(typed_condition),
                Box::new(converted_then),
                Box::new(converted_else),
            );
            Ok(conditional.with_type(common_type))
        }
        ParserExpr::CompoundAssignment(op, lhs, rhs, span) => {
            if matches!(op, AssignOp::Divide | AssignOp::Remainder) {
                warn_div_by_zero(matches!(op, AssignOp::Divide), &rhs, span);
            }
            let typed_lhs = typecheck_exp(*lhs, symbols)?;
            if matches!(op, AssignOp::BitwiseLeftShift | AssignOp::BitwiseRightShift) {
                warn_shift_count(&typed_lhs.exp_type, &rhs, span);
            }
            let typed_rhs = typecheck_exp(*rhs, symbols)?;
            if (!typed_lhs.exp_type.is_integer() || !typed_rhs.exp_type.is_integer())
                && matches!(
                    op,
                    AssignOp::Remainder
                        | AssignOp::BitwiseAnd
                        | AssignOp::BitwiseLeftShift
                        | AssignOp::BitwiseOr
                        | AssignOp::BitwiseRightShift
                        | AssignOp::BitwiseXOr
                )
            {
                return Err(SemanticError::with_span(
                    "bitwise, shift, and remainder operators require integer operands".to_string(),
                    span,
                ));
            }
            let left_type = typed_lhs.exp_type.clone();
            let op_type = match op {
                AssignOp::BitwiseLeftShift | AssignOp::BitwiseRightShift => left_type.clone(),
                _ => typed_lhs.exp_type.common_with(&typed_rhs.exp_type),
            };
            let compound = Expr::CompoundAssignment(op, Box::new(typed_lhs), Box::new(typed_rhs), op_type);
            Ok(compound.with_type(left_type))
        }
        ParserExpr::FunctionCall(Identifier(name), param_exps, span) => {
            let f_type = &symbols
                .get(&name)
                .expect("Undefined function in typechecking")
                .symbol_type;
            let (param_types, ret_type) = match f_type {
                Type::FunType { params, ret, .. } => (params.clone(), ret.clone()),
                _ => {
                    return Err(SemanticError::with_span(
                        format!("'{}' is not a function", name.bold()),
                        span,
                    ));
                }
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
                ));
            }

            let mut converted_args = Vec::with_capacity(param_exps.len());
            for (arg, param_type) in param_exps.iter().zip(param_types.iter()) {
                let typed_arg = typecheck_exp(arg.clone(), symbols)?;
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
fn typecheck_function_declaration(
    decl: FunDeclaration,
    symbols: &mut SymbolTable,
) -> Result<Option<TypedFunction>, SemanticError> {
    let mut global = decl.storage_class != Some(StorageClass::Static);
    let mut defined = decl.body.is_some();
    let mut saved_span = decl.span;
    let mut fun_type = decl.fun_type;

    let (new_param_types, new_ret_type) = match &fun_type {
        Type::FunType { params, ret, .. } => (params.clone(), ret.clone()),
        _ => unreachable!("Function declaration must have FunType"),
    };

    if let Some(old_dec) = symbols.get(&decl.name.0) {
        match &old_dec.symbol_type {
            Type::FunType {
                params: old_params,
                ret: old_ret_type,
                defined: old_defined,
            } => {
                if *old_params != new_param_types || old_ret_type != &new_ret_type {
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
            Type::Int | Type::Long | Type::UInt | Type::ULong | Type::Double => {
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
    if let Some(body) = decl.body {
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
        let typed_body = typecheck_block(body, symbols, &new_ret_type)?;

        Ok(Some(TypedFunction {
            name: decl.name,
            params: decl.params,
            body: Some(typed_body),
            fun_type,
            global,
        }))
    } else {
        // Just a declaration, no body
        Ok(None)
    }
}

/// Type-checks a block (compound statement), processing declarations and statements in order.
///
/// Variable declarations are type-checked as locals; function declarations in blocks
/// are validated but produce no output (bodies are forbidden, enforced in the resolve phase).
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
                typecheck_function_declaration(fun, symbols)?;
            }
            ParserBlockItem::Statement(stmt) => {
                let typed_stmt = typecheck_stmt(stmt.stmt, symbols, ret_type)?;
                typed_items.push(BlockItem::Statement(typed_stmt));
            }
        }
    }
    Ok(typed_items)
}

/// Type-checks a statement, recursively processing nested expressions and sub-statements.
///
/// `ret_type` is the enclosing function's return type, used to insert implicit casts
/// on `return` expressions. Switch statements normalize case values to the controlling
/// expression's type and check for duplicates.
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
        }
        ParserStmt::If(cond, then_s, else_s) => {
            let typed_cond = typecheck_exp(cond, symbols)?;
            let converted_then = typecheck_stmt(then_s.stmt, symbols, ret_type)?;
            let typed_else = (*else_s)
                .map(|e| typecheck_stmt(e.stmt, symbols, ret_type))
                .transpose()?
                .map(Box::new);
            Ok(Stmt::If(typed_cond, Box::new(converted_then), typed_else))
        }
        ParserStmt::Goto(lbl) => Ok(Stmt::Goto(lbl)),
        ParserStmt::Null => Ok(Stmt::Null),
        ParserStmt::Break(lbl) => Ok(Stmt::Break(lbl)),
        ParserStmt::Continue(lbl) => Ok(Stmt::Continue(lbl)),
        ParserStmt::Labeled(lbl, s) => {
            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            Ok(Stmt::Labeled(lbl, Box::new(typed_s)))
        }
        ParserStmt::Default(s, lbl) => {
            let typed_s = typecheck_stmt(s.stmt, symbols, ret_type)?;
            Ok(Stmt::Default(Box::new(typed_s), lbl))
        }
        ParserStmt::Compound(block) => {
            let typed_block = typecheck_block(block, symbols, ret_type)?;
            Ok(Stmt::Compound(typed_block))
        }
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

            // The controlling expression must have integer type (C §6.8.4.2). Reject `double` here,
            // before `as_i64` normalizes case values against it (that would otherwise panic).
            if !switch_type.is_integer() {
                return Err(SemanticError {
                    message: "switch controlling expression must have integer type".to_string(),
                    span: None,
                });
            }

            // Normalize cases and check for duplicates
            let mut normalized: HashMap<Option<i64>, Span> = HashMap::new();
            for (case, case_span) in cases.iter() {
                let norm_value = case.as_i64(switch_type);
                if let Some(previous_span) = normalized.insert(norm_value, *case_span) {
                    let (message, prev_note) = match norm_value {
                        Some(v) => (
                            format!("duplicate case value '{}'", v.to_string().bold()),
                            "previous case was here",
                        ),
                        None => (
                            "multiple default labels in one switch".to_string(),
                            "previous default was here",
                        ),
                    };
                    return Err(SemanticError::with_span(
                        format!("{}\n{}: {}: {}", message, previous_span, "note".cyan(), prev_note),
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
                }
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
            Ok(Stmt::For(
                Box::new(typed_init),
                typed_cond,
                typed_post,
                Box::new(typed_body),
                lbl,
            ))
        }
    }
}

/// Resolves variable names in an expression to their unique renamed identifiers.
///
/// Mutates the AST in place, replacing each `Var` name with its renamed counterpart
/// from `variable_map`. Tracks used variables in `used_vars` (for `-Wunused-parameter`).
/// Also validates lvalue requirements: assignments and pre/postfix operators must
/// target variables. Function calls are checked for shadowing by local variables.
fn resolve_exp(
    exp: &mut ParserExpr,
    variable_map: &HashMap<Rc<str>, VarInfo>,
    used_vars: &mut HashSet<Rc<str>>,
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
        ParserExpr::Unary(_, e, _) => resolve_exp(e, variable_map, used_vars),
        ParserExpr::Binary(_, left, right, _) => {
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
    variable_map: &mut HashMap<Rc<str>, VarInfo>,
    block_vars: &mut HashSet<Rc<str>>,
    labels: &mut HashSet<Rc<str>>,
    jumps: &mut HashMap<Rc<str>, Span>,
    name_gen: &mut NameGenerator,
    used_vars: &mut HashSet<Rc<str>>,
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
    let original_params: Vec<Rc<str>> = dec.params.iter().map(|Identifier(name)| name.clone()).collect();

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
            warn_unused_parameter(used_vars.contains(param), &org_name, dec.span);
        }
    }
    Ok(())
}

fn resolve_param(
    param: &mut Identifier,
    variable_map: &mut HashMap<Rc<str>, VarInfo>,
    name_gen: &mut NameGenerator,
    block_vars: &mut HashSet<Rc<str>>,
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
        warn_shadow(&name, span, shadow.map(|s| s.span));
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
fn resolve_file_var_declaration(dec: &mut VarDeclaration, variable_map: &mut HashMap<Rc<str>, VarInfo>) {
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
/// - Initializer expression is resolved, and checked for `-Wsequence-point`, if present
///
/// This is the resolve pass counterpart to `typecheck_local_variable_declaration`.
fn resolve_local_var_decoration(
    dec: &mut VarDeclaration,
    variable_map: &mut HashMap<Rc<str>, VarInfo>,
    name_gen: &mut NameGenerator,
    block_vars: &mut HashSet<Rc<str>>,
    used_vars: &mut HashSet<Rc<str>>,
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
        let shadow = variable_map.insert(
            name.clone(),
            VarInfo {
                renamed: unique_name.clone(),
                span: dec.span,
                ext_link: false,
            },
        );
        warn_shadow(name, dec.span, shadow.map(|s| s.span));
        if let Some(init_expr) = &mut dec.init {
            check_sequence_points(init_expr);
            resolve_exp(init_expr, variable_map, used_vars)?;
        }
    }
    block_vars.insert(name.clone());
    Ok(())
}

/// Resolution-pass handler for a single statement: renames variables in any contained
/// expressions, recurses into nested statements/blocks, and tracks control-flow labels.
///
/// - Each contained full expression (conditions, `return`/expression statements, `for` clauses) is
///   renamed via [`resolve_exp`]; emits `-Wsequence-point` for unsequenced double-modifications
/// - `Labeled` registers the label name (a duplicate label is an error); `Goto` records its target
///   in `jumps` for later existence validation
/// - `Compound` and `for` introduce a new scope (a cloned `variable_map`)
fn resolve_statement(
    statement: &mut SpannedStmt,
    variable_map: &mut HashMap<Rc<str>, VarInfo>,
    labels: &mut HashSet<Rc<str>>,
    jumps: &mut HashMap<Rc<str>, Span>,
    name_gen: &mut NameGenerator,
    used_vars: &mut HashSet<Rc<str>>,
) -> Result<(), SemanticError> {
    match &mut statement.stmt {
        ParserStmt::Return(e) => {
            check_sequence_points(e);
            resolve_exp(e, variable_map, used_vars)
        }
        ParserStmt::Expression(e) => {
            check_sequence_points(e);
            resolve_exp(e, variable_map, used_vars)
        }
        ParserStmt::Null => Ok(()),
        ParserStmt::If(e, then_stmt, else_stmt) => {
            check_sequence_points(e);
            resolve_exp(e, variable_map, used_vars)?;
            resolve_statement(then_stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            match else_stmt.as_mut() {
                Some(else_stmt) => resolve_statement(else_stmt, variable_map, labels, jumps, name_gen, used_vars),
                None => Ok(()),
            }
        }
        ParserStmt::Labeled(label_name, stmt) => {
            if !labels.insert(label_name.0.clone()) {
                // TODO: add a "previous definition was here" note (like duplicate case values).
                // Requires `labels` to store spans (HashSet<Rc<str>> -> HashMap<Rc<str>, Span>).
                return Err(SemanticError::with_span(
                    format!("duplicate label '{}'", label_name.0.bold()),
                    statement.span,
                ));
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
            check_sequence_points(exp);
            resolve_exp(exp, variable_map, used_vars)?;
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)
        }
        ParserStmt::DoWhile(stmt, exp, _) => {
            resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars)?;
            check_sequence_points(exp);
            resolve_exp(exp, variable_map, used_vars)?;
            Ok(())
        }
        ParserStmt::For(init, e1, e2, stmt, _) => {
            let mut shadow_map = variable_map.clone(); // TODO: expensive clone of HashMap per for loop - same issue as compound blocks
            match init {
                ParserForInit::InitExp(exp) => {
                    if let Some(e) = exp {
                        check_sequence_points(e);
                        resolve_exp(e, &shadow_map, used_vars)?;
                    }
                }
                ParserForInit::InitDecl(dec) => {
                    resolve_local_var_decoration(dec, &mut shadow_map, name_gen, &mut HashSet::new(), used_vars)?
                }
            }
            if let Some(e1) = e1 {
                check_sequence_points(e1);
                resolve_exp(e1, &shadow_map, used_vars)?;
            }
            if let Some(e2) = e2 {
                check_sequence_points(e2);
                resolve_exp(e2, &shadow_map, used_vars)?;
            }
            resolve_statement(stmt, &mut shadow_map, labels, jumps, name_gen, used_vars)?;
            Ok(())
        }
        ParserStmt::Break(..) | ParserStmt::Continue(..) => Ok(()),
        ParserStmt::Default(stmt, ..) => resolve_statement(stmt, variable_map, labels, jumps, name_gen, used_vars),
    }
}

/// Wide-enough exact value of any integer constant, split by signedness so each half maps to a
/// lossless std primitive (no single primitive holds both `u128::MAX` and negatives). Today the
/// widest StaticInt variants are i64/u64, so those suffice — see [`StaticInit::wide`] for the
/// invariant on widening this to i128/u128.
#[derive(Clone, Copy)]
enum Wide {
    Signed(i64),
    Unsigned(u64),
}

// Double -> integer conversion, hand-rolled to match x86 `cvttsd2si` — the instruction codegen
// emits, so a compile-time fold of e.g. `(int)1e20` must agree with the same cast at runtime.
//
// Rust's standard library can't do this for us:
//   1. There is no fallible float->int conversion — `TryFrom<f64> for i32` (etc.) doesn't exist —
//      so we can't ask "does this double fit?"; we range-check by hand.
//   2. `f64::to_int_unchecked` exists but is undefined behavior out of range, so it can't be turned
//      loose on arbitrary constants during folding.
//
// That leaves the `as` cast — but `as` does NOT match the hardware. Rust's float->int `as`
// SATURATES: an out-of-range value clamps to the target's MIN/MAX and NaN becomes 0. `cvttsd2si`
// instead yields the "integer indefinite" value — the target's MIN bit pattern (e.g. INT_MIN) — for
// *every* invalid input: positive overflow, ±infinity, and NaN alike. They agree only when the
// truncated value is already in range. So: explicit range/NaN check returning the indefinite value
// on failure, and plain `as` (which truncates toward zero, matching cvttsd2si) on success.
//
// `double_to_u32`/`u64` build on the signed path because `cvttsd2si` is signed-only (see each fn).
fn double_to_i32(d: f64) -> i32 {
    // cvttsd2si: valid iff trunc(d) fits in i32; NaN/inf/overflow -> indefinite (INT_MIN)
    if d.is_nan() || d < i32::MIN as f64 || d >= 2147483648.0
    /* 2^31 */
    {
        i32::MIN
    } else {
        d as i32 // in range: truncates toward zero, matches cvttsd2si
    }
}

fn double_to_i64(d: f64) -> i64 {
    // cvttsd2si: valid iff trunc(d) fits in i64; NaN/inf/overflow -> indefinite (INT_MIN)
    if d.is_nan() || d < i64::MIN as f64 || d >= 9223372036854775808.0
    /* 2^63 */
    {
        i64::MIN
    } else {
        d as i64 // in range: truncates toward zero, matches cvttsd2si
    }
}

fn double_to_u64(d: f64) -> u64 {
    const TWO_POW_63: f64 = 9223372036854775808.0; // 2^63
    if d < TWO_POW_63 {
        double_to_i64(d) as u64 // fits signed range
    } else {
        // subtract 2^63, convert the (now in-range) remainder, restore the top bit
        (double_to_i64(d - TWO_POW_63) as u64).wrapping_add(1u64 << 63)
    }
}

fn double_to_u32(d: f64) -> u32 {
    // u32's full range fits in i64, so convert via the signed 64-bit path and truncate.
    double_to_i64(d) as u32
}

/// Casts a normalized value (`i64`/`u64`) to each target type, yielding `(result, out_of_range)`
/// where `out_of_range` means the exact value can't be represented in the target's range.
macro_rules! to_target {
    ($v:expr, $target:expr) => {
        match $target {
            Type::Int => (StaticInit::IntInit($v as i32), i32::try_from($v).is_err()),
            Type::UInt => (StaticInit::UIntInit($v as u32), u32::try_from($v).is_err()),
            Type::Long => (StaticInit::LongInit($v as i64), i64::try_from($v).is_err()),
            Type::ULong => (StaticInit::ULongInit($v as u64), u64::try_from($v).is_err()),
            Type::Double => (StaticInit::DoubleInit($v as f64), false),
            Type::FunType { .. } => unreachable!("Cannot cast to function type in constant expression"),
        }
    };
}

/// Converts a constant to `target_type`. Returns `(value, truncated)` where `truncated` is true
/// only for a *narrowing* that changed the value — the `-Wconstant-conversion`/`-Woverflow` signal.
/// Widening and same-width conversions (including sign reinterprets like `-1` -> unsigned) never set
/// it; those are gcc's separate, off-by-default `-Wsign-conversion`. The caller decides whether to
/// warn (implicit conversions do; explicit casts and internal promotion do not).
// `unnecessary_cast`/`useless_conversion`: the i64/u64 normalizer coincides with the Long/ULong
// targets today, so those arms are identity casts; they become genuine narrowings once Wide is i128/u128.
#[allow(clippy::unnecessary_cast, clippy::useless_conversion)]
fn convert_to_type(val: StaticInit, target_type: &Type) -> (StaticInit, bool) {
    if let StaticInit::DoubleInit(v) = val {
        return match target_type {
            Type::Int => (StaticInit::IntInit(double_to_i32(v)), false),
            Type::Long => (StaticInit::LongInit(double_to_i64(v)), false),
            Type::UInt => (StaticInit::UIntInit(double_to_u32(v)), false),
            Type::ULong => (StaticInit::ULongInit(double_to_u64(v)), false),
            Type::Double => (val, false),
            Type::FunType { .. } => unreachable!(),
        };
    }
    let source_bits = val.get_type().size_bits();
    let (result, out_of_range) = match val.wide() {
        Wide::Signed(v) => to_target!(v, target_type),
        Wide::Unsigned(v) => to_target!(v, target_type),
    };
    let truncated = out_of_range && target_type.size_bits() < source_bits;
    (result, truncated)
}

enum ConstEvalError {
    NotConstant,
    DivByZero,
    InvalidType,
}

/// Evaluate a static / file-scope initializer to its `InitialValue`, mapping the
/// constant-eval failures (not-constant, division by zero, invalid-type op like `~` on a
/// `double`) to located diagnostics. Shared by the local-`static` and file-scope declaration
/// paths so their error messages stay in sync.
fn eval_static_initializer(
    expr: &ParserExpr,
    var_type: &Type,
    name: &Identifier,
    span: Span,
) -> Result<InitialValue, SemanticError> {
    match eval_constant_expr(expr) {
        Ok((c, overflowed)) => {
            warn_overflow(overflowed, span);
            // Implicit narrowing conversion to the declared type: -Wconstant-conversion.
            let (converted, truncated) = convert_to_type(c, var_type);
            warn_constant_conversion(truncated, &c, &converted, span);
            Ok(InitialValue::Initial(converted))
        }
        Err(ConstEvalError::DivByZero) => Err(SemanticError::with_span(
            "division by zero in constant expression".into(),
            span,
        )),
        Err(ConstEvalError::NotConstant) => Err(SemanticError::with_span(
            format!(
                "initializer for static variable '{}' is not a constant expression",
                name.0.bold()
            ),
            span,
        )),
        Err(ConstEvalError::InvalidType) => Err(SemanticError::with_span(
            format!("cannot take bitwise complement of a {} value", "'double'".bold()),
            span,
        )),
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
/// On success returns `(value, overflowed)`, where `overflowed` is true if any sub-operation
/// wrapped the result type during the fold (`INT_MAX + 1`, `-INT_MIN`, `INT_MIN / -1`, …). The
/// value is still the well-defined wrapped result — callers use the flag to emit `-Woverflow`.
///
/// Returns `Err(ConstEvalError::NotConstant)` if the expression contains non-constant
/// elements (variables, function calls, assignments, etc.), or
/// `Err(ConstEvalError::DivByZero)` if a `/` or `%` has a zero divisor.
fn eval_constant_expr(expr: &ParserExpr) -> Result<(StaticInit, bool), ConstEvalError> {
    match expr {
        ParserExpr::Constant(Const::ConstInt(val)) => Ok((StaticInit::IntInit(*val), false)),
        ParserExpr::Constant(Const::ConstLong(val)) => Ok((StaticInit::LongInit(*val), false)),
        ParserExpr::Constant(Const::ConstUInt(val)) => Ok((StaticInit::UIntInit(*val), false)),
        ParserExpr::Constant(Const::ConstULong(val)) => Ok((StaticInit::ULongInit(*val), false)),
        ParserExpr::Constant(Const::ConstDouble(val)) => Ok((StaticInit::DoubleInit(*val), false)),
        ParserExpr::Cast(target, val) => {
            let (v, o) = eval_constant_expr(val)?;
            // Explicit cast: suppress the conversion-truncation warning (programmer intent), but
            // keep `o` so arithmetic overflow inside the cast operand still surfaces.
            let (cv, _truncated) = convert_to_type(v, target);
            Ok((cv, o))
        }
        ParserExpr::Unary(op, inner, _) => {
            let (v, o) = eval_constant_expr(inner)?;
            let (r, o2) = match op {
                UnaryOp::Negate => v.neg(),
                UnaryOp::BitwiseComplement => (
                    match v {
                        StaticInit::IntInit(n) => StaticInit::IntInit(!n),
                        StaticInit::LongInit(n) => StaticInit::LongInit(!n),
                        StaticInit::ULongInit(n) => StaticInit::ULongInit(!n),
                        StaticInit::UIntInit(n) => StaticInit::UIntInit(!n),
                        StaticInit::DoubleInit(_) => return Err(ConstEvalError::InvalidType),
                    },
                    false,
                ),
                UnaryOp::Not => (StaticInit::IntInit(v.is_zero() as i32), false),
            };
            // Unsigned wraparound is well-defined, not overflow — only signed ops warn (-Woverflow).
            let o2 = o2 && r.get_type().is_signed();
            Ok((r, o | o2))
        }
        ParserExpr::Binary(op, left, right, _) => {
            let (l, lo) = eval_constant_expr(left)?;
            let (r, ro) = eval_constant_expr(right)?;
            if (matches!(l, StaticInit::DoubleInit(_)) || matches!(r, StaticInit::DoubleInit(_)))
                && matches!(
                    op,
                    BinOp::Remainder
                        | BinOp::BitwiseAnd
                        | BinOp::BitwiseOr
                        | BinOp::BitwiseXOr
                        | BinOp::BitwiseLeftShift
                        | BinOp::BitwiseRightShift
                )
            {
                return Err(ConstEvalError::InvalidType);
            }
            let base = lo | ro;
            let (v, op_ovf) = match op {
                BinOp::Add => l.add(r),
                BinOp::Subtract => l.sub(r),
                BinOp::Multiply => l.mul(r),
                BinOp::Divide => l.div(r)?,
                BinOp::Remainder => l.rem(r)?,
                BinOp::BitwiseAnd => (l.bitwise_and(r), false),
                BinOp::BitwiseOr => (l.bitwise_or(r), false),
                BinOp::BitwiseXOr => (l.bitwise_xor(r), false),
                BinOp::BitwiseLeftShift => (l.shl(r), false),
                BinOp::BitwiseRightShift => (l.shr(r), false),
                BinOp::Equal => (l.eq(r), false),
                BinOp::NotEqual => (l.ne(r), false),
                BinOp::LessThan => (l.lt(r), false),
                BinOp::LessOrEqual => (l.le(r), false),
                BinOp::GreaterThan => (l.gt(r), false),
                BinOp::GreaterOrEqual => (l.ge(r), false),
                BinOp::And => (l.and(r), false),
                BinOp::Or => (l.or(r), false),
                BinOp::Assignment | BinOp::CompoundAssignment | BinOp::Conditional => {
                    unreachable!("only used for parsing")
                }
            };
            // Unsigned wraparound is well-defined, not overflow — only signed ops warn (-Woverflow).
            let op_ovf = op_ovf && v.get_type().is_signed();
            Ok((v, base | op_ovf))
        }
        ParserExpr::Conditional(cond, true_expr, false_expr) => {
            let (cond_val, co) = eval_constant_expr(cond)?;
            let (v, o) = if cond_val.is_zero() {
                eval_constant_expr(false_expr)?
            } else {
                eval_constant_expr(true_expr)?
            };
            Ok((v, co | o))
        }
        // Variables, assignments, function calls etc. are not constant
        _ => Err(ConstEvalError::NotConstant),
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

    fn get_break_label(&self) -> Option<Rc<str>> {
        if let Some(label) = self.cur_label.last() {
            let s = match label {
                LabelTag::Switch(l) => format!("break_switch.{l}"),
                LabelTag::Loop(l) => format!("break_loop.{l}"),
            };
            Some(Rc::from(s))
        } else {
            None
        }
    }

    fn get_continue_label(&self) -> Option<Rc<str>> {
        if let Some(LabelTag::Loop(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => false,
            LabelTag::Loop(..) => true,
        }) {
            Some(Rc::from(format!("continue_loop.{label}")))
        } else {
            None
        }
    }

    fn get_switch_case(&mut self, c: StaticInit, span: &Span) -> Result<Rc<str>, SemanticError> {
        // get the active switch id
        if let Some(LabelTag::Switch(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => true,
            LabelTag::Loop(..) => false,
        }) {
            let case_exp = match c {
                StaticInit::IntInit(v) => SwitchIntType::Int(v),
                StaticInit::LongInit(v) => SwitchIntType::Long(v),
                StaticInit::UIntInit(v) => SwitchIntType::UInt(v),
                StaticInit::ULongInit(v) => SwitchIntType::ULong(v),
                // Case labels are folded here in Pass 1, before typecheck runs, so a double case
                // (`case 1.0:`) must be rejected here — not left to Pass 2 — or it would panic.
                StaticInit::DoubleInit(_) => {
                    return Err(SemanticError::with_span(
                        "case label must be an integer constant, not a double".to_string(),
                        *span,
                    ));
                }
            };
            // Just collect cases with spans - duplicate checking happens during typecheck
            self.switch_to_cases.get_mut(label).unwrap().push((case_exp, *span));
            Ok(Rc::from(case_exp.label_str(*label)))
        } else {
            Err(SemanticError::with_span("Case outside of switch".to_string(), *span))
        }
    }

    fn get_switch_default(&mut self, span: &Span) -> Result<Rc<str>, SemanticError> {
        // get the active switch id
        if let Some(LabelTag::Switch(label)) = self.cur_label.iter().rev().find(|x| match x {
            LabelTag::Switch(..) => true,
            LabelTag::Loop(..) => false,
        }) {
            let default_case = SwitchIntType::Default;
            // Just collect with span - duplicate checking happens during typecheck
            self.switch_to_cases.get_mut(label).unwrap().push((default_case, *span));
            Ok(Rc::from(default_case.label_str(*label)))
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
        self.switch_to_cases.remove(&switch_id).unwrap()
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
        ParserStmt::While(_, body, loop_num)
        | ParserStmt::DoWhile(body, _, loop_num)
        | ParserStmt::For(_, _, _, body, loop_num) => {
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
            let value = match eval_constant_expr(exp) {
                Ok((v, overflowed)) => {
                    warn_overflow(overflowed, stmt.span);
                    v
                }
                Err(ConstEvalError::DivByZero) => {
                    return Err(SemanticError::with_span(
                        "division by zero in constant expression".to_string(),
                        stmt.span,
                    ));
                }
                Err(ConstEvalError::NotConstant) => {
                    return Err(SemanticError::with_span(
                        "Expression is not an integer constant expression".to_string(),
                        stmt.span,
                    ));
                }
                Err(ConstEvalError::InvalidType) => {
                    return Err(SemanticError::with_span(
                        format!("cannot take bitwise complement of a {} value", "'double'".bold()),
                        stmt.span,
                    ));
                }
            };
            *label = label_tracker.get_switch_case(value, &stmt.span)?;
            label_statement(s, label_tracker)
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
    block_vars: &mut HashSet<Rc<str>>,
    variable_map: &mut HashMap<Rc<str>, VarInfo>,
    labels: &mut HashSet<Rc<str>>,
    jumps: &mut HashMap<Rc<str>, Span>,
    name_gen: &mut NameGenerator,
    used_vars: &mut HashSet<Rc<str>>,
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

pub fn resolve_program(mut program: Program) -> Result<(NameGenerator, TypedProgram, SymbolTable), SemanticError> {
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
                let mut jumps: HashMap<Rc<str>, Span> = HashMap::new();
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

pub fn typecheck_program(program: Program) -> Result<(TypedProgram, SymbolTable), SemanticError> {
    let mut symbols = SymbolTable::new();
    let mut typed_declarations = Vec::with_capacity(program.declarations.len());

    for decl in program.declarations {
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
    Ok((
        TypedProgram {
            declarations: typed_declarations,
        },
        symbols,
    ))
}
