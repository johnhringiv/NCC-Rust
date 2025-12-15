//! Parser for a subset of C using recursive descent with operator precedence climbing.
//!
//! The parser combines recursive descent for statement and declaration parsing with
//! precedence climbing for expression parsing. This hybrid approach provides good
//! performance while keeping the implementation straightforward.
//!
//! # Architecture
//!
//! - [`parse_factor`] handles primary expressions and unary operators
//! - [`parse_exp`] handles binary operators using precedence climbing
//! - Assignment operators are treated as binary operators for parsing simplicity
//!   but generate distinct AST nodes
//!
use crate::lexer::{Span, SpannedToken, Token};
use colored::*;
use std::collections::{HashSet, VecDeque};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if cfg!(target_os = "macos") { "L" } else { ".L" };
        write!(f, "{}{}", prefix, self.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    BitwiseComplement,
    Negate,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOr,
    BitwiseLeftShift,
    BitwiseRightShift,
}

impl From<&Token> for AssignOp {
    fn from(token: &Token) -> Self {
        match token {
            Token::PlusAssign => AssignOp::Add,
            Token::MinusAssign => AssignOp::Subtract,
            Token::AsteriskAssign => AssignOp::Multiply,
            Token::DivisionAssign => AssignOp::Divide,
            Token::ModulusAssign => AssignOp::Remainder,
            Token::BitwiseAndAssign => AssignOp::BitwiseAnd,
            Token::BitwiseOrAssign => AssignOp::BitwiseOr,
            Token::BitwiseXOrAssign => AssignOp::BitwiseXOr,
            Token::BitwiseLeftShiftAssign => AssignOp::BitwiseLeftShift,
            Token::BitwiseRightShiftAssign => AssignOp::BitwiseRightShift,
            _ => unreachable!(
                "AssignOp::from_token called with non-assign operator token: {:?}",
                token
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Constant(i32),
    Var(Identifier, Span),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>, Span),
    CompoundAssignment(AssignOp, Box<Expr>, Box<Expr>, Span),
    PostFixOp(IncDec, Box<Expr>, Span),
    PreFixOp(IncDec, Box<Expr>, Span),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    FunctionCall(Identifier, Vec<Expr>, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub enum IncDec {
    Increment,
    Decrement,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Subtract,
    Add,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOr,
    BitwiseLeftShift,
    BitwiseRightShift,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Assignment,         // not a binop but we include it for parsing convenience
    CompoundAssignment, // also not a binop, but used for compound assignments like +=, -=, etc.
    Conditional,
}

impl BinOp {
    fn precedence(&self) -> u64 {
        //https://en.cppreference.com/w/c/language/operator_precedence.html
        match self {
            BinOp::Multiply | BinOp::Divide | BinOp::Remainder => 50,
            BinOp::Subtract | BinOp::Add => 45,
            BinOp::BitwiseLeftShift | BinOp::BitwiseRightShift => 44,
            BinOp::LessThan | BinOp::GreaterThan | BinOp::LessOrEqual | BinOp::GreaterOrEqual => 35,
            BinOp::Equal | BinOp::NotEqual => 30,
            BinOp::BitwiseAnd => 29,
            BinOp::BitwiseXOr => 28,
            BinOp::BitwiseOr => 27,
            BinOp::And => 10,
            BinOp::Or => 5,
            BinOp::Conditional => 3,
            BinOp::Assignment | BinOp::CompoundAssignment => 1,
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
pub enum SwitchIntType {
    Int(i32),
    Default,
}

impl SwitchIntType {
    pub fn label_str(&self, switch_num: u64) -> String {
        match self {
            SwitchIntType::Int(val) => {
                if *val >= 0 {
                    format!("switch.{switch_num}_case.{val}")
                } else {
                    let c_str = &val.to_string()[1..];
                    format!("switch.{switch_num}_case.neg{c_str}")
                }
            }
            SwitchIntType::Default => {
                format!("switch.{switch_num}_default")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpannedStmt {
    pub stmt: Stmt,
    pub span: Span,
}
#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Expr),
    Expression(Expr),
    If(Expr, Box<SpannedStmt>, Box<Option<SpannedStmt>>), // if (controlling expression, then, else)
    Goto(Identifier),
    Labeled(Identifier, Box<SpannedStmt>),
    Compound(Block),
    Break(Identifier),
    Continue(Identifier),
    While(Expr, Box<SpannedStmt>, u64),   // while(condition, body, label)
    DoWhile(Box<SpannedStmt>, Expr, u64), // dowhile(body, condition, label)
    For(ForInit, Option<Expr>, Option<Expr>, Box<SpannedStmt>, u64), // for(init, condition, post, body, label)
    Switch(Expr, Box<SpannedStmt>, u64, HashSet<SwitchIntType>),
    Case(Expr, Box<SpannedStmt>, Identifier),
    Default(Box<SpannedStmt>, Identifier),
    Null,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    InitDecl(VarDeclaration),
    InitExp(Option<Expr>),
}

#[derive(PartialEq, Clone)]
pub struct VarDeclaration {
    pub name: Identifier,
    pub init: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunDeclaration {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    VarDeclaration(VarDeclaration),
    FunDeclaration(FunDeclaration),
}

impl fmt::Debug for VarDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("Declaration");
        debug_struct.field("name", &self.name);
        if let Some(ref init) = self.init {
            debug_struct.field("init", init);
        }
        debug_struct.finish()
    }
}

pub type Block = Vec<BlockItem>;

fn parse_block(tokens: &mut VecDeque<SpannedToken>) -> Result<Block, SyntaxError> {
    expect(&Token::OpenBrace, tokens)?;
    let mut body = vec![];
    while tokens.front().map(|t| &t.token) != Some(&Token::CloseBrace) {
        body.push(parse_block_item(tokens)?);
    }
    expect(&Token::CloseBrace, tokens)?;
    Ok(body)
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(SpannedStmt),
    Declaration(Declaration),
}

pub struct Program {
    pub functions: Vec<FunDeclaration>,
}

pub struct SyntaxError {
    message: String,
    span: Option<Span>,
}

impl SyntaxError {
    pub fn new(expected: Option<Token>, found: Option<SpannedToken>) -> Self {
        let expected_str = expected
            .as_ref()
            .map(|t| format!("{}", t).bold().to_string())
            .unwrap_or("end of file".to_string());
        let found_str = found
            .as_ref()
            .map(|t| format!("{}", t.token).bold().to_string())
            .unwrap_or("end of file".bold().to_string());
        let span = found.as_ref().map(|t| t.span);

        SyntaxError {
            message: format!("expected {}, found {}", expected_str, found_str),
            span,
        }
    }

    pub fn expression(found: Option<SpannedToken>) -> Self {
        let found_str = found
            .as_ref()
            .map(|t| format!("{}", t.token).bold().to_string())
            .unwrap_or("end of file".bold().to_string());
        let span = found.as_ref().map(|t| t.span);

        SyntaxError {
            message: format!("expected an expression, found {}", found_str),
            span,
        }
    }

    pub fn with_span(message: String, span: Option<Span>) -> Self {
        SyntaxError { message, span }
    }
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.span {
            Some(span) => write!(f, "{}: {}: {}", span, "SyntaxError".red(), self.message),
            None => write!(f, "{}: {}", "SyntaxError".red(), self.message),
        }
    }
}

fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

fn expect(expected: &Token, tokens: &mut VecDeque<SpannedToken>) -> Result<Span, SyntaxError> {
    let next = tokens.pop_front();
    match next {
        Some(token) => {
            if !variant_eq(expected, &token.token) {
                Err(SyntaxError::new(Some(expected.clone()), Some(token.clone())))
            } else {
                Ok(token.span)
            }
        }
        None => Err(SyntaxError::new(Some(expected.clone()), None)),
    }
}

/// Parses primary expressions and operators with precedence higher than any binary operator.
///
/// Handles:
/// - Constants and identifiers
/// - Prefix unary operators (`-`, `~`, `!`, `++`, `--`)
/// - Parenthesized expressions
/// - Postfix operators (`++`, `--`)
///
/// # Implementation Notes
///
/// Postfix operators are parsed in a loop after the primary expression to handle
/// cases like `x++++` (though this would fail validation). The loop structure
/// also makes it easy to extend with other postfix operators like array subscripts
/// or function calls in the future.
fn parse_factor(tokens: &mut VecDeque<SpannedToken>) -> Result<Expr, SyntaxError> {
    let next_token = tokens.front().cloned();
    let mut expr = match next_token {
        Some(ref spanned) => match &spanned.token {
            Token::ConstantInt(value_str) => {
                tokens.pop_front();
                match value_str.parse::<i32>() {
                    Ok(val) => Ok(Expr::Constant(val)),
                    Err(_) => Err(SyntaxError::with_span(
                        format!("Integer constant '{}' does not fit in 32-bit int", value_str.bold()),
                        Some(spanned.span),
                    )),
                }
            }
            Token::Negation => {
                // simply treating negation as an unop cases a panic when unwrapping int min
                if let Some(SpannedToken {
                    token: Token::ConstantInt(value_str),
                    span: _,
                }) = tokens.get_mut(1)
                {
                    *value_str = format!("-{value_str}");
                    tokens.pop_front();
                    parse_factor(tokens)
                } else {
                    let operator = parse_unop(tokens)?;
                    let inner_exp = parse_factor(tokens)?;
                    Ok(Expr::Unary(operator, Box::from(inner_exp)))
                }
            }
            Token::BitwiseComplement | Token::LogicalNot => {
                let operator = parse_unop(tokens)?;
                let inner_exp = parse_factor(tokens)?;
                Ok(Expr::Unary(operator, Box::from(inner_exp)))
            }
            Token::Increment | Token::Decrement => {
                tokens.pop_front();
                let inner_exp = parse_factor(tokens)?;
                let op = if spanned.token == Token::Increment {
                    IncDec::Increment
                } else {
                    IncDec::Decrement
                };
                Ok(Expr::PreFixOp(op, Box::from(inner_exp), spanned.span))
            }
            Token::OpenParen => {
                tokens.pop_front();
                let inner_exp = parse_exp(tokens, 0)?;
                expect(&Token::CloseParen, tokens)?;
                Ok(inner_exp)
            }
            Token::Identifier(name) => {
                tokens.pop_front();
                if let Some(tmp) = tokens.front().cloned() {
                    if tmp.token == Token::OpenParen {
                        tokens.pop_front();
                        // parse the function call
                        let params = parse_function_args(tokens, &Some(spanned.span))?;
                        Ok(Expr::FunctionCall(Identifier(name.clone()), params, spanned.span))
                    } else {
                        Ok(Expr::Var(Identifier(name.clone()), spanned.span))
                    }
                } else {
                    Err(SyntaxError::expression(next_token))
                }
            }
            _ => Err(SyntaxError::expression(next_token)),
        },
        _ => Err(SyntaxError::expression(next_token)),
    }?;
    // update the expression with any postfix increment/decrement operators
    while let Some(spanned) = tokens.front().cloned() {
        match &spanned.token {
            Token::Increment | Token::Decrement => {
                let op = if spanned.token == Token::Increment {
                    IncDec::Increment
                } else {
                    IncDec::Decrement
                };
                tokens.pop_front();
                expr = Expr::PostFixOp(op, Box::new(expr), spanned.span);
            }
            _ => break,
        }
    }
    Ok(expr)
}

/// Parses binary expressions using operator precedence climbing.
///
/// The `min_prec` parameter controls which operators can be parsed at this level,
/// implementing the precedence climbing algorithm. Only operators with precedence
/// >= `min_prec` are consumed.
///
/// # Algorithm
///
/// 1. Parse left operand using [`parse_factor`]
/// 2. While next token is a binary operator with precedence >= `min_prec`:
///    - For left-associative operators: recurse with `prec + 1`
///    - For right-associative operators: recurse with `prec`
///
/// # Implementation Notes
///
/// - Assignment operators receive special handling to generate `Assignment` and
///   `CompoundAssignment` AST nodes instead of `Binary` nodes. This distinction
///   is important for validation and code generation.
/// - The function assumes [`parse_factor`] handles all unary prefix operators,
///   so any identifier or constant at this level is a valid left operand.
fn parse_exp(tokens: &mut VecDeque<SpannedToken>, min_prec: u64) -> Result<Expr, SyntaxError> {
    let mut left = parse_factor(tokens)?;
    while let Some(operator) = parse_binop(&tokens.front()) {
        let prec = operator.precedence();
        if prec >= min_prec {
            match operator {
                BinOp::Assignment | BinOp::CompoundAssignment => {
                    let op_token = tokens.pop_front().unwrap();
                    let right = parse_exp(tokens, prec)?;
                    left = match operator {
                        BinOp::Assignment => Expr::Assignment(Box::from(left), Box::from(right), op_token.span),
                        BinOp::CompoundAssignment => Expr::CompoundAssignment(
                            AssignOp::from(&op_token.token),
                            Box::from(left),
                            Box::from(right),
                            op_token.span,
                        ),
                        _ => unreachable!("Already matched on operator"),
                    }
                }
                BinOp::Conditional => {
                    tokens.pop_front().unwrap();
                    let middle = parse_conditional_middle(tokens)?;
                    let right = parse_exp(tokens, prec)?;
                    left = Expr::Conditional(Box::from(left), Box::from(middle), Box::from(right))
                }
                _ => {
                    tokens.pop_front();
                    let right = parse_exp(tokens, prec + 1)?;
                    left = Expr::Binary(operator, Box::from(left), Box::from(right));
                }
            }
        } else {
            break;
        }
    }
    Ok(left)
}

fn parse_conditional_middle(tokens: &mut VecDeque<SpannedToken>) -> Result<Expr, SyntaxError> {
    let e = parse_exp(tokens, 0)?;
    expect(&Token::Colon, tokens)?;
    Ok(e)
}

fn parse_binop(next_token: &Option<&SpannedToken>) -> Option<BinOp> {
    match next_token {
        Some(spanned) => match spanned.token {
            Token::Negation => Some(BinOp::Subtract),
            Token::Plus => Some(BinOp::Add),
            Token::Asterisk => Some(BinOp::Multiply),
            Token::Division => Some(BinOp::Divide),
            Token::Modulus => Some(BinOp::Remainder),
            Token::BitwiseAnd => Some(BinOp::BitwiseAnd),
            Token::BitwiseOr => Some(BinOp::BitwiseOr),
            Token::BitwiseXOr => Some(BinOp::BitwiseXOr),
            Token::BitwiseLeftShift => Some(BinOp::BitwiseLeftShift),
            Token::BitwiseRightShift => Some(BinOp::BitwiseRightShift),
            Token::LessThan => Some(BinOp::LessThan),
            Token::GreaterThan => Some(BinOp::GreaterThan),
            Token::LessThanOrEqual => Some(BinOp::LessOrEqual),
            Token::GreaterThanOrEqual => Some(BinOp::GreaterOrEqual),
            Token::Equal => Some(BinOp::Equal),
            Token::NotEqual => Some(BinOp::NotEqual),
            Token::LogicalAnd => Some(BinOp::And),
            Token::LogicalOr => Some(BinOp::Or),
            Token::Assignment => Some(BinOp::Assignment),
            Token::PlusAssign
            | Token::MinusAssign
            | Token::AsteriskAssign
            | Token::DivisionAssign
            | Token::ModulusAssign
            | Token::BitwiseAndAssign
            | Token::BitwiseOrAssign
            | Token::BitwiseXOrAssign
            | Token::BitwiseLeftShiftAssign
            | Token::BitwiseRightShiftAssign => Some(BinOp::CompoundAssignment),
            Token::QuestionMark => Some(BinOp::Conditional),
            _ => None,
        },
        _ => None,
    }
}

fn parse_unop(tokens: &mut VecDeque<SpannedToken>) -> Result<UnaryOp, SyntaxError> {
    let spanned = tokens.pop_front().expect("parse_unop called with no token");
    match &spanned.token {
        Token::BitwiseComplement => Ok(UnaryOp::BitwiseComplement),
        Token::Negation => Ok(UnaryOp::Negate),
        Token::LogicalNot => Ok(UnaryOp::Not),
        _ => unreachable!("parse_unop called with non-unary operator token: {:?}", spanned.token),
    }
}

fn parse_identifier(tokens: &mut VecDeque<SpannedToken>) -> Result<(Identifier, Span), SyntaxError> {
    match tokens.pop_front() {
        Some(SpannedToken {
            token: Token::Identifier(value),
            span,
        }) => Ok((Identifier(value), span)),
        x => Err(SyntaxError::new(Some(Token::Identifier("whatever".to_string())), x)),
    }
}

fn parse_statement(tokens: &mut VecDeque<SpannedToken>) -> Result<SpannedStmt, SyntaxError> {
    let Some(next_token) = tokens.front().cloned() else {
        unreachable!("parse_statement called with no token")
    };
    match next_token.token {
        Token::ReturnKeyword => {
            let span = tokens.pop_front().unwrap().span;
            let exp = parse_exp(tokens, 0)?;
            expect(&Token::Semicolon, tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::Return(exp),
                span,
            })
        }
        Token::Semicolon => {
            let span = tokens.pop_front().unwrap().span;
            Ok(SpannedStmt { stmt: Stmt::Null, span })
        }
        Token::IfKeyword => {
            let span = tokens.pop_front().unwrap().span;
            let condition = get_condition(tokens)?;
            let then_stmt = parse_statement(tokens)?;
            // check for else
            let else_stmt = if tokens.front().map(|t| &t.token) == Some(&Token::ElseKeyword) {
                tokens.pop_front();
                Some(parse_statement(tokens)?)
            } else {
                None
            };
            let stmt = Stmt::If(condition, Box::new(then_stmt), Box::new(else_stmt));
            Ok(SpannedStmt { stmt, span })
        }
        Token::GotoKeyword => {
            let span = tokens.pop_front().unwrap().span;
            let (target, _) = parse_identifier(tokens)?;
            expect(&Token::Semicolon, tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::Goto(target),
                span,
            })
        }
        Token::Identifier(label_name) => {
            // Check if it's a label (identifier followed by colon)
            if tokens.get(1).map(|t| &t.token) == Some(&Token::Colon) {
                let label = Identifier(label_name);
                let span = tokens.pop_front().unwrap().span; // consume identifier
                tokens.pop_front(); // consume colon
                declaration_check(tokens)?;
                let stmt = parse_statement(tokens)?;
                Ok(SpannedStmt {
                    stmt: Stmt::Labeled(label, Box::new(stmt)),
                    span,
                })
            } else {
                // It's an expression statement
                let expr = parse_exp(tokens, 0)?;
                let span = expect(&Token::Semicolon, tokens)?;
                Ok(SpannedStmt {
                    stmt: Stmt::Expression(expr),
                    span,
                })
            }
        }
        Token::OpenBrace => {
            Ok(SpannedStmt {
                stmt: Stmt::Compound(parse_block(tokens)?),
                span: next_token.span,
            })
        }
        Token::BreakKeyword => {
            let span = tokens.pop_front().unwrap().span;
            expect(&Token::Semicolon, tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::Break(Identifier("_dummy".to_string())),
                span,
            })
        }
        Token::ContinueKeyword => {
            let span = tokens.pop_front().unwrap().span;
            expect(&Token::Semicolon, tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::Continue(Identifier("_dummy".to_string())),
                span,
            })
        }
        Token::WhileKeyword => {
            let span = tokens.pop_front().unwrap().span;
            let condition = get_condition(tokens)?;
            let body = parse_statement(tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::While(condition, Box::from(body), 0),
                span,
            })
        }
        Token::DoKeyword => {
            let span = tokens.pop_front().unwrap().span;
            let body = parse_statement(tokens)?;
            expect(&Token::WhileKeyword, tokens)?;
            let condition = get_condition(tokens)?;
            expect(&Token::Semicolon, tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::DoWhile(Box::from(body), condition, 0),
                span,
            })
        }
        Token::ForKeyword => {
            let span = tokens.pop_front().unwrap().span;
            let err_span = expect(&Token::OpenParen, tokens)?;
            let init = if let Some(dec) = parse_declaration(tokens, &Some(err_span))? {
                match dec {
                    Declaration::VarDeclaration(var) => ForInit::InitDecl(var),
                    Declaration::FunDeclaration(_) => {
                        return  Err(SyntaxError::with_span(
                            "function declaration not allowed in for init".to_string(),
                            Some(err_span),
                        ));
                    }
                }
            } else {
                let init = parse_exp(tokens, 0).ok();
                expect(&Token::Semicolon, tokens)?;
                ForInit::InitExp(init)
            };
            let condition = parse_exp(tokens, 0).ok();
            expect(&Token::Semicolon, tokens)?;
            let post = parse_exp(tokens, 0).ok();
            expect(&Token::CloseParen, tokens)?;
            let body = parse_statement(tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::For(init, condition, post, Box::from(body), 0),
                span,
            })
        }
        Token::CaseKeyword => {
            let span = tokens.pop_front().unwrap().span;
            let exp = parse_exp(tokens, 0)?;
            expect(&Token::Colon, tokens)?;
            declaration_check(tokens)?;
            let stmt = parse_statement(tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::Case(exp, Box::from(stmt), Identifier("_dummy".to_string())),
                span,
            })
        }
        Token::DefaultKeyword => {
            let span = tokens.pop_front().unwrap().span;
            expect(&Token::Colon, tokens)?;
            declaration_check(tokens)?;
            let stmt = parse_statement(tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::Default(Box::from(stmt), Identifier("_dummy".to_string())),
                span,
            })
        }
        Token::SwitchKeyword => {
            let span = tokens.pop_front().unwrap().span;
            expect(&Token::OpenParen, tokens)?;
            let exp = parse_exp(tokens, 0)?;
            expect(&Token::CloseParen, tokens)?;
            let stmt = parse_statement(tokens)?;
            // check for stmt before first case
            switch_unreachable_check(&stmt);
            Ok(SpannedStmt {
                stmt: Stmt::Switch(exp, Box::from(stmt), 0, HashSet::new()),
                span,
            })
        }
        _ => {
            let expr = parse_exp(tokens, 0)?;
            let span = expect(&Token::Semicolon, tokens)?;
            Ok(SpannedStmt {
                stmt: Stmt::Expression(expr),
                span,
            })
        }
    }
}

fn switch_unreachable_check(stmt: &SpannedStmt) {
    match &stmt.stmt {
        Stmt::Case(..) | Stmt::Default(..) | Stmt::Null => {}
        Stmt::Compound(block) => {
            if let Some(BlockItem::Statement(s)) = block.first() {
                switch_unreachable_check(s)
            }
        }
        _ => eprintln!(
            "{}: {}: statement will never be executed {}",
            stmt.span,
            "warning".purple(),
            "[-Wswitch-unreachable]".purple()
        ),
    }
}

fn declaration_check(tokens: &VecDeque<SpannedToken>) -> Result<(), SyntaxError> {
    if tokens.front().map(|t| &t.token) == Some(&Token::IntKeyword) {
        Err(SyntaxError::with_span(
            "A label can only be part of a statement and a declaration is not a statement. Add a statement or ';' before the declaration.".to_string(),
            Some(tokens.front().unwrap().span),
        ))
    } else {
        Ok(())
    }
}

fn get_condition(tokens: &mut VecDeque<SpannedToken>) -> Result<Expr, SyntaxError> {
    expect(&Token::OpenParen, tokens)?;
    let condition = parse_exp(tokens, 0)?;
    expect(&Token::CloseParen, tokens)?;
    Ok(condition)
}

fn parse_block_item(tokens: &mut VecDeque<SpannedToken>) -> Result<BlockItem, SyntaxError> {
    if let Some(dec) = parse_declaration(tokens, &None)? {
        Ok(BlockItem::Declaration(dec))
    } else {
        let spanned_stmt = parse_statement(tokens)?;
        Ok(BlockItem::Statement(spanned_stmt))
    }
}

fn parse_declaration(
    tokens: &mut VecDeque<SpannedToken>,
    err_span: &Option<Span>,
) -> Result<Option<Declaration>, SyntaxError> {
    if let Some(front) = tokens.front() {
        if front.token == Token::IntKeyword {
            tokens.pop_front();
            let (name, span) = parse_identifier(tokens)?;
            if let Some(front) = tokens.front() {
                if tokens.front().map(|t| &t.token) == Some(&Token::OpenParen) { // function declaration
                    tokens.pop_front();
                    let params = parse_function_params(tokens, &Some(span))?;

                    let body = if tokens.front().map(|t| &t.token) == Some(&Token::OpenBrace) {
                        Some(parse_block(tokens)?)
                    } else {
                        expect(&Token::Semicolon, tokens)?;
                        None
                    };

                    Ok(Some(Declaration::FunDeclaration(FunDeclaration {
                        name,
                        params,
                        body,
                        span,
                    })))

                } else {
                    let mut init = None;
                    if front.token == Token::Assignment {
                        tokens.pop_front();
                        init = Some(parse_exp(tokens, 0)?);
                    }
                    expect(&Token::Semicolon, tokens)?;
                    Ok(Some(Declaration::VarDeclaration(VarDeclaration { name, init, span })))
                }


            } else {
                Err(SyntaxError::with_span(
                    "EOF when parsing declaration".to_string(),
                    Some(span),
                ))
            }
        } else {
            Ok(None)
        }
    } else {
        Err(SyntaxError::with_span(
            "EOF when parsing declaration".to_string(),
            *err_span,
        ))
    }
}

fn parse_function_params(
    tokens: &mut VecDeque<SpannedToken>,
    err_span: &Option<Span>,
) -> Result<Vec<Identifier>, SyntaxError> {
    let mut params = vec![];
    if let Some(front) = tokens.front().cloned() {
        if front.token == Token::VoidKeyword {
            tokens.pop_front();
        } else {
            loop {
                if tokens.front().map(|t| &t.token) == Some(&Token::CloseParen) {
                    break;
                }
                expect(&Token::IntKeyword, tokens)?;
                let (name, _span) = parse_identifier(tokens)?;
                params.push(name.clone());
                // we expect comma or close
                if let Some(next_token) = tokens.front().cloned() {
                    if next_token.token == Token::Comma {
                        tokens.pop_front();
                        if tokens.front().map(|t| &t.token) == Some(&Token::CloseParen) {
                            return Err(SyntaxError::with_span("Trailing comma not allowed.".to_string(), Some(next_token.span)));
                        }
                    }
                }
            }
        }
        expect(&Token::CloseParen, tokens)?;
        Ok(params)
    } else {
        Err(SyntaxError::with_span(
            "EOF when parsing function parameters".to_string(),
            *err_span,
        ))
    }
}

fn parse_function_args(
    tokens: &mut VecDeque<SpannedToken>,
    err_span: &Option<Span>,
) -> Result<Vec<Expr>, SyntaxError> {
    let mut params = vec![];
    if let Some(front) = tokens.front().cloned() {
        if front.token == Token::VoidKeyword {
            tokens.pop_front();
        } else {
            loop {
                if tokens.front().map(|t| &t.token) == Some(&Token::CloseParen) {
                    break;
                }
                params.push(parse_exp(tokens, 0)?);
                // we expect comma or close
                if let Some(next_token) = tokens.front().cloned() {
                    if next_token.token == Token::Comma {
                        tokens.pop_front();
                        if tokens.front().map(|t| &t.token) == Some(&Token::CloseParen) {
                            return Err(SyntaxError::with_span("Trailing comma not allowed.".to_string(), Some(next_token.span)));
                        }
                    }
                }
            }
        }
        expect(&Token::CloseParen, tokens)?;
        Ok(params)
    } else {
        Err(SyntaxError::with_span(
            "EOF when parsing function parameters".to_string(),
            *err_span,
        ))
    }
}

fn parse_function_definition(tokens: &mut VecDeque<SpannedToken>) -> Result<FunDeclaration, SyntaxError> {
    expect(&Token::IntKeyword, tokens)?;
    let (name, span) = parse_identifier(tokens)?;
    expect(&Token::OpenParen, tokens)?;
    let params = parse_function_params(tokens, &Some(span))?;

    let body = if tokens.front().map(|t| &t.token) == Some(&Token::OpenBrace) {
        Some(parse_block(tokens)?)
    } else {
        expect(&Token::Semicolon, tokens)?;
        None
    };

    Ok(FunDeclaration {
        name,
        params,
        body,
        span,
    })
}

pub fn parse_program(tokens: &mut VecDeque<SpannedToken>) -> Result<Program, SyntaxError> {
    let mut functions = vec![parse_function_definition(tokens)?];
    while !tokens.is_empty() {
        functions.push(parse_function_definition(tokens)?)
    }
    Ok(Program{ functions })
}
