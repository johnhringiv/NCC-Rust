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
use crate::lexer::{SpannedToken, Token};
use crate::pretty::{ItfDisplay, Node, cyan, green, simple_node, yellow};
use std::collections::VecDeque;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if cfg!(target_os = "macos") { "L" } else { ".L" };
        write!(f, "{}{}", prefix, self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub struct Span {
    pub line: usize,
    pub column: usize,
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
    Constant(i64),
    Var(Identifier, Span),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>, Span),
    CompoundAssignment(AssignOp, Box<Expr>, Box<Expr>, Span),
    PostFixOp(IncDec, Box<Expr>, Span),
    PreFixOp(IncDec, Box<Expr>, Span),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
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

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Return(Expr),
    Expression(Expr),
    If(Expr, Box<Stmt>, Box<Option<Stmt>>), // if (controlling expression, then, else)
    Goto(Identifier),
    Labeled(Identifier, Box<Stmt>),
    Null,
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub name: Identifier,
    pub init: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum BlockItem {
    Statement(Stmt),
    Declaration(Declaration),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<BlockItem>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

pub struct SyntaxError {
    message: String,
}

impl SyntaxError {
    fn get_found_strs(found: Option<SpannedToken>) -> (String, String) {
        let loc_str: String = found
            .as_ref()
            .map(|t| format!(", at {}:{}", t.line, t.column))
            .unwrap_or("".to_string());
        let found_str = found
            .as_ref()
            .map(|t| t.token.to_string())
            .unwrap_or("None".to_string());
        (found_str, loc_str)
    }
    pub fn new(expected: Option<Token>, found: Option<SpannedToken>) -> Self {
        let expected = expected.as_ref().map(|t| t.to_string()).unwrap_or("None".to_string());
        let (found_str, loc_str) = Self::get_found_strs(found);
        SyntaxError {
            message: format!(r#"expected: {expected}, found: {found_str:?}{loc_str}"#),
        }
    }

    pub fn expression(found: Option<SpannedToken>) -> Self {
        let (found_str, loc_str) = Self::get_found_strs(found);
        SyntaxError {
            message: format!(r#"expected an expression <int> | <unop> <exp> | (<exp>), found: {found_str:?}{loc_str}"#),
        }
    }
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SyntaxError: {}", self.message)
    }
}

fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

fn expect(expected: &Token, tokens: &mut VecDeque<SpannedToken>) -> Result<(), SyntaxError> {
    let next = tokens.pop_front();
    match next {
        Some(token) => {
            if !variant_eq(expected, &token.token) {
                Err(SyntaxError::new(Some(expected.clone()), Some(token.clone())))
            } else {
                Ok(())
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
            Token::ConstantInt(value) => {
                tokens.pop_front();
                Ok(Expr::Constant(*value))
            }
            Token::BitwiseComplement | Token::Negation | Token::LogicalNot => {
                let operator = parse_unop(tokens)?;
                let inner_exp = parse_factor(tokens)?;
                Ok(Expr::Unary(operator, Box::from(inner_exp)))
            }
            Token::Increment | Token::Decrement => {
                tokens.pop_front();
                let inner_exp = parse_factor(tokens)?;
                let span = Span {
                    line: spanned.line,
                    column: spanned.column,
                };
                let op = if spanned.token == Token::Increment {
                    IncDec::Increment
                } else {
                    IncDec::Decrement
                };
                Ok(Expr::PreFixOp(op, Box::from(inner_exp), span))
            }
            Token::OpenParen => {
                tokens.pop_front();
                let inner_exp = parse_exp(tokens, 0)?;
                expect(&Token::CloseParen, tokens)?;
                Ok(inner_exp)
            }
            Token::Identifier(name) => {
                let span = Span {
                    line: spanned.line,
                    column: spanned.column,
                };
                tokens.pop_front();
                Ok(Expr::Var(Identifier(name.clone()), span))
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
                let span = Span {
                    line: spanned.line,
                    column: spanned.column,
                };
                expr = Expr::PostFixOp(op, Box::new(expr), span);
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
                    let span = Span {
                        line: op_token.line,
                        column: op_token.column,
                    };
                    let right = parse_exp(tokens, prec)?;
                    left = match operator {
                        BinOp::Assignment => Expr::Assignment(Box::from(left), Box::from(right), span),
                        BinOp::CompoundAssignment => Expr::CompoundAssignment(
                            AssignOp::from(&op_token.token),
                            Box::from(left),
                            Box::from(right),
                            span,
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
            line,
            column,
        }) => Ok((Identifier(value), Span { line, column })),
        x => Err(SyntaxError::new(Some(Token::Identifier("whatever".to_string())), x)),
    }
}

fn parse_statement(tokens: &mut VecDeque<SpannedToken>) -> Result<Stmt, SyntaxError> {
    let Some(next_token) = tokens.front().cloned() else {
        unreachable!("parse_statement called with no token")
    };
    match next_token.token {
        Token::ReturnKeyword => {
            tokens.pop_front();
            let exp = parse_exp(tokens, 0)?;
            expect(&Token::Semicolon, tokens)?;
            Ok(Stmt::Return(exp))
        }
        Token::Semicolon => {
            tokens.pop_front();
            Ok(Stmt::Null)
        }
        Token::IfKeyword => {
            tokens.pop_front();
            expect(&Token::OpenParen, tokens)?;
            let condition = parse_exp(tokens, 0)?;
            expect(&Token::CloseParen, tokens)?;
            let then_stmt = parse_statement(tokens)?;
            // check for else
            let else_stmt = if tokens.front().map(|t| &t.token) == Some(&Token::ElseKeyword) {
                tokens.pop_front();
                Some(parse_statement(tokens)?)
            } else {
                None
            };
            Ok(Stmt::If(condition, Box::new(then_stmt), Box::new(else_stmt)))
        }
        Token::GotoKeyword => {
            tokens.pop_front();
            let (target, _span) = parse_identifier(tokens)?;
            expect(&Token::Semicolon, tokens)?;
            Ok(Stmt::Goto(target))
        }
        Token::Identifier(label_name) => {
            // Check if it's a label (identifier followed by colon)
            if tokens.get(1).map(|t| &t.token) == Some(&Token::Colon) {
                let label = Identifier(label_name);
                tokens.pop_front(); // consume identifier
                tokens.pop_front(); // consume colon
                let stmt = parse_statement(tokens)?;
                Ok(Stmt::Labeled(label, Box::new(stmt)))
            } else {
                // It's an expression statement
                let expr = parse_exp(tokens, 0)?;
                expect(&Token::Semicolon, tokens)?;
                Ok(Stmt::Expression(expr))
            }
        }
        _ => {
            let expr = parse_exp(tokens, 0)?;
            expect(&Token::Semicolon, tokens)?;
            Ok(Stmt::Expression(expr))
        }
    }
}

fn parse_block_item(tokens: &mut VecDeque<SpannedToken>) -> Result<BlockItem, SyntaxError> {
    if tokens.front().expect("parse").token == Token::IntKeyword {
        // decoration
        let mut init = None;
        tokens.pop_front();
        let (name, span) = parse_identifier(tokens)?;
        if tokens.front().expect("parse").token == Token::Assignment {
            tokens.pop_front();
            init = Some(parse_exp(tokens, 0)?);
        }
        expect(&Token::Semicolon, tokens)?;
        Ok(BlockItem::Declaration(Declaration { name, init, span }))
    } else {
        let stmt = parse_statement(tokens)?;
        Ok(BlockItem::Statement(stmt))
    }
}

fn parse_function_definition(tokens: &mut VecDeque<SpannedToken>) -> Result<Function, SyntaxError> {
    expect(&Token::IntKeyword, tokens)?;
    let (name, _span) = parse_identifier(tokens)?;
    expect(&Token::OpenParen, tokens)?;
    expect(&Token::VoidKeyword, tokens)?;
    expect(&Token::CloseParen, tokens)?;
    expect(&Token::OpenBrace, tokens)?;
    let mut function_body = Vec::new();
    while tokens.front().is_some() && tokens.front().unwrap().token != Token::CloseBrace {
        let next_block = parse_block_item(tokens)?;
        function_body.push(next_block);
    }
    expect(&Token::CloseBrace, tokens)?;

    Ok(Function {
        name,
        body: function_body,
    })
}

pub fn parse_program(tokens: &mut VecDeque<SpannedToken>) -> Result<Program, SyntaxError> {
    let fun_def = parse_function_definition(tokens)?;
    if tokens.is_empty() {
        Ok(Program { function: fun_def })
    } else {
        Err(SyntaxError::new(None, tokens.front().cloned()))
    }
}

// AST pretty printing
simple_node!(UnaryOp);
simple_node!(BinOp);
simple_node!(AssignOp);
simple_node!(IncDec);

impl ItfDisplay for Identifier {
    fn itf_node(&self) -> Node {
        Node::leaf(green(format!("Identifier(\"{}\")", self.0)))
    }
}
impl ItfDisplay for Expr {
    fn itf_node(&self) -> Node {
        match self {
            Expr::Constant(c) => Node::leaf(yellow(format!("Constant({c})"))),
            Expr::Var(id, _span) => id.itf_node(),
            Expr::Unary(op, e) => Node::branch(cyan(format!("Unary ({op:?})")), vec![e.itf_node()]),
            Expr::Binary(op, e1, e2) => {
                Node::branch(cyan(format!("Binary ({op:?})")), vec![e1.itf_node(), e2.itf_node()])
            }
            Expr::Assignment(lhs, rhs, _span) => Node::branch(cyan("Assignment"), vec![lhs.itf_node(), rhs.itf_node()]),
            Expr::CompoundAssignment(op, lhs, rhs, _span) => Node::branch(
                cyan(format!("CompoundAssignment ({op:?})")),
                vec![lhs.itf_node(), rhs.itf_node()],
            ),
            Expr::PostFixOp(op, e, _span) => Node::branch(cyan(format!("PostFix ({op:?})")), vec![e.itf_node()]),
            Expr::PreFixOp(op, e, _span) => Node::branch(cyan(format!("PreFix ({op:?})")), vec![e.itf_node()]),
            Expr::Conditional(condition, then_expr, else_expr) => Node::branch(
                cyan("Conditional"),
                vec![
                    Node::branch("condition:", vec![condition.itf_node()]),
                    Node::branch("then:", vec![then_expr.itf_node()]),
                    Node::branch("else:", vec![else_expr.itf_node()]),
                ],
            ),
        }
    }
}
impl ItfDisplay for Stmt {
    fn itf_node(&self) -> Node {
        match self {
            Stmt::Return(expr) => Node::branch(cyan("Return"), vec![expr.itf_node()]),
            Stmt::Expression(expr) => Node::branch(cyan("Expression"), vec![expr.itf_node()]),
            Stmt::If(condition, then_stmt, else_stmt) => {
                let mut children = vec![
                    Node::branch("condition:", vec![condition.itf_node()]),
                    Node::branch("then:", vec![then_stmt.itf_node()]),
                ];
                if let Some(else_s) = else_stmt.as_ref() {
                    children.push(Node::branch("else:", vec![else_s.itf_node()]));
                }
                Node::branch(cyan("If"), children)
            }
            Stmt::Null => Node::leaf(cyan("Null")),
            Stmt::Goto(label) => Node::branch(cyan("Goto"), vec![label.itf_node()]),
            Stmt::Labeled(label, stmt) => Node::branch(cyan("Labeled"), vec![label.itf_node(), stmt.itf_node()]),
        }
    }
}
impl ItfDisplay for Declaration {
    fn itf_node(&self) -> Node {
        let name_node = Node::leaf(format!("name: {}", self.name.itf_node().text));
        match &self.init {
            Some(init_expr) => {
                let init_node = Node::branch("init:", vec![init_expr.itf_node()]);
                Node::branch(cyan("Declaration"), vec![name_node, init_node])
            }
            None => Node::branch(cyan("Declaration"), vec![name_node]),
        }
    }
}
impl ItfDisplay for BlockItem {
    fn itf_node(&self) -> Node {
        match self {
            BlockItem::Statement(stmt) => stmt.itf_node(),
            BlockItem::Declaration(decl) => decl.itf_node(),
        }
    }
}
impl ItfDisplay for Function {
    fn itf_node(&self) -> Node {
        let name_line = Node::leaf(format!("name: {}", self.name.itf_node().text));
        let mut body_node = self.body.itf_node();
        body_node.text = format!("body: {}", body_node.text);
        Node::branch(cyan("Function"), vec![name_line, body_node])
    }
}
impl ItfDisplay for Program {
    fn itf_node(&self) -> Node {
        Node::branch(cyan("Program"), vec![self.function.itf_node()])
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{Stage, get_sandler_dirs, run_tests};

    #[test]
    fn test_conditional_valid() {
        let dirs = vec!["c_programs/conditional/valid/".to_string()];
        let (passed, failed) = run_tests(&dirs, true, &Stage::Parse);
        assert_eq!(failed.len(), 0, "Failed to parse valid files: {failed:?}");
        println!("Passed: {passed}");
    }

    #[test]
    fn test_conditional_invalid() {
        let dirs = vec!["c_programs/conditional/invalid_parse/".to_string()];
        let (passed, failed) = run_tests(&dirs, false, &Stage::Parse);
        assert_eq!(failed.len(), 0, "Should have rejected invalid files: {failed:?}");
        println!("Passed: {passed}");
    }

    #[test]
    fn sandler_tests_valid() {
        let dirs = get_sandler_dirs(true, &Stage::Parse);
        let (passed, failed) = run_tests(&dirs, true, &Stage::Parse);
        assert_eq!(failed.len(), 0, "Failed to parse valid files: {failed:?}");
        println!("Passed: {passed}");
    }

    #[test]
    fn sandler_tests_invalid() {
        let dirs = get_sandler_dirs(false, &Stage::Parse);
        let (passed, failed) = run_tests(&dirs, false, &Stage::Parse);
        assert_eq!(failed.len(), 0, "Should have rejected invalid files: {failed:?}");
        println!("Passed: {passed}");
    }
}
