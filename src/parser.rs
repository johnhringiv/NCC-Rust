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

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    BitwiseComplement,
    Negate,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Constant(i64),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
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
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Return(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub body: Stmt,
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
            .map(|t| t.token.variant_str())
            .unwrap_or("None".to_string());
        (found_str, loc_str)
    }
    pub fn new(expected: Option<Token>, found: Option<SpannedToken>) -> Self {
        let expected = expected.as_ref().map(|t| t.variant_str()).unwrap_or("None".to_string());
        let (found_str, loc_str) = Self::get_found_strs(found);
        SyntaxError {
            message: format!(r#"expected: {expected:?}, found: {found_str:?}{loc_str}"#),
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

fn parse_factor(tokens: &mut VecDeque<SpannedToken>) -> Result<Expr, SyntaxError> {
    let next_token = tokens.front().cloned();
    match next_token {
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
            Token::OpenParen => {
                tokens.pop_front();
                let inner_exp = parse_exp(tokens, 0)?;
                expect(&Token::CloseParen, tokens)?;
                Ok(inner_exp)
            }
            _ => Err(SyntaxError::expression(next_token)),
        },
        _ => Err(SyntaxError::expression(next_token)),
    }
}

fn parse_exp(tokens: &mut VecDeque<SpannedToken>, min_prec: u64) -> Result<Expr, SyntaxError> {
    let mut left = parse_factor(tokens)?;
    while let Some(operator) = parse_binop(&tokens.front()) {
        let prec = operator.precedence();
        if prec >= min_prec {
            tokens.pop_front();
            let right = parse_exp(tokens, prec + 1)?;
            left = Expr::Binary(operator, Box::from(left), Box::from(right));
        } else {
            break;
        }
    }
    Ok(left)
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

fn parse_identifier(tokens: &mut VecDeque<SpannedToken>) -> Result<Identifier, SyntaxError> {
    match tokens.pop_front() {
        Some(SpannedToken {
            token: Token::Identifier(value),
            ..
        }) => Ok(Identifier(value)),
        x => Err(SyntaxError::new(Some(Token::Identifier("whatever".to_string())), x)),
    }
}

fn parse_statement(tokens: &mut VecDeque<SpannedToken>) -> Result<Stmt, SyntaxError> {
    expect(&Token::ReturnKeyword, tokens)?;
    let exp = parse_exp(tokens, 0)?;
    expect(&Token::Semicolon, tokens)?;
    Ok(Stmt::Return(exp))
}

fn parse_function_definition(tokens: &mut VecDeque<SpannedToken>) -> Result<Function, SyntaxError> {
    expect(&Token::IntKeyword, tokens)?;
    let name = parse_identifier(tokens)?;
    expect(&Token::OpenParen, tokens)?;
    expect(&Token::VoidKeyword, tokens)?;
    expect(&Token::CloseParen, tokens)?;
    expect(&Token::OpenBrace, tokens)?;

    let body = parse_statement(tokens)?;
    expect(&Token::CloseBrace, tokens)?;

    Ok(Function { name, body })
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

impl ItfDisplay for Identifier {
    fn itf_node(&self) -> Node {
        Node::leaf(green(format!("Identifier(\"{}\")", self.0)))
    }
}
impl ItfDisplay for Expr {
    fn itf_node(&self) -> Node {
        match self {
            Expr::Constant(c) => Node::leaf(yellow(format!("Constant({c})"))),
            Expr::Unary(op, e) => Node::branch(cyan(format!("Unary ({op:?})")), vec![e.itf_node()]),
            Expr::Binary(op, e1, e2) => {
                Node::branch(cyan(format!("Binary ({op:?})")), vec![e1.itf_node(), e2.itf_node()])
            }
        }
    }
}
impl ItfDisplay for Stmt {
    fn itf_node(&self) -> Node {
        match self {
            Stmt::Return(expr) => Node::branch(cyan("Return"), vec![expr.itf_node()]),
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
    use super::*;
    use crate::lexer::tokenizer;

    #[test]
    fn basic_return() {
        let input = std::fs::read_to_string("writing-a-c-compiler-tests/tests/chapter_1/valid/multi_digit.c")
            .expect("Failed to read input file");
        let mut tokens = tokenizer(&input).unwrap();
        let ast = parse_program(&mut tokens).unwrap();
        let expected = Program {
            function: Function {
                name: Identifier("main".to_string()),
                body: Stmt::Return(Expr::Constant(100)),
            },
        };
        assert_eq!(ast, expected);
    }

    fn run_parser_test_invalid(file: &str) {
        let input = std::fs::read_to_string(file).expect("Failed to read input file");
        let mut tokens = tokenizer(&input).unwrap();
        let result = parse_program(&mut tokens);
        assert!(result.is_err());
    }

    #[test]
    fn end_before_expr() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c")
    }

    #[test]
    fn test_extra_junk() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/extra_junk.c");
    }

    #[test]
    fn test_invalid_function_name() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c");
    }

    #[test]
    fn test_keyword_wrong_case() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/keyword_wrong_case.c");
    }

    #[test]
    fn test_missing_type() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/missing_type.c");
    }

    #[test]
    fn test_misspelled_keyword() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/misspelled_keyword.c");
    }

    #[test]
    fn test_no_semicolon() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/no_semicolon.c");
    }

    #[test]
    fn test_not_expression() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/not_expression.c");
    }

    #[test]
    fn test_space_in_keyword() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/space_in_keyword.c");
    }

    #[test]
    fn test_switched_parens() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/switched_parens.c");
    }

    #[test]
    fn test_unclosed_brace() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/unclosed_brace.c");
    }

    #[test]
    fn test_unclosed_paren() {
        run_parser_test_invalid("writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/unclosed_paren.c");
    }
}
