use std::collections::VecDeque;
use std::fmt;
use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(i64),
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
    expected: Option<Token>,
    found: Option<Token>,
}

impl SyntaxError {
    pub fn new(expected: Option<Token>, found: Option<Token>) -> Self {
        SyntaxError { expected, found}
    }
}


impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let expected = self.expected.as_ref().map(|t| t.variant_str()).unwrap_or("None".to_string());
        let found = self.found.as_ref().map(|t| t.variant_str()).unwrap_or("None".to_string());
        write!(f, "SyntaxError: expected {}, found {}", expected, found)
    }
}

fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

fn expect(expected: &Token, tokens: &mut VecDeque<Token>) -> Result<(), SyntaxError> {
    let next = tokens.pop_front();
    match next {
        Some(token) => {
            if !variant_eq(expected, &token) {
                Err(SyntaxError::new(Option::from(expected.clone()), Some(token.clone())))
            } else {
                Ok(())
            }
        },
        None => Err(SyntaxError::new(Option::from(expected.clone()), None)),
    }
}

fn parse_expr(tokens: &mut VecDeque<Token>) -> Result<Expr, SyntaxError> {
    match tokens.pop_front() {
        Some(Token::ConstantInt(value)) => Ok(Expr::Int(value)),
        x => Err(SyntaxError::new(Some(Token::ConstantInt(0)), x))
    }
}

fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<Identifier, SyntaxError> {
    match tokens.pop_front() {
        Some(Token::Identifier(value)) => Ok(Identifier(value)),
        x => Err(SyntaxError::new(Option::from(Token::Identifier("whatever".to_string())), x))
    }
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Stmt, SyntaxError> {
    expect(&Token::ReturnKeyword, tokens)?;
    let exp = parse_expr(tokens)?;
    expect(&Token::Semicolon, tokens)?;
    Ok(Stmt::Return(exp))
}

fn parse_function_definition(tokens: &mut VecDeque<Token>) -> Result<Function, SyntaxError> {
    expect(&Token::IntKeyword, tokens)?;
    let name = parse_identifier(tokens)?;
    expect(&Token::OpenParen, tokens)?;
    expect(&Token::VoidKeyword, tokens)?;
    expect(&Token::CloseParen, tokens)?;
    expect(&Token::OpenBrace, tokens)?;
    
    let body = parse_statement(tokens)?;
    expect(&Token::CloseBrace, tokens)?;
    
    Ok(Function{name, body})
}

pub fn parse_program(tokens: &mut VecDeque<Token>) -> Result<Program, SyntaxError> {
    let fun_def = parse_function_definition(tokens)?;
    if tokens.is_empty() {
        Ok(Program { function: fun_def })
    } else { 
        Err(SyntaxError::new(None, tokens.front().cloned()))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokenizer;
    use super::*;

    #[test]
    fn basic_return() {
        let mut tokens: VecDeque<Token> = crate::lexer::tests::basic_return(100).into_iter().collect();
        let ast = parse_program(&mut tokens).unwrap();
        let expected = Program {
            function: Function {
                name: Identifier("main".to_string()),
                body: Stmt::Return(Expr::Int(100)),
            },
        };
        assert_eq!(ast, expected);
    }

    fn run_parser_test_invalid(file: &str) {
        let input = std::fs::read_to_string(file).expect("Failed to read input file");
        let tokens = tokenizer(&input).unwrap();
        let mut tokens: VecDeque<Token> = tokens.into_iter().collect();
        let result = parse_program(&mut tokens);
        assert!(result.is_err());
    }
    
    #[test]
    fn end_before_expr() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/end_before_expr.c")
    }

    #[test]
    fn test_extra_junk() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/extra_junk.c");
    }

    #[test]
    fn test_invalid_function_name() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/invalid_function_name.c");
    }

    #[test]
    fn test_keyword_wrong_case() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/keyword_wrong_case.c");
    }

    #[test]
    fn test_missing_type() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/missing_type.c");
    }

    #[test]
    fn test_misspelled_keyword() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/misspelled_keyword.c");
    }

    #[test]
    fn test_no_semicolon() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/no_semicolon.c");
    }

    #[test]
    fn test_not_expression() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/not_expression.c");
    }

    #[test]
    fn test_space_in_keyword() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/space_in_keyword.c");
    }

    #[test]
    fn test_switched_parens() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/switched_parens.c");
    }

    #[test]
    fn test_unclosed_brace() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/unclosed_brace.c");
    }

    #[test]
    fn test_unclosed_paren() {
        run_parser_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_parse/unclosed_paren.c");
    }
}