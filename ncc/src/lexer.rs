use std::fmt;
use regex::Regex;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    ConstantInt(i64),
    IntKeyword,
    VoidKeyword,
    ReturnKeyword,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

impl Token {
    pub fn variant_str(&self) -> String {
        match self {
            Token::Identifier(_) => "Identifier".to_string(),
            Token::ConstantInt(_) => "ConstantInt".to_string(),
            other => format!("{:?}", other),
        }
    }
    
    pub fn len(&self) -> usize {
        match self {
            Token::Identifier(s) => s.len(),
            Token::ConstantInt(i) => i.to_string().len(),
            Token::IntKeyword => 3,
            Token::VoidKeyword => 4,
            Token::ReturnKeyword => 6,
            Token::OpenParen | Token::CloseParen | Token::OpenBrace | Token::CloseBrace | Token::Semicolon => 1,
        }
    }
}
struct TokenMatch {
    token: Token,
    length: usize,
}

struct TokenDef {
    regex: Regex,
    variant: Token,
}

fn next_token(input: &str) -> Result<Token, LexerError> {
    let mut matches = vec![];
    for token_def in &[
        TokenDef { regex: Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(), variant: Token::Identifier(String::new()) },
        TokenDef { regex: Regex::new(r"^[0-9]+\b").unwrap(), variant: Token::ConstantInt(0) },
        TokenDef { regex: Regex::new(r"^int\b").unwrap(), variant: Token::IntKeyword },
        TokenDef { regex: Regex::new(r"^void\b").unwrap(), variant: Token::VoidKeyword },
        TokenDef { regex: Regex::new(r"^return\b").unwrap(), variant: Token::ReturnKeyword },
        TokenDef { regex: Regex::new(r"^\(").unwrap(), variant: Token::OpenParen },
        TokenDef { regex: Regex::new(r"^\)").unwrap(), variant: Token::CloseParen },
        TokenDef { regex: Regex::new(r"^\{").unwrap(), variant: Token::OpenBrace },
        TokenDef { regex: Regex::new(r"^}").unwrap(), variant: Token::CloseBrace },
        TokenDef { regex: Regex::new(r"^;").unwrap(), variant: Token::Semicolon },
    ] {
        if let Some(mat) = token_def.regex.find(input) {
            let token = match &token_def.variant {
                Token::Identifier(_) => Token::Identifier(mat.as_str().to_string()),
                Token::ConstantInt(_) => Token::ConstantInt(mat.as_str().parse().unwrap()),
                other => other.clone(),
            };
            matches.push(TokenMatch {
                token,
                length: mat.end(),
            });
        }
    }
    if let Some(best_match) = matches.iter().max_by_key(|m| m.length) {
        Ok(best_match.token.clone())
    } else {
        Err(LexerError::new(format!("No token matched on: '{}'", input)))
    }
}

#[derive(Clone)]
pub struct LexerError {
    message: String,
}

impl LexerError {
    fn new(message: String) -> Self {
        LexerError {
            message
        }
    }
}

impl fmt::Debug for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LexerError {{ message: \"{}\" }}", self.message)
    }
}

//todo report line number of the error
pub(crate) fn tokenizer(mut input: &str) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    // let total_lines = input.matches('\n').count();
    while !input.is_empty() {
        input = input.trim_start();
        // check for comments and skip them need to support both // and /* */
        if input.starts_with("//") {
            if let Some(newline_index) = input.find('\n') {
                input = &input[newline_index + 1..];
            }
            continue;
        } else if input.starts_with("/*") {
            if let Some(end_comment_index) = input.find("*/") {
                input = &input[end_comment_index + 2..];
            } else {
                return Err(LexerError::new("Unterminated comment".to_string()));
            }
            continue;
        }

        if input.is_empty() {
            break;
        }
        
        // let cur_line = total_lines - input.matches('\n').count();

        match next_token(&input) {
            Ok(token) => {
                input = &input[token.len()..];
                tokens.push(token);
            }
            Err(e) => return Err(e),
        }
    }
    Ok(tokens)
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    pub(crate) fn basic_return(ret: i64) -> Vec<Token> {
        vec![
            Token::IntKeyword,
            Token::Identifier("main".to_string()),
            Token::OpenParen,
            Token::VoidKeyword,
            Token::CloseParen,
            Token::OpenBrace,
            Token::ReturnKeyword,
            Token::ConstantInt(ret),
            Token::Semicolon,
            Token::CloseBrace,
        ]
    }

    fn run_lexer_test_valid(file: &str, expected: Vec<Token>) {
        let input = std::fs::read_to_string(file).expect("Failed to read input file");
        let tokens = tokenizer(&input).expect("Lexer failed");
        assert_eq!(tokens, expected);
    }

    fn run_lexer_test_invalid(file: &str) {
        let input = std::fs::read_to_string(file).expect("Failed to read input file");
        assert!(tokenizer(&input).is_err());
    }

    #[test]
    fn test_lexer_multi_digit() {
        run_lexer_test_valid("../writing-a-c-compiler-tests/tests/chapter_1/valid/multi_digit.c", basic_return(100));
    }

    #[test]
    fn test_lexer_newlines() {
        run_lexer_test_valid("../writing-a-c-compiler-tests/tests/chapter_1/valid/newlines.c", basic_return(0))
    }

    #[test]
    fn test_lexer_no_newlines() {
        run_lexer_test_valid("../writing-a-c-compiler-tests/tests/chapter_1/valid/no_newlines.c", basic_return(0))
    }

    #[test]
    fn test_lexer_return_0() {
        run_lexer_test_valid("../writing-a-c-compiler-tests/tests/chapter_1/valid/return_0.c", basic_return(0))
    }

    #[test]
    fn test_lexer_return_2() {
        run_lexer_test_valid("../writing-a-c-compiler-tests/tests/chapter_1/valid/return_2.c", basic_return(2))
    }

    #[test]
    fn test_lexer_spaces() {
        run_lexer_test_valid("../writing-a-c-compiler-tests/tests/chapter_1/valid/spaces.c", basic_return(0))
    }

    #[test]
    fn test_lexer_tabs() {
        run_lexer_test_valid("../writing-a-c-compiler-tests/tests/chapter_1/valid/tabs.c", basic_return(0))
    }

    #[test]
    fn test_lexer_at_sign() {
        run_lexer_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/at_sign.c")
    }

    #[test]
    fn test_lexer_backslash() {
        run_lexer_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/backslash.c")
    }

    #[test]
    fn test_lexer_backtick() {
        run_lexer_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/backtick.c")
    }

    #[test]
    fn test_lexer_invalid_identifier() {
        run_lexer_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/invalid_identifier.c")
    }

    #[test]
    fn test_lexer_invalid_identifier2() {
        run_lexer_test_invalid("../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/invalid_identifier_2.c")
    }
}