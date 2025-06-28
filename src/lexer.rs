use regex::Regex;
use std::collections::VecDeque;
use std::fmt;
use std::sync::LazyLock;

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
    BitwiseComplement,  // ~
    Negation,           // -
    Decrement,          // --
    Plus,               // +
    Asterisk,           // *
    Division,           // /
    Modulus,            // %
    BitwiseAnd,         // &
    BitwiseOr,          // |
    BitwiseXOr,         // ^
    BitwiseLeftShift,   // <<
    BitwiseRightShift,  // >>
    LogicalNot,         // !
    LogicalAnd,         // &&
    LogicalOr,          // ||
    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=
}

static TOKEN_DEFS: LazyLock<Vec<TokenDef>, fn() -> Vec<TokenDef>> = LazyLock::new(|| {
    vec![
        TokenDef {
            regex: Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(),
            variant: Token::Identifier(String::new()),
        },
        TokenDef {
            regex: Regex::new(r"^[0-9]+\b").unwrap(),
            variant: Token::ConstantInt(0),
        },
        TokenDef {
            regex: Regex::new(r"^int\b").unwrap(),
            variant: Token::IntKeyword,
        },
        TokenDef {
            regex: Regex::new(r"^void\b").unwrap(),
            variant: Token::VoidKeyword,
        },
        TokenDef {
            regex: Regex::new(r"^return\b").unwrap(),
            variant: Token::ReturnKeyword,
        },
        TokenDef {
            regex: Regex::new(r"^\(").unwrap(),
            variant: Token::OpenParen,
        },
        TokenDef {
            regex: Regex::new(r"^\)").unwrap(),
            variant: Token::CloseParen,
        },
        TokenDef {
            regex: Regex::new(r"^\{").unwrap(),
            variant: Token::OpenBrace,
        },
        TokenDef {
            regex: Regex::new(r"^}").unwrap(),
            variant: Token::CloseBrace,
        },
        TokenDef {
            regex: Regex::new(r"^;").unwrap(),
            variant: Token::Semicolon,
        },
        TokenDef {
            regex: Regex::new(r"^~").unwrap(),
            variant: Token::BitwiseComplement,
        },
        TokenDef {
            regex: Regex::new(r"^--").unwrap(),
            variant: Token::Decrement,
        },
        TokenDef {
            regex: Regex::new(r"^-").unwrap(),
            variant: Token::Negation,
        },
        TokenDef {
            regex: Regex::new(r"^\+").unwrap(),
            variant: Token::Plus,
        },
        TokenDef {
            regex: Regex::new(r"^\*").unwrap(),
            variant: Token::Asterisk,
        },
        TokenDef {
            regex: Regex::new(r"^/").unwrap(),
            variant: Token::Division,
        },
        TokenDef {
            regex: Regex::new(r"^%").unwrap(),
            variant: Token::Modulus,
        },
        TokenDef {
            regex: Regex::new(r"^&").unwrap(),
            variant: Token::BitwiseAnd,
        },
        TokenDef {
            regex: Regex::new(r"^\|").unwrap(),
            variant: Token::BitwiseOr,
        },
        TokenDef {
            regex: Regex::new(r"^\^").unwrap(),
            variant: Token::BitwiseXOr,
        },
        TokenDef {
            regex: Regex::new(r"^<<").unwrap(),
            variant: Token::BitwiseLeftShift,
        },
        TokenDef {
            regex: Regex::new(r"^>>").unwrap(),
            variant: Token::BitwiseRightShift,
        },
        TokenDef {
            regex: Regex::new(r"^!").unwrap(),
            variant: Token::LogicalNot,
        },
        TokenDef {
            regex: Regex::new(r"^&&").unwrap(),
            variant: Token::LogicalAnd,
        },
        TokenDef {
            regex: Regex::new(r"^\|\|").unwrap(),
            variant: Token::LogicalOr,
        },
        TokenDef {
            regex: Regex::new(r"^==").unwrap(),
            variant: Token::Equal,
        },
        TokenDef {
            regex: Regex::new(r"^!=").unwrap(),
            variant: Token::NotEqual,
        },
        TokenDef {
            regex: Regex::new(r"^<=").unwrap(),
            variant: Token::LessThanOrEqual,
        },
        TokenDef {
            regex: Regex::new(r"^>=").unwrap(),
            variant: Token::GreaterThanOrEqual,
        },
        TokenDef {
            regex: Regex::new(r"^<").unwrap(),
            variant: Token::LessThan,
        },
        TokenDef {
            regex: Regex::new(r"^>").unwrap(),
            variant: Token::GreaterThan,
        },
    ]
});

impl Token {
    pub fn variant_str(&self) -> String {
        match self {
            Token::Identifier(_) => "Identifier".to_string(),
            Token::ConstantInt(_) => "ConstantInt".to_string(),
            other => format!("{:?}", other),
        }
    }
}

#[derive(Clone)]
struct TokenMatch {
    token: Token,
    length: usize,
}

struct TokenDef {
    regex: Regex,
    variant: Token,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub line: usize,
    pub column: usize,
}

fn next_token(input: &str) -> Result<TokenMatch, LexerError> {
    let mut matches = vec![];
    for TokenDef { regex, variant } in TOKEN_DEFS.iter() {
        if let Some(mat) = regex.find(input) {
            let token = match variant {
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
        Ok(best_match.clone())
    } else {
        Err(LexerError::new(format!("No token matched on: '{}'", input)))
    }
}

pub struct LexerError {
    message: String,
}

impl LexerError {
    fn new(message: String) -> Self {
        LexerError { message }
    }
}

impl fmt::Debug for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LexerError {{ message: \"{}\" }}", self.message)
    }
}

//todo report line number of the error
pub(crate) fn tokenizer(mut input: &str) -> Result<VecDeque<SpannedToken>, LexerError> {
    let mut tokens = VecDeque::new();
    let mut line = 1;
    let mut col = 1;
    while !input.is_empty() {
        let trimmed = input.trim_start_matches([' ', '\t']);
        col += input.len() - trimmed.len();
        input = trimmed;

        // check for comments and skip them need to support both // and /* */
        // also skipping # for now
        if ["//", "\n", "#"].iter().any(|s| input.starts_with(s)) {
            // same line, col logic
            if let Some(newline_index) = input.find('\n') {
                line += 1;
                col = 1;
                input = &input[newline_index + 1..];
            }
            continue;
        } else if input.starts_with("/*") {
            if let Some(end_comment_index) = input.find("*/") {
                let comment = &input[..end_comment_index + 2];
                let newline_count = comment.matches('\n').count();
                if newline_count > 0 {
                    line += newline_count;
                    // Set col to the position after the last newline in the comment
                    if let Some(last_newline) = comment.rfind('\n') {
                        col = comment.len() - last_newline;
                    } else {
                        col += comment.len();
                    }
                } else {
                    col += comment.len();
                }
                input = &input[end_comment_index + 2..];
            } else {
                return Err(LexerError::new("Unterminated comment".to_string()));
            }
            continue;
        }

        if input.is_empty() {
            break;
        }

        match next_token(&input) {
            Ok(TokenMatch { token, length }) => {
                tokens.push_back(SpannedToken {
                    token,
                    line,
                    column: col,
                });
                col += length; // no newline in tokens
                input = &input[length..];
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
        let tokens = tokenizer(&input)
            .expect("Lexer failed")
            .iter()
            .map(|t| t.token.clone())
            .collect::<Vec<_>>();
        assert_eq!(tokens, expected);
    }

    fn run_lexer_test_invalid(file: &str) {
        let input = std::fs::read_to_string(file).expect("Failed to read input file");
        assert!(tokenizer(&input).is_err());
    }

    #[test]
    fn test_lexer_multi_digit() {
        run_lexer_test_valid(
            "../writing-a-c-compiler-tests/tests/chapter_1/valid/multi_digit.c",
            basic_return(100),
        );
    }

    #[test]
    fn test_lexer_newlines() {
        run_lexer_test_valid(
            "../writing-a-c-compiler-tests/tests/chapter_1/valid/newlines.c",
            basic_return(0),
        )
    }

    #[test]
    fn test_lexer_no_newlines() {
        run_lexer_test_valid(
            "../writing-a-c-compiler-tests/tests/chapter_1/valid/no_newlines.c",
            basic_return(0),
        )
    }

    #[test]
    fn test_lexer_return_0() {
        run_lexer_test_valid(
            "../writing-a-c-compiler-tests/tests/chapter_1/valid/return_0.c",
            basic_return(0),
        )
    }

    #[test]
    fn test_lexer_return_2() {
        run_lexer_test_valid(
            "../writing-a-c-compiler-tests/tests/chapter_1/valid/return_2.c",
            basic_return(2),
        )
    }

    #[test]
    fn test_lexer_spaces() {
        run_lexer_test_valid(
            "../writing-a-c-compiler-tests/tests/chapter_1/valid/spaces.c",
            basic_return(0),
        )
    }

    #[test]
    fn test_lexer_tabs() {
        run_lexer_test_valid(
            "../writing-a-c-compiler-tests/tests/chapter_1/valid/tabs.c",
            basic_return(0),
        )
    }

    #[test]
    fn test_lexer_at_sign() {
        run_lexer_test_invalid(
            "../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/at_sign.c",
        )
    }

    #[test]
    fn test_lexer_backslash() {
        run_lexer_test_invalid(
            "../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/backslash.c",
        )
    }

    #[test]
    fn test_lexer_backtick() {
        run_lexer_test_invalid(
            "../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/backtick.c",
        )
    }

    #[test]
    fn test_lexer_invalid_identifier() {
        run_lexer_test_invalid(
            "../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/invalid_identifier.c",
        )
    }

    #[test]
    fn test_lexer_invalid_identifier2() {
        run_lexer_test_invalid(
            "../writing-a-c-compiler-tests/tests/chapter_1/invalid_lex/invalid_identifier_2.c",
        )
    }
}
