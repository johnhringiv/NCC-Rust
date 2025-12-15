// todo: we should stream tokens to parser in a future refactor
use colored::*;
use regex::Regex;
use std::collections::VecDeque;
use std::fmt;
use std::sync::LazyLock;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    ConstantInt(String),
    IntKeyword,              // int
    VoidKeyword,             // void
    ReturnKeyword,           // return
    OpenParen,               // (
    CloseParen,              // )
    OpenBrace,               // {
    CloseBrace,              // }
    Semicolon,               // ;
    BitwiseComplement,       // ~
    Negation,                // -
    Plus,                    // +
    Asterisk,                // *
    Division,                // /
    Modulus,                 // %
    BitwiseAnd,              // &
    BitwiseOr,               // |
    BitwiseXOr,              // ^
    BitwiseLeftShift,        // <<
    BitwiseRightShift,       // >>
    LogicalNot,              // !
    LogicalAnd,              // &&
    LogicalOr,               // ||
    Equal,                   // ==
    NotEqual,                // !=
    LessThan,                // <
    GreaterThan,             // >
    LessThanOrEqual,         // <=
    GreaterThanOrEqual,      // >=
    Assignment,              // =
    PlusAssign,              // +=
    MinusAssign,             // -=
    AsteriskAssign,          // *=
    DivisionAssign,          // /=
    ModulusAssign,           // %=
    BitwiseAndAssign,        // &=
    BitwiseOrAssign,         // |=
    BitwiseXOrAssign,        // ^=
    BitwiseLeftShiftAssign,  // <<=
    BitwiseRightShiftAssign, // >>=
    Decrement,               // --
    Increment,               // ++
    IfKeyword,               // if
    ElseKeyword,             // else
    GotoKeyword,             // goto
    QuestionMark,            // ?
    Colon,                   // :
    DoKeyword,               // do
    WhileKeyword,            // while
    ForKeyword,              // for
    BreakKeyword,            // break
    ContinueKeyword,         // continue
    SwitchKeyword,           // switch
    DefaultKeyword,          // default
    CaseKeyword,             // case
    Comma,                   // ,
}

const TOKEN_PATTERNS: &[(&str, Token)] = &[
    // Special handling tokens (handled differently in next_token)
    (r"^[a-zA-Z_]\w*\b", Token::Identifier(String::new())),
    (r"^[0-9]+\b", Token::ConstantInt(String::new())),
    // Keywords
    (r"^int\b", Token::IntKeyword),
    (r"^void\b", Token::VoidKeyword),
    (r"^return\b", Token::ReturnKeyword),
    // Delimiters
    (r"^\(", Token::OpenParen),
    (r"^\)", Token::CloseParen),
    (r"^\{", Token::OpenBrace),
    (r"^}", Token::CloseBrace),
    (r"^;", Token::Semicolon),
    (r"^\,", Token::Comma),
    // Single-character operators
    (r"^~", Token::BitwiseComplement),
    (r"^-", Token::Negation),
    (r"^\+", Token::Plus),
    (r"^\*", Token::Asterisk),
    (r"^/", Token::Division),
    (r"^%", Token::Modulus),
    (r"^&", Token::BitwiseAnd),
    (r"^\|", Token::BitwiseOr),
    (r"^\^", Token::BitwiseXOr),
    // Shift operators
    (r"^<<", Token::BitwiseLeftShift),
    (r"^>>", Token::BitwiseRightShift),
    // Logical operators
    (r"^!", Token::LogicalNot),
    (r"^&&", Token::LogicalAnd),
    (r"^\|\|", Token::LogicalOr),
    // Comparison operators
    (r"^==", Token::Equal),
    (r"^!=", Token::NotEqual),
    (r"^<", Token::LessThan),
    (r"^>", Token::GreaterThan),
    (r"^<=", Token::LessThanOrEqual),
    (r"^>=", Token::GreaterThanOrEqual),
    // Assignment operators
    (r"^=", Token::Assignment),
    (r"^\+=", Token::PlusAssign),
    (r"^-=", Token::MinusAssign),
    (r"^\*=", Token::AsteriskAssign),
    (r"^/=", Token::DivisionAssign),
    (r"^%=", Token::ModulusAssign),
    (r"^&=", Token::BitwiseAndAssign),
    (r"^\|=", Token::BitwiseOrAssign),
    (r"^\^=", Token::BitwiseXOrAssign),
    (r"^<<=", Token::BitwiseLeftShiftAssign),
    (r"^>>=", Token::BitwiseRightShiftAssign),
    // P*fix operators
    (r"^--", Token::Decrement),
    (r"^\+\+", Token::Increment),
    (r"^if\b", Token::IfKeyword),
    (r"^else\b", Token::ElseKeyword),
    (r"^goto\b", Token::GotoKeyword),
    (r"^\?", Token::QuestionMark),
    (r"^:", Token::Colon),
    (r"^do\b", Token::DoKeyword),
    (r"^while\b", Token::WhileKeyword),
    (r"^for\b", Token::ForKeyword),
    (r"^break\b", Token::BreakKeyword),
    (r"^continue\b", Token::ContinueKeyword),
    (r"^switch\b", Token::SwitchKeyword),
    (r"^default\b", Token::DefaultKeyword),
    (r"^case\b", Token::CaseKeyword),
];

static TOKEN_DEFS: LazyLock<Vec<TokenDef>, fn() -> Vec<TokenDef>> = LazyLock::new(|| {
    let mut defs = Vec::with_capacity(TOKEN_PATTERNS.len());
    for (pattern, token) in TOKEN_PATTERNS {
        defs.push(TokenDef {
            regex: Regex::new(pattern).unwrap(),
            variant: token.clone(),
        });
    }
    defs
});

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(_) => write!(f, "Identifier"),
            Token::ConstantInt(_) => write!(f, "ConstantInt"),
            other => write!(f, "{other:?}"),
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

#[derive(Clone, Debug, PartialEq, Copy)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let filename = std::env::args().nth(1);
        if let Some(fname) = filename {
            write!(f, "{}:{}:{}", fname, self.line, self.column)
        } else {
            write!(f, "{}:{}", self.line, self.column)
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

fn next_token(input: &str, span: Span) -> Result<TokenMatch, LexerError> {
    let mut matches = vec![];
    for TokenDef { regex, variant } in TOKEN_DEFS.iter() {
        if let Some(mat) = regex.find(input) {
            let token = match variant {
                Token::Identifier(_) => Token::Identifier(mat.as_str().to_string()),
                Token::ConstantInt(_) => Token::ConstantInt(mat.as_str().to_string()),
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
        let preview = if input.len() > 20 {
            format!("{}...", &input[..20])
        } else {
            input.to_string()
        };
        Err(LexerError::with_span(
            format!("unexpected character sequence: {}", preview.bold()),
            span,
        ))
    }
}

pub struct LexerError {
    message: String,
    span: Option<Span>,
}

impl LexerError {
    fn with_span(message: String, span: Span) -> Self {
        LexerError {
            message,
            span: Some(span),
        }
    }
}

impl fmt::Debug for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.span {
            Some(span) => write!(f, "{}: {}: {}", span, "LexerError".red(), self.message),
            None => write!(f, "{}: {}", "LexerError".red(), self.message),
        }
    }
}

pub(crate) fn tokenizer(mut input: &str) -> Result<VecDeque<SpannedToken>, LexerError> {
    let mut tokens = VecDeque::new();
    let mut line = 1;
    let mut col = 1;
    while !input.is_empty() {
        let trimmed = input.trim_start_matches([' ', '\t', '\r']);
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
            } else {
                // End of file reached with // or # comment - skip rest of input
                break;
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
                return Err(LexerError::with_span(
                    "unterminated comment".to_string(),
                    Span { line, column: col },
                ));
            }
            continue;
        }

        if input.is_empty() {
            break;
        }

        match next_token(input, Span { line, column: col }) {
            Ok(TokenMatch { token, length }) => {
                tokens.push_back(SpannedToken {
                    token,
                    span: Span { line, column: col },
                });
                col += length; // no newline in tokens
                input = &input[length..];
            }
            Err(e) => return Err(e),
        }
    }
    Ok(tokens)
}
