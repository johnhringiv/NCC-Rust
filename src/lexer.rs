//! # Lexer — Source Text to Tokens
//!
//! Converts raw C source text into a stream of [`SpannedToken`]s using regex-based
//! pattern matching with maximal munch disambiguation.
//!
//! ## Technical Approach
//!
//! Token patterns are pre-compiled into regexes at startup via [`LazyLock`] and anchored
//! with `^` so they match only at the current scan position. On each iteration the lexer
//! tries every pattern, collects all matches, and picks the longest (maximal munch).
//! This avoids keyword/identifier ambiguity (e.g. `int` vs `integer`) without reserved-word
//! tables.
//!
//! ## What This Pass Accomplishes
//!
//! - Joins backslash-newline line splices first (C translation phase 2), keeping each byte's
//!   original position for spans
//! - Strips comments (`//`, `/* */`) and `#` lines (only when `#` starts the line; a stray `#`
//!   elsewhere is an error)
//! - Produces a [`VecDeque<SpannedToken>`] carrying line/column [`Span`]s for error reporting
//! - Detects lexer errors (unexpected characters, unterminated comments) and exits with code 10
//! - Promotes integer literals with an `L`/`l` suffix to [`Token::ConstantLong`]
//! - Keeps integer lexemes as written (a `0x` prefix marks hex and a leading `0` marks octal; the
//!   parser reads them in that base, see [`literal_radix`]) and rejects the digits 8 and 9 in octal
//!   literals
//! - Recognizes floating-point literals (fraction and/or exponent) as [`Token::ConstantDouble`]
//!
//! ## Call Order
//!
//! ```text
//! tokenizer()          — public entry point, drives the scan loop
//!   ├─ splice_lines()  — joins backslash-newlines, maps bytes back to source positions
//!   └─ next_token()    — tries all TOKEN_DEFS, returns longest match
//! ```

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
    ConstantLong(String),
    ConstantUnsignedInt(String),
    ConstantUnsignedLong(String),
    ConstantDouble(String),
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
    StaticKeyword,           // static
    ExternKeyword,           // extern
    LongKeyword,             // long
    SignedKeyword,           // signed
    UnsignedKeyword,         // unsigned
    DoubleKeyword,           // double
}

const TOKEN_PATTERNS: &[(&str, Token)] = &[
    // Special handling tokens (handled differently in next_token)
    (r"^[a-zA-Z_]\w*\b", Token::Identifier(String::new())),
    // Integer literals: hex (`0x1F`) or decimal/octal digits, then an optional suffix.
    (r"^(?:0[xX][0-9a-fA-F]+|[0-9]+)\b", Token::ConstantInt(String::new())),
    (
        r"^(?:0[xX][0-9a-fA-F]+|[0-9]+)[lL]\b",
        Token::ConstantLong(String::new()),
    ),
    (
        r"^(?:0[xX][0-9a-fA-F]+|[0-9]+)[uU]\b",
        Token::ConstantUnsignedInt(String::new()),
    ),
    (
        r"^(?:0[xX][0-9a-fA-F]++|[0-9]++)([lL][uU]|[uU][lL])\b",
        Token::ConstantUnsignedLong(String::new()),
    ),
    (
        r"^((?:[0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)(?:[^\w.]|$)",
        Token::ConstantDouble(String::new()),
    ),
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
    (r"^static\b", Token::StaticKeyword),
    (r"^extern\b", Token::ExternKeyword),
    (r"^long\b", Token::LongKeyword),
    (r"^signed\b", Token::SignedKeyword),
    (r"^unsigned\b", Token::UnsignedKeyword),
    (r"^double\b", Token::DoubleKeyword),
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

#[derive(Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

/// The base of an integer lexeme as written (suffix already removed): 16 for a `0x`/`0X` prefix,
/// 8 for a leading `0` followed by more digits, otherwise 10. A lone `0` counts as decimal; both
/// readings give the same value.
pub(crate) fn literal_radix(lexeme: &str) -> u32 {
    if lexeme.starts_with("0x") || lexeme.starts_with("0X") {
        16
    } else if lexeme.len() > 1 && lexeme.starts_with('0') {
        8
    } else {
        10
    }
}

fn next_token(input: &str, span: Span) -> Result<TokenMatch, LexerError> {
    let mut matches = vec![];
    for TokenDef { regex, variant } in TOKEN_DEFS.iter() {
        if let Some(caps) = regex.captures(input) {
            let mat = caps.get(0).unwrap();
            let token = match variant {
                Token::Identifier(_) => Token::Identifier(mat.as_str().to_string()),
                Token::ConstantInt(_) => Token::ConstantInt(mat.as_str().to_string()),
                Token::ConstantUnsignedInt(_) => {
                    let s = mat.as_str();
                    Token::ConstantUnsignedInt(s[..s.len() - 1].to_string())
                }
                Token::ConstantLong(_) => {
                    let s = mat.as_str();
                    Token::ConstantLong(s[..s.len() - 1].to_string())
                }
                Token::ConstantUnsignedLong(_) => {
                    let s = mat.as_str();
                    Token::ConstantUnsignedLong(s[..s.len() - 2].to_string())
                }
                Token::ConstantDouble(_) => {
                    let value = caps.get(1).unwrap();
                    Token::ConstantDouble(value.as_str().to_string())
                }
                other => other.clone(),
            };
            let length = match variant {
                Token::ConstantDouble(_) => caps.get(1).unwrap().end(),
                _ => mat.end(),
            };
            matches.push(TokenMatch { token, length });
        }
    }
    if let Some(best_match) = matches.iter().max_by_key(|m| m.length) {
        // A leading `0` makes an integer literal octal, so 8 and 9 are invalid in it. Checked on
        // the winning match only: `08.5` and `09e1` are decimal floating literals, which is fine.
        if let Token::ConstantInt(digits)
        | Token::ConstantLong(digits)
        | Token::ConstantUnsignedInt(digits)
        | Token::ConstantUnsignedLong(digits) = &best_match.token
            && literal_radix(digits) == 8
            && digits.contains(['8', '9'])
        {
            return Err(LexerError::with_span(
                format!("invalid digit in octal constant: {}", digits.bold()),
                span,
            ));
        }
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

/// Joins lines ending in a backslash (C translation phase 2), before any tokenizing.
///
/// Returns the spliced text and, for every byte of it, the original 1-based (line, column) it
/// came from, so spans still point at the source the user wrote. A splice can fall anywhere,
/// even inside a token (`ret\` + newline + `urn` is `return`) or a `//` comment, which then
/// continues onto the next line.
fn splice_lines(source: &str) -> (String, Vec<Span>) {
    let mut text = String::with_capacity(source.len());
    let mut positions = Vec::with_capacity(source.len() + 1);
    let (mut line, mut column) = (1, 1);
    let mut rest = source;
    while let Some(c) = rest.chars().next() {
        let splice_len = if rest.starts_with("\\\n") {
            2
        } else if rest.starts_with("\\\r\n") {
            3
        } else {
            0
        };
        if splice_len > 0 {
            line += 1;
            column = 1;
            rest = &rest[splice_len..];
            continue;
        }
        text.push(c);
        // one entry per byte, so any byte offset into `text` can be looked up
        positions.extend(std::iter::repeat_n(Span { line, column }, c.len_utf8()));
        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += c.len_utf8();
        }
        rest = &rest[c.len_utf8()..];
    }
    positions.push(Span { line, column }); // end of input
    (text, positions)
}

pub(crate) fn tokenizer(source: &str) -> Result<VecDeque<SpannedToken>, LexerError> {
    let (text, positions) = splice_lines(source);
    let mut tokens = VecDeque::new();
    let mut input = text.as_str();
    // Original source position of the current scan point.
    let pos = |input: &str| positions[text.len() - input.len()];
    // `#` starts a (skipped) preprocessing directive only as the first non-blank character of a
    // line. Anywhere else it's a stray character, which `next_token` rejects.
    let mut at_line_start = true;
    while !input.is_empty() {
        // C whitespace also includes form feed and vertical tab
        input = input.trim_start_matches([' ', '\t', '\r', '\x0C', '\x0B']);

        if input.starts_with('\n') {
            input = &input[1..];
            at_line_start = true;
        } else if input.starts_with("//") || (at_line_start && input.starts_with('#')) {
            // Skip to the end of the line; the newline itself is handled next iteration.
            input = &input[input.find('\n').unwrap_or(input.len())..];
        } else if input.starts_with("/*") {
            // Search past the opening `/*` so `/*/` doesn't pair its own `*` with the `/`
            match input[2..].find("*/") {
                Some(i) => input = &input[i + 4..],
                None => return Err(LexerError::with_span("unterminated comment".to_string(), pos(input))),
            }
        } else if !input.is_empty() {
            let span = pos(input);
            let TokenMatch { token, length } = next_token(input, span)?;
            tokens.push_back(SpannedToken { token, span });
            input = &input[length..];
            at_line_start = false;
        }
    }
    Ok(tokens)
}
