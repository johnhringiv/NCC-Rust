# NCC - Not Completely C
[![Lint](https://github.com/johnhringiv/NCC-Rust/actions/workflows/lint.yml/badge.svg)](https://github.com/johnhringiv/NCC-Rust/actions/workflows/lint.yml)
[![codecov](https://codecov.io/gh/johnhringiv/NCC-Rust/graph/badge.svg?token=GJJCD2Z8Y6)](https://codecov.io/gh/johnhringiv/NCC-Rust)

A simple C compiler written in Rust, following Sandler's "Writing a C Compiler".
Some design decisions are informed by the book but the implementation is my own.

So far chapter 8, including extra credit is implemented, which includes a lexer, parser, semantic analysis, and code generator for C code with local variables, compound statements, loops, and switch statements.
This compiler is a fully standalone executable; it does not rely on any external programs for assembling or linking (on Linux).
All provided tests pass.

## Language Grammar

The compiler currently implements a subset of C with the following grammar:

```ebnf
<program> ::= <function>
<function> ::= "int" <identifier> "(" "void" ")" <block>
<block> ::= "{" { <block-item> } "}"
<block-item> ::= <statement> | <declaration>
<declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
<for-init> ::= <declaration> | [ <exp> ] ";"
<statement> ::= "return" <exp> ";"
            | <exp> ";"
            | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
            | "goto" <identifier> ";"
            | <identifier> ":" <statement>
            | <block>
            | "break" ";"
            | "continue" ";"
            | "while" "(" <exp> ")" <statement>
            | "do" <statement> "while" "(" <exp> ")" ";"
            | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
            | "switch" "(" <exp> ")" <statement>
            | "case" <exp> ":" <statement>
            | "default" ":" <statement>
            | ";"
<exp> ::= <factor> | <exp> <binop> <exp> | <exp> <assign-op> <exp> 
       | <exp> "?" <exp> ":" <exp> | <exp> "++" | <exp> "--"
<factor> ::= <int> | <identifier> | <unop> <factor> | "++" <factor> | "--" <factor> | "(" <exp> ")"
<unop> ::= "-" | "~" | "!"
<binop> ::= "-" | "+" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "&&" | "||"
         | "==" | "!=" | "<" | "<=" | ">" | ">="
<assign-op> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
<identifier> ::= ? An identifier token ?
<int> ::= ? A constant token ?
```

### Supported Features

The compiler supports:
- **Single function programs** with `int main(void)` signature
- **Local variable declarations** with optional initialization
- **Compound statements (blocks)**: `{ ... }` with proper scoping
- **Variable scoping**: Block-local variables with shadowing support
- **Integer arithmetic**: addition, subtraction, multiplication, division, modulo
- **Bitwise operations**: AND (`&`), OR (`|`), XOR (`^`), complement (`~`), left/right shift (`<<`, `>>`)
- **Logical operations**: AND (`&&`), OR (`||`), NOT (`!`) with short-circuit evaluation
- **Comparison operators**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Assignment operators**: simple (`=`) and compound (`+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`)
- **Increment/decrement**: prefix (`++x`, `--x`) and postfix (`x++`, `x--`)
- **Conditional (ternary) operator**: `condition ? true_expr : false_expr`
- **Control flow**:
  - `if`/`else` statements
  - `switch` statements with `case` and `default` labels
  - `while` loops
  - `do-while` loops
  - `for` loops with all three components (init, condition, update)
  - `break` and `continue` statements
  - Compound statements/blocks
  - `goto` and labeled statements
  - `return` statements
- **Expression statements** and **null statements**

### Safer C

NCC provides several safety features and guarantees to help developers write more reliable code:

#### Guaranteed Behaviors
- **Deterministic integer overflow**: Integer arithmetic uses two's complement wrapping (32-bit). For example, `INT_MAX + 1` reliably wraps to `INT_MIN`.
- **Left-to-right evaluation**: Binary operations are evaluated left to right, eliminating undefined behavior from evaluation order.

#### Compile-Time Warnings
- **Variable shadowing** (`Wshadow`): Warns when a variable declaration shadows a previous declaration in an outer scope
- **Duplicate switch cases** (` Wswitch-unreachable`): Detects and reports duplicate case values in switch statements, including those from constant expressions

#### Developer Experience
- **Precise error locations**: All errors and warnings include exact line and column numbers with file:line:column format, making it easy to locate problematic code
- **Contextual error messages**: Semantic errors reference related code locations (e.g., showing both the shadowing variable and the original declaration)
- **Pretty-printed ASTs**: Visual tree representations of parsed code (`--parse`), intermediate representations (`--tacky`), and generated code (`--codegen`) for debugging and understanding compilation stages

These features help catch common bugs at compile time while providing predictable runtime behavior.

## Requirements

- Rust (latest stable)
- GCC (Optional) for linking instead of `libwild`

## Setup

```bash
# Clone the repository
gh repo clone johnhringiv/NCC-Rust

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Initialize submodules
git submodule update --init

# Install build essentials (Linux)
sudo apt install build-essential
```

## Building

Clone the repository and build with Cargo:

```sh
cargo build --release
```

### Running Tests
```sh
cargo test
```

## Usage

Usage: ncc [OPTIONS] `FILENAME`

### Arguments
`FILENAME` Input file (required)

### Options
| Option                    | Description                           |
|---------------------------|---------------------------------------|
| `--lex`                   | Run lexer                             |
| `--parse`                 | Run lexer and parser                  |
| `--validate`              | Run lexer, parser, and validator      |
| `--codegen`               | Run lexer, parser, and code generator |
| `--tacky`                 | Emit TACKY IR                         |
| `--run`                   | Run Compiled Program and print result |
| `--gcc`                   | Use GCC for linking (instead of wild) |
| `-S`                      | Emits assembly                        |
| `-o`, `--output <OUTPUT>` | Override output file location         |
| `-h`, `--help`            | Print help                            |
| `-V`, `--version`         | Print version                         |


Note: `--lex`, `--parse`, `--validate`, `--codegen`, `--tacky`, `--run` are mutually exclusive options.

### Exit Codes

NCC uses specific exit codes to indicate different types of failures:

| Exit Code | Description                                   |
|-----------|-----------------------------------------------|
| 0         | Success                                       |
| 1         | General error (file I/O, compilation failure) |
| 10        | Lexer error (tokenization failed)             |
| 20        | Parser error (syntax error)                   |
| 30        | Validation error (semantic error)             |

## Contributing
As a reminder to myself.
Use the Makefile to check code quality `make quality` and fix formatting `make fix`.

### Requirements for Makefile
```shell
sudo apt-get install pkg-config libssl-dev make
```


