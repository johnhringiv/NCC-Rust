# Writing a C Compiler in Rust

This project is a simple C compiler written in Rust, following Sandler's "Writing a C Compiler".
Some design decisions are informed by the book but the implementation is my own.

So far chapter 5 (inc. bitwise, compound, and increment extra credit) is implemented, which includes a lexer, parser, and code generator for very basic C code.
This compiler is a fully standalone executable; it does not rely on any external programs for assembling or linking (on linux).
All provided tests pass.

## Requirements

- Rust (latest stable)
- GCC (Optional) for linking instead of `libwild`

## Building

Clone the repository and build with Cargo:

```sh
cargo build --release
```

### Running Tests
```sh
 ./writing-a-c-compiler-tests/test_compiler target/release/ncc --chapter 5 --extra-credit
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

## Language Grammar

The compiler currently implements a subset of C with the following grammar:

```ebnf
<program> ::= <function>
<function> ::= "int" <identifier> "(" "void" ")" "{" { <block-item> } "}"
<block-item> ::= <statement> | <declaration>
<declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
<statement> ::= "return" <exp> ";" | <exp> ";" | ";"
<exp> ::= <factor> | <exp> <binop> <exp> | <exp> <assign-op> <exp> | <exp> "++" | <exp> "--"
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
- **Integer arithmetic**: addition, subtraction, multiplication, division, modulo
- **Bitwise operations**: AND (`&`), OR (`|`), XOR (`^`), complement (`~`), left/right shift (`<<`, `>>`)
- **Logical operations**: AND (`&&`), OR (`||`), NOT (`!`) with short-circuit evaluation
- **Comparison operators**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Assignment operators**: simple (`=`) and compound (`+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`)
- **Increment/decrement**: prefix (`++x`, `--x`) and postfix (`x++`, `x--`)
- **Expression statements** and **return statements**

## Contributing
As a reminder to myself.
Use the Makefile to check code quality `make quality` and fix formatting `make fix`.

### Requirements for Makefile
```shell
sudo apt-get install pkg-config libssl-dev make
```


