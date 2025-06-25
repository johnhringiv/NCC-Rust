# Writing a C Compiler in Rust

This project is a simple C compiler written in Rust, following Sandler's "Writing a C Compiler".
Some design decisions are informed by the book but the implementation is my own.

So far chapter 4 (inc. bitwise extra credit) is implemented, which includes a lexer, parser, and code generator for very basic C code.
All provided tests pass.

## Requirements

- Rust (latest stable)
- GCC (for assembling and linking output)

## Building

Clone the repository and build with Cargo:

```sh
cargo build --release
```

### Running Tests
```sh
 ./writing-a-c-compiler-tests/test_compiler target/release/ncc --chapter 4 --bitwise
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
| `--codegen`               | Run lexer, parser, and code generator |
| `--tacky`                 | Emit TACKY IR                         |
| `--run`                   | Run Compiled Program and print result |
| `-S`                      | Emits assembly                        |
| `-o`, `--output <OUTPUT>` | Override output file location         |
| `-h`, `--help`            | Print help                            |
| `-V`, `--version`         | Print version                         |


Note: `--lex`, `--parse`, `--code-gen`, `--tacky`, `--run` are mutually exclusive options.
