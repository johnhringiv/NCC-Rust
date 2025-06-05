# Writing a C Compiler in Rust

This project is a simple C compiler written in Rust, following Sandler's "Writing a C Compiler".
Some design decisions are informed by the book but the implementation is my own.

So far chapter 2 is implemented, which includes a lexer, parser, and code generator for very basic C code.
All provided tests pass.

## Requirements

- Rust (latest stable)
- GCC (for assembling and linking output)

## Building

Clone the repository and build with Cargo:

```sh
cargo build --release
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
| `-S`                      | Emits assembly                        |
| `-o`, `--output <OUTPUT>` | Override output file location         |
| `-h`, `--help`            | Print help                            |
| `-V`, `--version`         | Print version                         |


Note: `--lex`, `--parse`, `--code-gen`, `--tacky` are mutually exclusive options.
