# Writing a C Compiler in Rust

This project is a simple C compiler written in Rust, following Sandler's "Writing a C Compiler".
Some design decisions are informed by the book but the implementation is my own.

So far chapter 4 (inc. bitwise extra credit) is implemented, which includes a lexer, parser, and code generator for very basic C code.
All provided tests pass.

## Requirements

- Rust (latest stable)
 - GCC (for the default assembly-based backend)
 - [wild](https://github.com/davidlattimore/wild) (via `libwild` crate) for the `--iced` option

## Building

Clone the repository and build with Cargo:

```sh
cargo build --release
```

If Cargo fails with an error about `symbolic-common` not matching any
available versions, update the crates index and lock file:

```sh
cargo update
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
| `--iced`                  | Use iced-x86 backend with wild linker |
| `-S`                      | Emits assembly                        |
| `-o`, `--output <OUTPUT>` | Override output file location         |
| `-h`, `--help`            | Print help                            |
| `-V`, `--version`         | Print version                         |


Note: `--lex`, `--parse`, `--code-gen`, `--tacky`, `--run`, `--iced` are mutually exclusive options.

## Contributing
As a reminder to myself.
Use the Makefile to check code quality `make quality` and fix formatting `make fix`.

### Requirements for Makefile
```shell
sudo apt-get install pkg-config libssl-dev make
```


