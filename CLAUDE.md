# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Working Principles

**This is a hobby/learning project.** The user wants to implement features themselves—do not implement core compiler logic or new features unless explicitly requested.

**What to do:**
- Answer questions about C and Rust related to features the user is working on
- Proactively suggest more performant or idiomatic Rust code when reviewing changes
- Suggest related features or improvements (e.g., new warnings, edge cases, test coverage) as the user works—but don't implement them
- Implement test cases (in `tests/` only, ensure they add value and don't overlap with existing tests)
- Make simple updates, write documentation, and implement pretty printing
- Confirm existing documentation (README.md, CLAUDE.md, comments) stays in sync with code changes
- When the user asks about a specific part of the codebase, focus on that part and ignore surrounding compilation errors (feature additions often break things temporarily)

**What NOT to do:**
- Do not implement core compiler logic (lexer, parser, validator, tackifier, codegen, emitter logic)
- Do not add new language features or compiler passes
- Push back if a task involves writing substantial feature logic—the user wants to learn by doing it themselves

**Test Case Guidelines:**
- User's custom tests: `tests/c_programs/` (with `tests/c_programs/expected_results.json`)
- Sandler's book tests: `writing-a-c-compiler-tests/tests/`
- **Always add new tests to `tests/c_programs/`**, never to the book's test directory
- New tests should add value and not overlap with existing tests in either location
- Follow the directory structure: `invalid_lex/`, `invalid_parse/`, `invalid_semantics/` for error tests

## Build & Development Commands

### Building
```bash
cargo build --release          # Production build
cargo build                    # Debug build
```

### Testing
```bash
cargo test                     # Run all tests
cargo test test_custom_programs # Run only custom test suite
cargo test test_all_sandler    # Run Sandler test suite
cargo test -- --nocapture      # Show test output
```

The test runner (`tests/runner.rs`) compiles and executes C programs, comparing exit codes and stdout. Tests are auto-discovered from:
- `writing-a-c-compiler-tests/tests/chapter_*/` (Sandler's test suite)
- `tests/c_programs/` (custom tests)

Test classification by directory:
- `invalid_lex/` → expects exit code 10 (lexer error)
- `invalid_parse/` → expects exit code 20 (parser error)
- `invalid_semantics/` → expects exit code 30 (validation error)
- All others → expects successful compilation + runtime return code from `expected_results.json`

Library tests (`*_client.c` files) run cross-compilation tests to validate ABI compliance by linking ncc-compiled code with gcc-compiled code in both directions.

### Code Quality
```bash
make quality                   # Run all checks (fmt, sort, clippy, test, machete)
make fix                       # Auto-fix formatting and dependency order
make clippy                    # Run clippy with warnings as errors
```

### Running NCC
```bash
# Compile and run
cargo run -- examples/program.c --run

# Debug compiler passes
cargo run -- file.c --lex      # Tokenize only
cargo run -- file.c --parse    # Parse to AST
cargo run -- file.c --validate # Semantic analysis
cargo run -- file.c --tacky    # Emit TACKY IR
cargo run -- file.c --codegen  # Generate assembly AST
cargo run -- file.c -S         # Emit x86-64 assembly

# Compilation options
cargo run -- file.c -c         # Emit object file only
cargo run -- file.c -o binary  # Custom output name
cargo run -- file.c --static   # Static linking (Linux only)
cargo run -- file.c --external-linker # Use system ld instead of libwild
cargo run -- file.c --no-iced  # Use deprecated text-based assembler
```

## Compiler Architecture

NCC is a multi-pass C compiler that generates x86-64 machine code. The pipeline:

```
Source .c → Lexer → Parser → Validator → Tackifier → Codegen → Emitter → Linker → Executable
           tokens    AST    typed AST   TACKY IR   asm AST   object    binary
```

### Core Passes (src/)

**Lexer** (`lexer.rs`): Tokenizes source using regex patterns with maximal munch. Each token carries a `Span` (file, line, column) for error reporting. Exit code 10 on error.

**Parser** (`parser.rs`): Recursive descent parser with precedence climbing. Builds an abstract AST representing C syntax. No semantic analysis yet—identifiers are just strings, types are not checked. Exit code 20 on error.

**Validator** (`validate.rs`): Two-pass semantic analysis:
1. **Resolution pass**: Renames variables to unique identifiers, labels loops/switches for break/continue, validates goto targets
2. **Type checking pass**: Builds symbol table, checks types, evaluates constant expressions (for static initializers and case labels), emits warnings (-Wshadow, -Wswitch-unreachable, -Wunused-parameter)

Returns `(NameGenerator, SymbolTable)` needed by subsequent passes. Exit code 30 on error.

**Tackifier** (`tacky.rs`): Converts typed AST to TACKY (three-address code IR). Flattens nested expressions into sequences of simple operations. Makes control flow explicit with jumps/labels. Pseudo-registers used instead of x86 registers.

**Codegen** (`codegen.rs`): Lowers TACKY to x86-64 assembly AST. Assigns pseudo-registers to stack slots, fixes invalid instruction operands (x86 restrictions), implements System V AMD64 calling convention (arguments in RDI, RSI, RDX, RCX, R8, R9, then stack).

**Emitter** (`emit_iced.rs`): Primary emitter using [iced-x86](https://github.com/icedland/iced) to encode instructions to machine code and [object](https://github.com/gimli-rs/object) crate to write ELF (Linux) or Mach-O (macOS) object files. No external assembler needed.

Alternative: `emit.rs` (deprecated `--no-iced` flag) generates text assembly for `as`.

**Linker** (`main.rs`): On Linux, uses [wild](https://github.com/wild-linker/wild) for in-process linking. On macOS, shells out to system `ld`. Locates CRT files and libc via the `cc` compiler.

### Key Data Structures

**AST Evolution**:
- Parser AST: Untyped, identifiers are strings
- Validator AST: Adds type information (`TypedExpression`), renames identifiers uniquely
- TACKY IR: Flattened three-address code with explicit control flow
- Assembly AST: x86-64 instructions with pseudo-registers
- Machine code: Final encoded bytes

**Symbol Table**: Maps identifiers to their type, storage class, and initial value. Built during validation, used by tackifier.

**NameGenerator**: Produces unique names for temporaries, labels, and renamed variables. Threaded through validation and tackification.

### Pretty Printing

The `pretty.rs` module provides tree-view debug output via the `ItfDisplay` trait. Use `--parse`, `--tacky`, or `--codegen` flags to visualize intermediate representations.

## Platform Support

- **Linux**: Full support with libwild linker and static linking
- **macOS**: Intel and Apple Silicon (arm64), requires system `ld` linker. NCC emits x86-64, so on
  Apple Silicon the external assembler/linker are invoked with `-arch x86_64` and binaries run under Rosetta 2
- **Windows**: Not supported (WSL works)

## Testing Strategy

Tests validate both successful compilation and error handling:
- Valid programs check exit codes and stdout
- Invalid programs verify correct error detection with proper exit codes
- Library tests validate ABI compliance via cross-compilation
- Assembly output tests (`test_label_printing_with_iced`, `test_data_symbol_resolution_with_iced`) verify correct label generation and symbol resolution
- Warning tests ensure diagnostics are emitted correctly

## Language Implementation Notes

**Type System**: Currently supports `int` (32-bit) and `long` (64-bit). Type conversions follow two's complement truncation.

**Integer Arithmetic**: Deterministic wrapping behavior (non-standard C extension). Shift amounts are masked to prevent undefined behavior.

**Evaluation Order**: Left-to-right (non-standard, eliminates UB).

**Constant Expressions**: Evaluated at compile time using the same rules as runtime expressions. Used for static initializers and case labels.

**Storage Classes**:
- `static` → internal linkage (file-local)
- `extern` → external linkage (visible to other files)
- No specifier → functions default to extern, variables default to auto (local) or tentative (global)

**ABI Compliance**: Implements System V AMD64 calling convention. Validated by linking ncc-compiled code with gcc-compiled code.
