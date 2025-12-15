mod codegen;
mod emit;
mod emit_iced;
mod lexer;
mod parser;
mod pretty;
mod tacky;
mod validate;

use crate::pretty::ItfDisplay;
use clap::{ArgGroup, Parser};
use colored::{ColoredString, Colorize};
use iced_x86::{Formatter, FormatterOutput, FormatterTextKind};
use libwild::{Args as WildArgs, Linker};
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(group(
    ArgGroup::new("mode")
        .required(false)
        .args(&["lex", "parse", "validate", "codegen", "s", "tacky", "run", "c"])
))]
struct Args {
    /// Run lexer
    #[arg(long)]
    lex: bool,

    /// Run lexer and parser
    #[arg(long)]
    parse: bool,

    /// Stop after semantic analysis
    #[arg(long)]
    validate: bool,

    /// Run lexer, parser, tacky, and code generator
    #[arg(long, name = "codegen")]
    codegen: bool,

    /// Emit Tacky IR
    #[arg(long, name = "tacky")]
    tacky: bool,

    /// emits assembly
    #[arg(short = 'S')]
    s: bool,

    /// Run the compiled program
    #[arg(long, name = "run")]
    run: bool,

    /// Emit object file only (no linking)
    #[arg(short = 'c')]
    c: bool,

    /// Use GCC for linking
    #[arg(long)]
    gcc: bool,

    /// Link statically (no runtime dependencies)
    #[arg(long = "static")]
    static_link: bool,

    /// Use text based asm building instead of iced (Deprecated)
    #[arg(long)]
    no_iced: bool,

    #[arg(short = 'o', long)]
    output: Option<String>,

    /// Input file (required)
    filename: String,
}

// For colored output, we need to implement a custom FormatterOutput
struct MyFormatterOutput {
    vec: Vec<(String, FormatterTextKind)>,
}
impl MyFormatterOutput {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }
}

impl FormatterOutput for MyFormatterOutput {
    fn write(&mut self, text: &str, kind: FormatterTextKind) {
        // This allocates a string. If that's a problem, just call print!() here
        // instead of storing the result in a vector.
        self.vec.push((String::from(text), kind));
    }
}

fn get_color(s: &str, kind: FormatterTextKind) -> ColoredString {
    match kind {
        FormatterTextKind::Directive | FormatterTextKind::Keyword => s.yellow(),
        FormatterTextKind::Prefix | FormatterTextKind::Mnemonic => s.magenta(),
        FormatterTextKind::Register => s.blue(),
        FormatterTextKind::Number => s.cyan(),
        FormatterTextKind::Label => s.green(),
        _ => s.white(),
    }
}

fn main() {
    let mut args = Args::parse();
    let Ok(input) = fs::read_to_string(&args.filename) else {
        eprintln!("Failed to read file: {}", args.filename);
        std::process::exit(1);
    };

    let tokens = lexer::tokenizer(&input).unwrap_or_else(|e| {
        eprintln!("{e:?}");
        std::process::exit(10)
    });

    if cfg!(target_os = "macos") & !args.gcc {
        println!("Using GCC as libwild does not support MacOS");
        args.gcc = true;
    }

    if args.lex {
        // stop here if only lexing
        println!(
            "Processed tokens: {:?}",
            tokens.iter().map(|t| t.token.clone()).collect::<Vec<_>>()
        );
        std::process::exit(0);
    }

    let mut tokens = tokens;
    let ast = parser::parse_program(&mut tokens).unwrap_or_else(|e| {
        eprintln!("{e:?}");
        std::process::exit(20)
    });

    if args.parse {
        let ast_val = ast;
        // println!("{ast_val:?}");
        println!("{}", ast_val.itf_string());
        std::process::exit(0);
    }

    let mut ast = ast;
    let validated_result = validate::resolve_program(&mut ast).unwrap_or_else(|e| {
        eprintln!("{e:?}");
        std::process::exit(30);
    });

    let mut name_gen = validated_result;
    if args.validate {
        println!("Program Valid");
        std::process::exit(0);
    }

    let tacky_ast = tacky::tackify_program(&ast, &mut name_gen);

    if args.tacky {
        println!("{tacky_ast:?}");
        println!("{}", tacky_ast.itf_string());
        std::process::exit(0);
    }

    let code_ast = codegen::generate(&tacky_ast);
    if args.codegen {
        println!("{}", code_ast.itf_string());
        std::process::exit(0);
    }

    let path = Path::new(&args.filename);
    let out_file = args
        .output
        .unwrap_or_else(|| path.with_extension("").to_string_lossy().to_string());

    if args.no_iced {
        let asm = emit::emit_program(&code_ast);
        if args.s {
            print!("Generated asm:\n\n{asm}");
        }
        let asm_file = format!("{out_file}.s");

        fs::write(&asm_file, asm).expect("Failed to write assembly file");

        let status = std::process::Command::new("gcc")
            .arg(&asm_file)
            .arg("-o")
            .arg(out_file.clone())
            .status()
            .expect("Failed to execute gcc");

        if !status.success() {
            eprintln!("Compilation failed with status: {status}");
            std::process::exit(1);
        }

        fs::remove_file(&asm_file).expect("Failed to delete assembly file");
    } else {
        let obj = emit_iced::emit_object(&code_ast).expect("iced obj");

        if args.s {
            let (asm, resolver, label_idx) = emit_iced::get_instructions(&code_ast).unwrap();

            let mut formatter = iced_x86::GasFormatter::with_options(Some(Box::new(resolver)), None);
            let ops = formatter.options_mut();
            ops.set_number_base(iced_x86::NumberBase::Decimal);
            let mut output = MyFormatterOutput::new();

            println!("Generated ASM\n");

            let mut current_offset = 0;
            let mut current_function = 0;

            for ins in asm.iter() {
                // Check if we've reached a new function based on byte offset
                if let Some(label_name) = label_idx.get(&current_offset) {
                    if current_function > 0 {
                        println!(); // blank line between functions
                    }
                    println!("{}:", label_name.green());
                    current_function += 1;
                }

                print!("  ");
                output.vec.clear();
                formatter.format(ins, &mut output);
                for (text, kind) in output.vec.iter() {
                    print!("{}", get_color(text.as_str(), *kind));
                }
                println!();
                current_offset += ins.len();
            }

            std::process::exit(0);
        }

        let obj_file = format!("{out_file}.o");
        fs::write(&obj_file, &obj).expect("Failed to write object file");

        if args.c {
            std::process::exit(0);
        }

        if args.gcc {
            let status = std::process::Command::new("gcc")
                .arg("-nostdlib")
                .arg("-static")
                .arg(&obj_file)
                .arg("-o")
                .arg(&out_file)
                .arg("-Wl,-e,_start")
                .status()
                .expect("Failed to execute gcc");

            if !status.success() {
                println!("Compilation failed with status: {status}");
                std::process::exit(1);
            }
        } else {
            let linker = Linker::new();

            // Library search paths (architecture-specific)
            let lib_dirs = [
                "/lib/x86_64-linux-gnu",      // Ubuntu/Debian
                "/usr/lib/x86_64-linux-gnu",  // Alternative Ubuntu/Debian
                "/lib64",                      // RHEL/Fedora
                "/usr/lib64",                  // RHEL/Fedora alt
                "/usr/lib",                    // Generic
            ];

            // Find the library directory that has CRT files
            let lib_dir = lib_dirs.iter()
                .find(|d| std::path::Path::new(&format!("{}/crt1.o", d)).exists());

            let mut args_vec: Vec<String> = vec!["-o".to_string(), out_file.clone()];

            // Static linking: no dynamic linker needed, links against libc.a
            if args.static_link {
                args_vec.push("-static".to_string());
            } else {
                // Find dynamic linker for dynamic linking
                let ld_paths = [
                    "/lib64/ld-linux-x86-64.so.2",
                    "/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2",
                ];
                if let Some(ld) = ld_paths.iter().find(|p| std::path::Path::new(p).exists()) {
                    args_vec.push(format!("--dynamic-linker={}", ld));
                }
            }

            // Add library search paths
            for dir in &lib_dirs {
                if std::path::Path::new(dir).exists() {
                    args_vec.push(format!("-L{}", dir));
                }
            }

            // For static linking, we need libgcc for compiler support routines
            if args.static_link {
                // Find gcc library directory
                let gcc_dirs = [
                    "/usr/lib/gcc/x86_64-linux-gnu/13",
                    "/usr/lib/gcc/x86_64-linux-gnu/12",
                    "/usr/lib/gcc/x86_64-linux-gnu/11",
                    "/usr/lib/gcc/x86_64-linux-gnu/10",
                    "/usr/lib/gcc/x86_64-linux-gnu/9",
                    "/usr/lib64/gcc/x86_64-linux-gnu/13",
                ];
                if let Some(gcc_dir) = gcc_dirs.iter().find(|d| std::path::Path::new(d).exists()) {
                    args_vec.push(format!("-L{}", gcc_dir));
                }
            }

            // Add CRT startup files and object file in correct order:
            // crt1.o crti.o <user objects> -lc [-lgcc -lgcc_eh] crtn.o
            if let Some(dir) = lib_dir {
                args_vec.push(format!("{}/crt1.o", dir));
                args_vec.push(format!("{}/crti.o", dir));
            }

            // User object file
            args_vec.push(obj_file.clone());

            // Link with libc
            args_vec.push("-lc".to_string());

            // Static linking needs libgcc for compiler support
            if args.static_link {
                args_vec.push("-lgcc".to_string());
                args_vec.push("-lgcc_eh".to_string());
            }

            // CRT epilogue
            if let Some(dir) = lib_dir {
                args_vec.push(format!("{}/crtn.o", dir));
            }

            let args_refs: Vec<&str> = args_vec.iter().map(|s| s.as_str()).collect();
            let parsed = WildArgs::parse(args_refs.iter()).expect("parse args");
            let _ = libwild::setup_tracing(&parsed);
            linker.run(&parsed).expect("link failed");
        }
        fs::remove_file(&obj_file).ok();
    }

    if args.run {
        let run_status = std::process::Command::new(&out_file)
            .status()
            .expect("Failed to execute compiled binary");
        match run_status.code() {
            Some(code) => println!("Result: {}", code),
            None => {
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    if let Some(signal) = run_status.signal() {
                        eprintln!("Process terminated by signal: {}", signal);
                    } else {
                        eprintln!("Process terminated without exit code");
                    }
                }
                #[cfg(not(unix))]
                eprintln!("Process terminated without exit code");
            }
        }
        fs::remove_file(&out_file).ok();
    }
}

// Available for both unit tests and integration tests
#[cfg(test)]
pub mod test_utils {
    use crate::lexer::tokenizer;
    use crate::parser::parse_program;
    use crate::validate::resolve_program;
    use std::fs;
    use std::path::Path;

    static CHAPTER_COMPLETED: i32 = 8;
    static EXTRA_COMPLETED: i32 = 8;
    pub enum Stage {
        Lex,
        Parse,
        Validate,
    }

    impl Stage {
        pub fn dir(&self) -> &'static str {
            match self {
                Stage::Lex => "lex",
                Stage::Parse => "parse",
                Stage::Validate => "semantics",
            }
        }
    }

    pub fn get_sandler_dirs(valid: bool, stage: &Stage) -> Vec<String> {
        let mut dirs = vec![];
        for chapter in 1..=CHAPTER_COMPLETED {
            if valid {
                dirs.push(format!("../writing-a-c-compiler-tests/tests/chapter_{chapter}/valid/"));
            } else {
                dirs.push(format!(
                    "../writing-a-c-compiler-tests/tests/chapter_{chapter}/invalid_{:}/",
                    stage.dir()
                ));
            }
        }
        for chapter in 1..=EXTRA_COMPLETED {
            if valid {
                dirs.push(format!(
                    "../writing-a-c-compiler-tests/tests/chapter_{chapter}/valid/extra_credit/"
                ));
            } else {
                dirs.push(format!(
                    "../writing-a-c-compiler-tests/tests/chapter_{chapter}/invalid_{:}/extra_credit/",
                    stage.dir()
                ));
            }
        }
        dirs
    }

    pub fn run_tests(dir_paths: &Vec<String>, should_pass: bool, stage: &Stage) -> (usize, Vec<String>) {
        let mut passed = 0;
        let mut failed = vec![];
        for dir_path in dir_paths {
            let test_dir = Path::new(dir_path);

            if test_dir.exists() {
                for entry in fs::read_dir(test_dir).unwrap() {
                    let entry = entry.unwrap();
                    let path = entry.path();
                    if path.is_file() && path.extension() == Some("c".as_ref()) {
                        let input = fs::read_to_string(&path).unwrap();
                        let test_result = match stage {
                            Stage::Lex => tokenizer(&input).is_err(),
                            Stage::Parse => {
                                let mut tokens = tokenizer(&input).unwrap();
                                parse_program(&mut tokens).is_err()
                            }
                            Stage::Validate => {
                                let mut tokens = tokenizer(&input).unwrap();
                                let mut program = parse_program(&mut tokens).unwrap();
                                resolve_program(&mut program).is_err()
                            }
                        };

                        let test_passed = if should_pass { !test_result } else { test_result };

                        if test_passed {
                            passed += 1;
                        } else {
                            failed.push(path.file_name().unwrap().to_str().unwrap().to_string());
                        }
                    }
                }
            }
        }

        (passed, failed)
    }
}
