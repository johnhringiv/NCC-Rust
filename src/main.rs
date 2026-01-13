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
use std::fs;
use std::path::Path;

#[cfg(target_os = "linux")]
use libwild::{Args as WildArgs, Linker};

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

    /// Use external system linker (ld) instead of built-in libwild. Required on macOS.
    #[arg(long)]
    external_linker: bool,

    /// Link statically (no runtime dependencies) - Linux only
    #[arg(long = "static")]
    static_link: bool,

    /// Use text based asm building instead of iced (Deprecated)
    #[arg(long)]
    no_iced: bool,

    #[arg(short = 'o', long)]
    output: Option<String>,

    /// Input files (required)
    #[arg(required = true)]
    filenames: Vec<String>,
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

/// Ask the C compiler for a file path (e.g., crt1.o, crti.o)
#[cfg(target_os = "linux")]
fn get_cc_file(name: &str) -> Option<String> {
    std::process::Command::new("cc")
        .arg(format!("-print-file-name={}", name))
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty() && s != name) // cc returns just the name if not found
}

/// Get library search paths from the C compiler
#[cfg(target_os = "linux")]
fn get_cc_library_paths() -> Vec<String> {
    std::process::Command::new("cc")
        .args(["-print-search-dirs"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .and_then(|s| {
            s.lines()
                .find(|line| line.starts_with("libraries:"))
                .map(|line| {
                    line.trim_start_matches("libraries:")
                        .trim_start_matches(" =")
                        .split(':')
                        .filter(|p| !p.is_empty() && std::path::Path::new(p).exists())
                        .map(String::from)
                        .collect()
                })
        })
        .unwrap_or_default()
}

/// Build linker arguments for Linux (shared between ld and libwild)
#[cfg(target_os = "linux")]
fn build_linux_linker_args(obj_files: &[String], out_file: &str, static_link: bool) -> Vec<String> {
    let mut args = vec!["-o".to_string(), out_file.to_string()];

    if static_link {
        args.push("-static".to_string());
    } else if let Some(ld) = get_cc_file("ld-linux-x86-64.so.2") {
        args.push(format!("--dynamic-linker={}", ld));
    }

    // Add library search paths
    for path in get_cc_library_paths() {
        args.push(format!("-L{}", path));
    }

    if let Some(crt1) = get_cc_file("crt1.o") {
        args.push(crt1);
    }
    if let Some(crti) = get_cc_file("crti.o") {
        args.push(crti);
    }

    args.extend(obj_files.iter().cloned());
    args.push("-lc".to_string());

    if static_link {
        args.push("-lgcc".to_string());
        args.push("-lgcc_eh".to_string());
    }

    if let Some(crtn) = get_cc_file("crtn.o") {
        args.push(crtn);
    }

    args
}

/// Link object files using the system linker (ld)
#[allow(unused_variables)]
fn link_with_ld(obj_files: &[String], out_file: &str, static_link: bool) {
    let mut cmd = std::process::Command::new("ld");

    #[cfg(target_os = "macos")]
    {
        cmd.args(["-arch", "x86_64"]);

        let macos_version = std::process::Command::new("sw_vers")
            .args(["-productVersion"])
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .map(|v| v.trim().to_string())
            .unwrap_or_else(|| "11.0".to_string());
        cmd.args(["-platform_version", "macos", &macos_version, &macos_version]);

        if let Ok(output) = std::process::Command::new("xcrun").args(["--show-sdk-path"]).output() {
            let sdk_path = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !sdk_path.is_empty() {
                cmd.arg("-syslibroot").arg(&sdk_path);
            }
        }

        cmd.arg("-lSystem");
        for obj in obj_files {
            cmd.arg(obj);
        }
        cmd.arg("-o").arg(out_file);
    }

    #[cfg(target_os = "linux")]
    {
        for arg in build_linux_linker_args(obj_files, out_file, static_link) {
            cmd.arg(arg);
        }
    }

    let status = cmd.status().expect("Failed to execute ld");
    if !status.success() {
        eprintln!("Linking failed with status: {status}");
        std::process::exit(1);
    }
}

/// Link object files using libwild (Linux only, in-process linker)
#[cfg(target_os = "linux")]
fn link_with_libwild(obj_files: &[String], out_file: &str, static_link: bool) {
    let args_vec = build_linux_linker_args(obj_files, out_file, static_link);
    let args_refs: Vec<&str> = args_vec.iter().map(|s| s.as_str()).collect();

    let linker = Linker::new();
    let parsed = WildArgs::parse(|| args_refs.iter()).expect("parse args");
    let _ = libwild::setup_tracing(&parsed);
    let activated = parsed.activate_thread_pool().expect("activate args");
    linker.run(&activated).expect("link failed");
}

fn main() {
    let args = Args::parse();

    // On macOS, always use external linker since libwild doesn't support it
    let use_external_linker = args.external_linker || cfg!(target_os = "macos");

    // Separate C files from assembly files
    let (c_files, asm_files): (Vec<_>, Vec<_>) = args.filenames.iter().partition(|f| f.ends_with(".c"));

    // For single-file debug modes (lex, parse, validate, tacky, codegen, -S),
    // only process the first C file
    let first_c_file = c_files.first().map(|s| s.as_str()).unwrap_or("");

    if !first_c_file.is_empty() && (args.lex || args.parse || args.validate || args.tacky || args.codegen || args.s) {
        let Ok(input) = fs::read_to_string(first_c_file) else {
            eprintln!("Failed to read file: {}", first_c_file);
            std::process::exit(1);
        };

        let tokens = lexer::tokenizer(&input).unwrap_or_else(|e| {
            eprintln!("{e:?}");
            std::process::exit(10)
        });

        if args.lex {
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
            println!("{}", ast.itf_string());
            std::process::exit(0);
        }

        let mut ast = ast;
        let validated_result = validate::resolve_program(&mut ast).unwrap_or_else(|e| {
            eprintln!("{e:?}");
            std::process::exit(30);
        });

        if args.validate {
            println!("Program Valid");
            std::process::exit(0);
        }

        let (mut name_gen, symbols) = validated_result;
        let tacky_ast = tacky::tackify_program(&ast, &mut name_gen, &symbols);

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

        // -S flag: emit assembly
        if args.s {
            let (asm, resolver, label_idx) = emit_iced::get_instructions(&code_ast).unwrap();

            let mut formatter = iced_x86::GasFormatter::with_options(Some(Box::new(resolver)), None);
            let ops = formatter.options_mut();
            ops.set_number_base(iced_x86::NumberBase::Decimal);
            ops.set_rip_relative_addresses(true);
            let mut output = MyFormatterOutput::new();

            println!("Generated ASM\n");

            let mut current_offset = 0;
            let mut current_function = 0;

            for ins in asm.iter() {
                if let Some(label_name) = label_idx.get(&current_offset) {
                    if current_function > 0 {
                        println!();
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

            // Print static variables (skip extern vars)
            let mut defined_vars = code_ast
                .static_vars
                .iter()
                .filter_map(|sv| match sv.init {
                    tacky::VarInit::Defined(v) => Some((sv, v)),
                    tacky::VarInit::Extern => None,
                })
                .peekable();
            if defined_vars.peek().is_some() {
                println!();
                for (sv, init_val) in defined_vars {
                    let section = if init_val == 0 { ".bss" } else { ".data" };
                    println!("{}", section.cyan());
                    if sv.global {
                        println!("  {}", format!(".globl {}", sv.name).dimmed());
                    }
                    println!("{}:", sv.name.green());
                    if init_val == 0 {
                        println!("  {}", ".zero 4".yellow());
                    } else {
                        println!("  {} {}", ".long".yellow(), init_val.to_string().cyan());
                    }
                }
            }

            std::process::exit(0);
        }
    }

    // Multi-file compilation: compile each C file to object, then link all together
    let mut obj_files: Vec<String> = Vec::new();

    // Compile each C file to an object file
    for c_file in &c_files {
        let Ok(input) = fs::read_to_string(c_file) else {
            eprintln!("Failed to read file: {}", c_file);
            std::process::exit(1);
        };

        let mut tokens = lexer::tokenizer(&input).unwrap_or_else(|e| {
            eprintln!("{e:?}");
            std::process::exit(10)
        });

        let ast = parser::parse_program(&mut tokens).unwrap_or_else(|e| {
            eprintln!("{e:?}");
            std::process::exit(20)
        });

        let mut ast = ast;
        let validated_result = validate::resolve_program(&mut ast).unwrap_or_else(|e| {
            eprintln!("{e:?}");
            std::process::exit(30);
        });

        let (mut name_gen, symbols) = validated_result;
        let tacky_ast = tacky::tackify_program(&ast, &mut name_gen, &symbols);
        let code_ast = codegen::generate(&tacky_ast);

        let path = Path::new(c_file);
        let obj_file = path.with_extension("o").to_string_lossy().to_string();

        if args.no_iced {
            let asm = emit::emit_program(&code_ast);
            let asm_file = path.with_extension("s").to_string_lossy().to_string();
            fs::write(&asm_file, asm).expect("Failed to write assembly file");

            let status = std::process::Command::new("as")
                .arg(&asm_file)
                .arg("-o")
                .arg(&obj_file)
                .status()
                .expect("Failed to execute as");

            if !status.success() {
                eprintln!("Assembly failed with status: {status}");
                std::process::exit(1);
            }
            fs::remove_file(&asm_file).ok();
        } else {
            let obj = emit_iced::emit_object(&code_ast).expect("iced obj");
            fs::write(&obj_file, &obj).expect("Failed to write object file");
        }

        obj_files.push(obj_file);
    }

    // Assemble any .s files
    for asm_file in &asm_files {
        let path = Path::new(asm_file);
        let obj_file = path.with_extension("o").to_string_lossy().to_string();

        let status = std::process::Command::new("as")
            .arg(asm_file)
            .arg("-o")
            .arg(&obj_file)
            .status()
            .expect("Failed to execute as");

        if !status.success() {
            eprintln!("Assembly of {} failed with status: {status}", asm_file);
            std::process::exit(1);
        }

        obj_files.push(obj_file);
    }

    // Determine output file name
    let first_file = c_files
        .first()
        .or(asm_files.first())
        .map(|s| s.as_str())
        .unwrap_or("a.out");
    let path = Path::new(first_file);
    let out_file = args
        .output
        .unwrap_or_else(|| path.with_extension("").to_string_lossy().to_string());

    if args.c {
        // -c flag: just produce object files, don't link
        std::process::exit(0);
    }

    // Link all object files together
    if use_external_linker {
        link_with_ld(&obj_files, &out_file, args.static_link);
    } else {
        #[cfg(target_os = "linux")]
        link_with_libwild(&obj_files, &out_file, args.static_link);
    }

    // Clean up object files
    for obj in &obj_files {
        fs::remove_file(obj).ok();
    }

    if args.run {
        let run_status = std::process::Command::new(format!("./{}", &out_file))
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
