mod codegen;
mod emit;
mod emit_iced;
mod lexer;
mod parser;
mod pretty;
mod tacky;

use crate::pretty::ItfDisplay;
use clap::{ArgGroup, Parser};
use iced_x86::Formatter;
use libwild::{Args as WildArgs, Linker};
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(group(
    ArgGroup::new("mode")
        .required(false)
        .args(&["lex", "parse", "codegen", "s", "tacky", "run"])
))]
struct Args {
    /// Run lexer
    #[arg(long)]
    lex: bool,

    /// Run lexer and parser
    #[arg(long)]
    parse: bool,

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

    /// Use GCC for assembling and linking
    #[arg(long)]
    gcc: bool,

    #[arg(short = 'o', long)]
    output: Option<String>,

    /// Input file (required)
    filename: String,
}

fn main() {
    let args = Args::parse();
    let input = fs::read_to_string(&args.filename).expect("Failed to read input file");
    let tokens = lexer::tokenizer(&input);
    if tokens.is_err() {
        println!("Failed to tokenize: {:?}", tokens.err().unwrap());
        std::process::exit(1)
    }

    if args.lex {
        // stop here if only lexing
        println!("Processed tokens: {:?}", tokens.unwrap());
        std::process::exit(0);
    }

    let mut tokens = tokens.unwrap();
    let ast = parser::parse_program(&mut tokens);
    if ast.is_err() {
        println!("Failed to parse: {:?}", ast.err().unwrap());
        std::process::exit(1);
    }

    if args.parse {
        let ast_val = ast.unwrap();
        println!("{ast_val:?}");
        println!("{}", ast_val.itf_string());
        std::process::exit(0);
    }

    let tacky_ast = tacky::tackify_program(&ast.unwrap());

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

    if !args.gcc { // use iced_x86 for assembly generation
        let obj = emit_iced::emit_object(&code_ast).expect("iced obj");

        if args.s {
            let mut formatter = iced_x86::GasFormatter::new();
            let mut output = String::new();

            println!("Generated ASM");
            let asm = emit_iced::get_instructions(&code_ast).unwrap();
            for ins in asm {
                output.clear();
                formatter.format(&ins, &mut output);
                println!("{output}");
            }
        }

        let obj_file = format!("{out_file}.o");
        fs::write(&obj_file, &obj).expect("Failed to write object file");
        let linker = Linker::new();
        let args_vec = ["-o", &out_file, &obj_file, "--entry=_start"];
        let parsed = WildArgs::parse(args_vec.iter()).expect("parse args");
        let _ = libwild::setup_tracing(&parsed);
        linker.run(&parsed).expect("link failed");
        fs::remove_file(&obj_file).ok();
    } else {
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
            println!("Compilation failed with status: {status}");
            std::process::exit(1);
        }

        fs::remove_file(&asm_file).expect("Failed to delete assembly file");
    }

    if args.run {
        let run_status = std::process::Command::new(&out_file)
            .status()
            .expect("Failed to execute compiled binary");
        println!("Result: {}", run_status.code().unwrap());
    }
}
