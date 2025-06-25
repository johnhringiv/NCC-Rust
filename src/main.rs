mod lexer;
mod parser;
mod codegen;
mod emit;
mod tacky;
mod pretty;

use std::fs;
use std::path::Path;
use clap::{ArgGroup, Parser};
use crate::pretty::ItfDisplay;

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

    #[arg(short = 'o', long)]
    output: Option<String>,

    /// Input file (required)
    filename: String,
}

fn main() {
    let args = Args::parse();
    let input = fs::read_to_string(&args.filename)
        .expect("Failed to read input file");
    let tokens = lexer::tokenizer(&input);
    if tokens.is_err() {
            println!("Failed to tokenize: {:?}", tokens.err().unwrap());
            std::process::exit(1)
    }

    if args.lex { // stop here if only lexing
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
        println!("{}", ast_val.itf_string());
        std::process::exit(0);
    }
    
    let tacky_ast = tacky::tackify_program(&ast.unwrap());
    
    if args.tacky {
        println!("{}", tacky_ast.itf_string());
        std::process::exit(0);
    }

    let code_ast = codegen::generate(&tacky_ast);
    if args.codegen {
        println!("{}", code_ast.itf_string());
        std::process::exit(0);
    }
    
    let asm = emit::emit_program(&code_ast);
    if args.s {
        print!("Generated asm:\n\n{}", asm);
    }
    let path = Path::new(&args.filename);
    let out_file = args.output.unwrap_or_else(|| path.with_extension("").to_string_lossy().to_string());
    let asm_file = format!("{}.s", out_file);
    
    fs::write(&asm_file, asm)
        .expect("Failed to write assembly file");
    
    let status = std::process::Command::new("gcc")
        .arg(&asm_file)
        .arg("-o")
        .arg(out_file.clone())
        .status()
        .expect("Failed to execute gcc");
    
    if !status.success() {
        println!("Compilation failed with status: {}", status);
        std::process::exit(1);
    }
    
    fs::remove_file(&asm_file)
        .expect("Failed to delete assembly file");
    
    if args.run {
        let run_status = std::process::Command::new(&out_file)
            .status()
            .expect("Failed to execute compiled binary");
        println!("Result: {}", run_status.code().unwrap());
    }
}
