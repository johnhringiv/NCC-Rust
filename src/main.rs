mod codegen;
mod emit;
mod lexer;
mod parser;
mod pretty;
mod tacky;

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

    /// Use GCC for linking
    #[arg(long)]
    gcc: bool,

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
        FormatterTextKind::Directive | FormatterTextKind::Keyword => s.bright_yellow(),
        FormatterTextKind::Prefix | FormatterTextKind::Mnemonic => s.bright_magenta(),
        FormatterTextKind::Register => s.bright_blue(),
        FormatterTextKind::Number => s.bright_cyan(),
        _ => s.white(),
    }
}

// todo default to external linker on macOS
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

    let obj = emit::emit_object(&code_ast).expect("iced obj");

    if args.s {
        let mut formatter = iced_x86::GasFormatter::new();
        let ops = formatter.options_mut();
        ops.set_number_base(iced_x86::NumberBase::Decimal);
        let mut output = MyFormatterOutput::new();

        println!("Generated ASM");
        let asm = emit::get_instructions(&code_ast).unwrap();
        for ins in asm {
            output.vec.clear();
            formatter.format(&ins, &mut output);
            for (text, kind) in output.vec.iter() {
                print!("{}", get_color(text.as_str(), *kind));
            }
            println!();
        }
    }

    let obj_file = format!("{out_file}.o");
    fs::write(&obj_file, &obj).expect("Failed to write object file");

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
        let args_vec = ["-o", &out_file, &obj_file, "--entry=_start"];
        let parsed = WildArgs::parse(args_vec.iter()).expect("parse args");
        let _ = libwild::setup_tracing(&parsed);
        linker.run(&parsed).expect("link failed");
    }
    fs::remove_file(&obj_file).ok();
}
