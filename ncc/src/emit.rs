use crate::codegen::CodeGenAST;

fn emit(ast : &CodeGenAST) -> String {
    let mut output = String::new();
    
    match ast {
        CodeGenAST::Imm(i) => {
            output.push_str(&format!("${}", i));
        }
        CodeGenAST::Reg(r) => {
            output.push_str(&format!("%{}", r))
        }
        CodeGenAST::Mov {src, dst} => {
            output.push_str(&format!("movl {}, {}\n", emit(src), emit(dst)));
        }
        CodeGenAST::Ret => {
            output.push_str("ret\n");
        }
        CodeGenAST::Function { name, body } => {
            let processed_name = if cfg!(target_os = "macos") {
                format!("_{}", name)
            } else { name.to_string() };
            output.push_str(&format!("\t.global {}\n", processed_name));
            output.push_str(&format!("{}:\n", processed_name));
            for ins in body {
                output.push_str(&format!("\t{}", emit(ins)));
            }
        }
        CodeGenAST::Program(fun_def) => {
            output.push_str(&*emit(fun_def));
        }
    }
    output
}

pub fn emit_asm(ast: &CodeGenAST) -> String {
    let mut output = emit(ast);
    if cfg!(target_os = "linux") {
        output.push_str("\n.section .note.GNU-stack,\"\",@progbits\n");
    }
    output
}