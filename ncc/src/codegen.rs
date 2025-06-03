use crate::parser::{Expr, Stmt, Function, Identifier, Program};

#[derive(Debug, PartialEq)]
pub enum CodeGenAST {
    Imm(i64),
    Reg(String),
    Mov {
        src: Box<CodeGenAST>,
        dst: Box<CodeGenAST>,
    },
    Ret,
    Function {
        name: String,
        body: Vec<CodeGenAST>,
    },
    Program(Box<CodeGenAST>),
}

fn convert_exp(ast: &Expr) -> CodeGenAST {
    match ast {
        Expr::Int(value) => CodeGenAST::Imm(*value),
    }
}

fn convert_statement(ast: &Stmt) -> Vec<CodeGenAST> {
    match ast {
        Stmt::Return(exp) => {
            let converted_exp = convert_exp(exp);
            vec![CodeGenAST::Mov {dst: Box::new(CodeGenAST::Reg("eax".to_string())), src: Box::new(converted_exp)}, CodeGenAST::Ret]
        }
    }
}

fn convert_function(ast: &Function) -> CodeGenAST {
    match ast { 
        Function { name, body } => {
            let fname = match name{
                Identifier(name) => {name.to_string()}
            };
            let body_instructions = convert_statement(body);
            CodeGenAST::Function {
                name: fname,
                body: body_instructions,
            }
            
        },
    }
}

pub fn generate(ast: Program) -> CodeGenAST {
    match ast {
        Program{function} => {
            CodeGenAST::Program(Box::new(convert_function(&function)))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;
    use crate::lexer::{Token};
    use crate::parser::parse_program;
    use super::*;

    #[test]
    fn basic_return() {
        let mut tokens: VecDeque<Token> = crate::lexer::tests::basic_return(100).into_iter().collect();
        let ast = parse_program(&mut tokens).unwrap();
        let expected = CodeGenAST::Program(Box::new(CodeGenAST::Function {
            name: "main".to_string(),
            body: vec![
                CodeGenAST::Mov {
                    dst: Box::new(CodeGenAST::Reg("eax".to_string())),
                    src: Box::new(CodeGenAST::Imm(100)),
                },
                CodeGenAST::Ret,
            ],
        }));
        assert_eq!(generate(ast), expected);
    }
}