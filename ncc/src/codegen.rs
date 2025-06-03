use crate::parser::AST;

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

fn convert_exp(ast: &AST) -> CodeGenAST {
    match ast {
        AST::Int(value) => CodeGenAST::Imm(*value),
        _ => unreachable!("Convert Expression: expected Int, found {:?}", ast),
    }
}

fn convert_statement(ast: &AST) -> Vec<CodeGenAST> {
    match ast {
        AST::Return(exp) => {
            let converted_exp = convert_exp(exp);
            vec![CodeGenAST::Mov {dst: Box::new(CodeGenAST::Reg("eax".to_string())), src: Box::new(converted_exp)}, CodeGenAST::Ret]
        }
        _ => unreachable!("Convert Statement: expected Return, found {:?}", ast),
    }
}

fn convert_function(ast: &AST) -> CodeGenAST {
    match ast { 
        AST::Function { name, body } => {
            let fname = match &**name{
                AST::Identifier(name) => {name.to_string()}
                _ => unreachable!("Convert Function: expected Identifier, found {:?}", name),
            };
            let body_instructions = convert_statement(body);
            CodeGenAST::Function {
                name: fname,
                body: body_instructions,
            }
            
        },
        _ => unreachable!("Convert Function: expected Function, found {:?}", ast),
    }
}

pub fn generate(ast: AST) -> CodeGenAST {
    match ast {
        AST::Program(fun_def) => {
            CodeGenAST::Program(Box::new(convert_function(&fun_def)))
        }
        _ => unreachable!("Gen: expected Program, found {:?}", ast)
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