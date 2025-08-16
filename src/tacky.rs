use crate::parser;
use crate::parser::{Block, BlockItem, Declaration, Expr, ForInit, Identifier, IncDec, UnaryOp};
use crate::pretty::{ItfDisplay, Node, cyan, simple_node};
use crate::tacky::Instruction::{JumpIfNotZero, JumpIfZero};
use crate::validate::NameGenerator;

#[derive(Clone, Debug)]
pub enum Val {
    Constant(i64),
    Var(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Subtract,
    Add,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOr,
    BitwiseLeftShift,
    BitwiseRightShift,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

impl From<&parser::BinOp> for BinOp {
    fn from(op: &parser::BinOp) -> Self {
        match op {
            parser::BinOp::Add => BinOp::Add,
            parser::BinOp::Subtract => BinOp::Subtract,
            parser::BinOp::Multiply => BinOp::Multiply,
            parser::BinOp::Divide => BinOp::Divide,
            parser::BinOp::Remainder => BinOp::Remainder,
            parser::BinOp::BitwiseAnd => BinOp::BitwiseAnd,
            parser::BinOp::BitwiseOr => BinOp::BitwiseOr,
            parser::BinOp::BitwiseXOr => BinOp::BitwiseXOr,
            parser::BinOp::BitwiseLeftShift => BinOp::BitwiseLeftShift,
            parser::BinOp::BitwiseRightShift => BinOp::BitwiseRightShift,
            parser::BinOp::Equal => BinOp::Equal,
            parser::BinOp::NotEqual => BinOp::NotEqual,
            parser::BinOp::LessThan => BinOp::LessThan,
            parser::BinOp::LessOrEqual => BinOp::LessOrEqual,
            parser::BinOp::GreaterThan => BinOp::GreaterThan,
            parser::BinOp::GreaterOrEqual => BinOp::GreaterOrEqual,
            _ => unreachable!("Unsupported binary operator {:#?} in tacky conversion", op),
        }
    }
}

impl From<&IncDec> for BinOp {
    fn from(inc_dec: &IncDec) -> Self {
        match inc_dec {
            IncDec::Increment => BinOp::Add,
            IncDec::Decrement => BinOp::Subtract,
        }
    }
}

impl From<&parser::AssignOp> for BinOp {
    fn from(op: &parser::AssignOp) -> Self {
        match op {
            parser::AssignOp::Add => BinOp::Add,
            parser::AssignOp::Subtract => BinOp::Subtract,
            parser::AssignOp::Multiply => BinOp::Multiply,
            parser::AssignOp::Divide => BinOp::Divide,
            parser::AssignOp::Remainder => BinOp::Remainder,
            parser::AssignOp::BitwiseAnd => BinOp::BitwiseAnd,
            parser::AssignOp::BitwiseOr => BinOp::BitwiseOr,
            parser::AssignOp::BitwiseXOr => BinOp::BitwiseXOr,
            parser::AssignOp::BitwiseLeftShift => BinOp::BitwiseLeftShift,
            parser::AssignOp::BitwiseRightShift => BinOp::BitwiseRightShift,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Return(Val),
    Unary { op: UnaryOp, src: Val, dst: Val },
    Binary { op: BinOp, src1: Val, src2: Val, dst: Val },
    Copy { src: Val, dst: Val },
    Jump { target: Identifier },
    JumpIfZero { condition: Val, target: Identifier },
    JumpIfNotZero { condition: Val, target: Identifier },
    Label(Identifier),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Program {
    pub function: FunctionDefinition,
}

/// Converts AST expressions into TACKY IR instructions.
///
/// Returns the `Val` containing the expression's result. For most expressions,
/// this is a temporary variable. For assignments and increment/decrement operators,
/// the return value follows C semantics.
///
/// # Implementation Notes
///
/// - Logical operators (`&&`, `||`) use short-circuit evaluation with jumps
/// - Assignment operators return the assigned value (supporting `a = b = c`)
/// - Postfix operators return the original value before modification
/// - Prefix operators return the new value after modification
/// - Lvalue expressions are currently limited to simple variables
fn tackify_expr(e: &parser::Expr, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator) -> Val {
    match e {
        parser::Expr::Constant(c) => Val::Constant(*c),
        parser::Expr::Unary(op, inner) => {
            let src = tackify_expr(inner, instructions, name_generator);
            let dst = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Unary {
                op: op.clone(),
                src,
                dst: dst.clone(),
            });
            dst
        }
        parser::Expr::Binary(parser::BinOp::And, e1, e2) => {
            let src1 = tackify_expr(e1, instructions, name_generator);
            let false_label = Identifier(name_generator.next("and_false"));
            instructions.push(Instruction::JumpIfZero {
                condition: src1,
                target: false_label.clone(),
            });
            let src2 = tackify_expr(e2, instructions, name_generator);
            instructions.push(Instruction::JumpIfZero {
                condition: src2,
                target: false_label.clone(),
            });
            let result = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Copy {
                src: Val::Constant(1),
                dst: result.clone(),
            });
            let end_label = Identifier(name_generator.next("end"));
            instructions.push(Instruction::Jump {
                target: end_label.clone(),
            });
            instructions.push(Instruction::Label(false_label));
            instructions.push(Instruction::Copy {
                src: Val::Constant(0),
                dst: result.clone(),
            });
            instructions.push(Instruction::Label(end_label));
            result
        }
        parser::Expr::Binary(parser::BinOp::Or, e1, e2) => {
            let src1 = tackify_expr(e1, instructions, name_generator);
            let false_label = Identifier(name_generator.next("or_false"));
            instructions.push(Instruction::JumpIfNotZero {
                condition: src1,
                target: false_label.clone(),
            });
            let src2 = tackify_expr(e2, instructions, name_generator);
            instructions.push(Instruction::JumpIfNotZero {
                condition: src2,
                target: false_label.clone(),
            });
            let result = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Copy {
                src: Val::Constant(0),
                dst: result.clone(),
            });
            let end_label = Identifier(name_generator.next("end"));
            instructions.push(Instruction::Jump {
                target: end_label.clone(),
            });
            instructions.push(Instruction::Label(false_label));
            instructions.push(Instruction::Copy {
                src: Val::Constant(1),
                dst: result.clone(),
            });
            instructions.push(Instruction::Label(end_label));
            result
        }
        parser::Expr::Binary(op, e1, e2) => {
            // Evaluate left-to-right and capture values immediately to avoid undefined behavior
            let mut src1 = tackify_expr(e1, instructions, name_generator);
            // If src1 is a variable, copy it to a temp to capture its current value
            // before evaluating src2 (which might modify it)
            if let Val::Var(_) = &src1 {
                let temp = Val::Var(name_generator.next("binary_left"));
                instructions.push(Instruction::Copy {
                    src: src1.clone(),
                    dst: temp.clone(),
                });
                src1 = temp;
            }
            let src2 = tackify_expr(e2, instructions, name_generator);
            let dst = Val::Var(name_generator.next("temp"));
            instructions.push(Instruction::Binary {
                op: BinOp::from(op),
                src1,
                src2,
                dst: dst.clone(),
            });
            dst
        }
        Expr::Var(Identifier(name), _span) => Val::Var(name.clone()),
        Expr::Assignment(lhs, rhs, _span) => match lhs.as_ref() {
            Expr::Var(Identifier(name), _) => {
                let res = tackify_expr(rhs, instructions, name_generator);
                instructions.push(Instruction::Copy {
                    src: res.clone(),
                    dst: Val::Var(name.clone()),
                });
                Val::Var(name.clone())
            }
            _ => unreachable!("Assignment to non-lvalue"),
        },
        Expr::CompoundAssignment(op, lhs, rhs, _span) => match lhs.as_ref() {
            Expr::Var(Identifier(name), _) => {
                let current_val = Val::Var(name.clone()); // get the original value
                let rhs_val = tackify_expr(rhs, instructions, name_generator);
                let dst = Val::Var(name_generator.next("compound_temp"));
                // do the operation and store the result in a temporary variable
                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: current_val.clone(),
                    src2: rhs_val.clone(),
                    dst: dst.clone(),
                });

                // copy the result back to the original variable
                instructions.push(Instruction::Copy {
                    src: dst.clone(),
                    dst: Val::Var(name.clone()),
                });
                dst
            }
            _ => unreachable!("Compound assignment to non-lvalue"),
        },
        Expr::PostFixOp(op, e, _) => match e.as_ref() {
            Expr::Var(Identifier(name), _) => {
                let current_val = Val::Var(name.clone()); // get the original value
                let org_tmp = Val::Var(name_generator.next("postfix_org"));
                let new_val = Val::Var(name_generator.next("postfix_new"));
                instructions.push(Instruction::Copy {
                    src: current_val.clone(),
                    dst: org_tmp.clone(),
                });
                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: current_val.clone(),
                    src2: Val::Constant(1),
                    dst: new_val.clone(),
                });
                instructions.push(Instruction::Copy {
                    src: new_val.clone(),
                    dst: Val::Var(name.clone()),
                });
                org_tmp
            }
            _ => unreachable!("Postfix on non-lvalue"),
        },
        Expr::PreFixOp(op, e, _) => match e.as_ref() {
            Expr::Var(Identifier(name), _) => {
                let current_val = Val::Var(name.clone()); // get the original value
                let new_val = Val::Var(name_generator.next("prefix_new"));
                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: current_val.clone(),
                    src2: Val::Constant(1),
                    dst: new_val.clone(),
                });
                instructions.push(Instruction::Copy {
                    src: new_val.clone(),
                    dst: Val::Var(name.clone()),
                });
                new_val
            }
            _ => unreachable!("Prefix on non-lvalue"),
        },
        Expr::Conditional(cond, e1, e2) => {
            let c = tackify_expr(cond, instructions, name_generator);
            let result = Val::Var(name_generator.next("temp"));
            let cond_false = Identifier(name_generator.next("cond_false"));
            let cond_end = Identifier(name_generator.next("cond_end"));
            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: cond_false.clone(),
            });
            let left = tackify_expr(e1, instructions, name_generator);
            instructions.push(Instruction::Copy {
                src: left,
                dst: result.clone(),
            });
            instructions.push(Instruction::Jump {
                target: cond_end.clone(),
            });
            instructions.push(Instruction::Label(cond_false.clone()));
            let right = tackify_expr(e2, instructions, name_generator);
            instructions.push(Instruction::Copy {
                src: right,
                dst: result.clone(),
            });
            instructions.push(Instruction::Label(cond_end.clone()));
            result
        }
    }
}

fn tackify_stmt(stmt: &parser::Stmt, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator) {
    match &stmt {
        parser::Stmt::Return(expr) => {
            let val = tackify_expr(expr, instructions, name_generator);
            instructions.push(Instruction::Return(val));
        }
        parser::Stmt::Expression(e) => {
            tackify_expr(e, instructions, name_generator);
        }
        parser::Stmt::Null => (),
        &parser::Stmt::If(cond, then_stmt, else_stmt) => {
            let c = tackify_expr(cond, instructions, name_generator);
            let end_label = Identifier(name_generator.next("fi"));

            if let Some(e) = &**else_stmt {
                // if with else
                let else_label = Identifier(name_generator.next("if_else"));
                instructions.push(Instruction::JumpIfZero {
                    condition: c,
                    target: else_label.clone(),
                });
                tackify_stmt(then_stmt, instructions, name_generator);
                instructions.push(Instruction::Jump {
                    target: end_label.clone(),
                });
                instructions.push(Instruction::Label(else_label));
                tackify_stmt(e, instructions, name_generator);
            } else {
                // no else
                instructions.push(Instruction::JumpIfZero {
                    condition: c,
                    target: end_label.clone(),
                });
                tackify_stmt(then_stmt, instructions, name_generator);
            }
            instructions.push(Instruction::Label(end_label.clone()));
        }
        &parser::Stmt::Labeled(label_name, stmt, _) => {
            instructions.push(Instruction::Label(Identifier(label_name.to_string())));
            tackify_stmt(stmt, instructions, name_generator)
        }
        &parser::Stmt::Goto(label_name, _) => instructions.push(Instruction::Jump {
            target: Identifier(label_name.to_string()),
        }),
        &parser::Stmt::Compound(block) => tackify_block(block, instructions, name_generator),
        parser::Stmt::Break(target, _) | parser::Stmt::Continue(target, _) => {
            instructions.push(Instruction::Jump { target: target.clone() })
        }
        parser::Stmt::While(condition, body, label) => {
            let continue_label = Identifier(format!("continue_loop.{label}"));
            let break_label = Identifier(format!("break_loop.{label}"));

            instructions.push(Instruction::Label(continue_label.clone()));
            let c = tackify_expr(condition, instructions, name_generator);
            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: break_label.clone(),
            });
            tackify_stmt(body, instructions, name_generator);
            instructions.push(Instruction::Jump { target: continue_label });
            instructions.push(Instruction::Label(break_label));
        }
        parser::Stmt::DoWhile(body, condition, label) => {
            let start_label = Identifier(format!("start_loop.{label}"));
            let continue_label = Identifier(format!("continue_loop.{label}"));
            let break_label = Identifier(format!("break_loop.{label}"));

            instructions.push(Instruction::Label(start_label.clone()));
            tackify_stmt(body, instructions, name_generator);
            instructions.push(Instruction::Label(continue_label));
            let c = tackify_expr(condition, instructions, name_generator);
            instructions.push(JumpIfNotZero {
                condition: c,
                target: start_label,
            });
            instructions.push(Instruction::Label(break_label));
        }
        parser::Stmt::For(init, condition, post, body, label) => {
            match init {
                ForInit::InitDecl(dec) => tackify_declaration(dec, instructions, name_generator),
                ForInit::InitExp(exp) => {
                    if let Some(e) = exp {
                        tackify_expr(e, instructions, name_generator);
                    }
                }
            }

            let start_label = Identifier(format!("start_loop.{label}"));
            let continue_label = Identifier(format!("continue_loop.{label}"));
            let break_label = Identifier(format!("break_loop.{label}"));

            instructions.push(Instruction::Label(start_label.clone()));
            if let Some(condition) = condition {
                let c = tackify_expr(condition, instructions, name_generator);
                instructions.push(JumpIfZero {
                    condition: c,
                    target: break_label.clone(),
                })
            }
            tackify_stmt(body, instructions, name_generator);
            instructions.push(Instruction::Label(continue_label));
            if let Some(post) = post {
                tackify_expr(post, instructions, name_generator);
            }
            instructions.push(Instruction::Jump { target: start_label });
            instructions.push(Instruction::Label(break_label));
        }
    }
}

fn tackify_block(block: &Block, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator) {
    for item in block {
        match item {
            BlockItem::Statement(stmt) => tackify_stmt(stmt, instructions, name_generator),
            BlockItem::Declaration(dec) => tackify_declaration(dec, instructions, name_generator),
        }
    }
}

fn tackify_declaration(
    declaration: &Declaration,
    instructions: &mut Vec<Instruction>,
    name_generator: &mut NameGenerator,
) {
    if let Declaration {
        name,
        init: Some(e),
        span,
    } = declaration
    {
        let ass = Expr::Assignment(Box::new(Expr::Var(name.clone(), *span)), Box::new(e.clone()), *span);
        tackify_expr(&ass, instructions, name_generator);
    }
}

fn tackify_function(func: &parser::Function, name_generator: &mut NameGenerator) -> FunctionDefinition {
    let mut instructions = Vec::new();
    let parser::Identifier(name) = &func.name;
    tackify_block(&func.body, &mut instructions, name_generator);

    // Return 0 if the function has no return statement
    // will not be reached if the function has a return statement
    instructions.push(Instruction::Return(Val::Constant(0)));

    FunctionDefinition {
        name: name.clone(),
        body: instructions,
    }
}

pub fn tackify_program(program: &parser::Program, name_generator: &mut NameGenerator) -> Program {
    let function = tackify_function(&program.function, name_generator);

    Program { function }
}
simple_node!(BinOp);

impl ItfDisplay for Val {
    fn itf_node(&self) -> Node {
        match self {
            Val::Constant(c) => Node::branch(cyan("Const"), vec![c.itf_node()]),
            Val::Var(s) => Node::branch(cyan("Var"), vec![s.itf_node()]),
        }
    }
}

impl ItfDisplay for Instruction {
    fn itf_node(&self) -> Node {
        match self {
            Instruction::Return(v) => Node::branch(cyan("Return"), vec![v.itf_node()]),
            Instruction::Unary { op, src, dst } => {
                Node::branch(cyan("Unary"), vec![op.itf_node(), src.itf_node(), dst.itf_node()])
            }
            Instruction::Binary { op, src1, src2, dst } => Node::branch(
                cyan("Binary"),
                vec![op.itf_node(), src1.itf_node(), src2.itf_node(), dst.itf_node()],
            ),
            Instruction::Copy { src, dst } => Node::branch(cyan("Copy"), vec![src.itf_node(), dst.itf_node()]),
            Instruction::Jump { target } => Node::branch(cyan("Jump"), vec![target.itf_node()]),
            Instruction::JumpIfZero { condition, target } => {
                Node::branch(cyan("JumpIfZero"), vec![condition.itf_node(), target.itf_node()])
            }
            Instruction::JumpIfNotZero { condition, target } => {
                Node::branch(cyan("JumpIfNotZero"), vec![condition.itf_node(), target.itf_node()])
            }
            Instruction::Label(lbl) => Node::branch(cyan("Label"), vec![lbl.itf_node()]),
        }
    }
}

impl ItfDisplay for FunctionDefinition {
    fn itf_node(&self) -> Node {
        let name_line = Node::leaf(format!("name: {}", self.name.itf_node().text));
        let body_children: Vec<Node> = self.body.iter().map(|i| i.itf_node()).collect();
        let body_node = Node::branch(cyan("body"), body_children);
        Node::branch(cyan("FunctionDefinition"), vec![name_line, body_node])
    }
}

impl ItfDisplay for Program {
    fn itf_node(&self) -> Node {
        Node::branch(cyan("Program"), vec![self.function.itf_node()])
    }
}
