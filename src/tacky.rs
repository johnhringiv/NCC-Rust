//! # Tackifier — Typed AST to Three-Address Code IR (TACKY)
//!
//! Flattens the typed AST into TACKY, a linear three-address code intermediate
//! representation with explicit control flow via jumps and labels.
//!
//! ## Technical Approach
//!
//! Each nested expression is recursively decomposed into a sequence of simple
//! [`Instruction`]s that operate on at most two source [`Val`]s and one destination.
//! Temporaries are generated via [`NameGenerator`] and tracked in `temp_types` for
//! downstream passes. Control flow (if/while/for/switch) is lowered to conditional
//! and unconditional jumps with generated labels.
//!
//! ## What This Pass Accomplishes
//!
//! - Flattens nested expressions into linear instruction sequences
//! - Makes all control flow explicit (no structured if/while/for in output)
//! - Implements short-circuit evaluation for `&&` and `||`
//! - Implements left-to-right evaluation by capturing operands to temporaries
//! - Lowers switch statements to cascading equality comparisons with jumps
//! - Extracts static variables from the [`SymbolTable`] into [`StaticVariable`] entries
//! - Ensures every function ends with a `Return` (inserts default `return 0` if missing)
//!
//! ## Call Order
//!
//! ```text
//! tackify_program()                     — public entry point
//!   ├─ tackify_function()               — per function (with body only)
//!   │    └─ tackify_block()             — process compound statements
//!   │         ├─ tackify_var_declaration() — emit local var initializers (skip static/extern)
//!   │         └─ tackify_stmt()         — lower statements to jumps/labels
//!   │              └─ tackify_expr()    — core: flatten expressions to instructions
//!   │                   └─ emit_cast()  — int<->long conversion instructions
//!   └─ convert_symbols_to_tacky()       — extract static vars from symbol table
//! ```

use std::cmp::PartialEq;
use std::collections::HashMap;
use crate::parser;
use crate::parser::{Type, Declaration, Identifier, IncDec, UnaryOp, VarDeclaration, Const, SwitchIntType, AssignOp};
use crate::validate::{Expr, TypedExpression, InitialValue, NameGenerator, SymbolTable, StaticInt, Stmt, ForInit, Block, BlockItem, TypedVarDeclaration, TypedFunction, TypedProgram, TypedDeclaration, get_common_type};

#[derive(Clone, Debug)]
pub enum Val {
    Constant(Const),
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
    SignExtend {
        src: Val,
        dst: Val,
    },
    Truncate {
        src: Val,
        dst: Val,
    },
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Val,
    },
    Binary {
        op: BinOp,
        src1: Val,
        src2: Val,
        dst: Val,
    },
    Copy {
        src: Val,
        dst: Val,
    },
    Jump {
        target: Identifier,
    },
    JumpIfZero {
        condition: Val,
        target: Identifier,
    },
    JumpIfNotZero {
        condition: Val,
        target: Identifier,
    },
    Label(Identifier),
    FunCall {
        fun_name: Identifier,
        args: Vec<Val>,
        dst: Val,
    },
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub params: Vec<Identifier>,
    pub body: Vec<Instruction>,
    pub global: bool,
    pub temp_types: HashMap<String, Type>,
}

/// Storage initialization for static variables.
#[derive(Debug, Clone)]
pub enum VarInit {
    /// Variable defined here with initial value (0 = .bss, non-zero = .data)
    Defined(StaticInt),
    /// Extern variable - no storage allocated, resolved by linker
    Extern,
}

#[derive(Debug, Clone)]
pub struct StaticVariable {
    pub name: String,
    pub global: bool,
    pub init: VarInit,
    pub var_type: Type,
}

#[derive(Debug)]
pub struct Program {
    pub function_defs: Vec<FunctionDefinition>,
    pub static_vars: Vec<StaticVariable>,
}

fn emit_cast(src: Val, dst: Val, src_type: &Type, dst_type: &Type, instructions: &mut Vec<Instruction>) {
    match (src_type, dst_type) {
        (Type::Int, Type::Long) => instructions.push(Instruction::SignExtend { src, dst }),
        (Type::Long, Type::Int) => instructions.push(Instruction::Truncate { src, dst }),
        _ => unreachable!("Unsupported cast: {:?} -> {:?}", src_type, dst_type),
    }
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
fn tackify_expr(e: &TypedExpression, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator, temp_types: &mut HashMap<String, Type>) -> Val {
    let e_type = e.exp_type.clone();
    match &e.exp {
        Expr::Const(c) => Val::Constant(c.clone()),
        Expr::Cast(target_type, inner) => {
            let source_type = inner.exp_type.clone();
            let result = tackify_expr(inner, instructions, name_generator, temp_types);
            if *target_type == source_type {
                return result
            }
            let dst_name = name_generator.next("temp");
            temp_types.insert(dst_name.to_string(), target_type.clone());
            let dst = Val::Var(dst_name);
            emit_cast(result, dst.clone(), &source_type, target_type, instructions);
            dst
        }
        Expr::Unary(op, inner) => {
            let src = tackify_expr(inner, instructions, name_generator, temp_types);
            let dst_name = name_generator.next("temp");
            temp_types.insert(dst_name.to_string(), e.exp_type.clone());
            let dst = Val::Var(dst_name);
            instructions.push(Instruction::Unary {
                op: op.clone(),
                src,
                dst: dst.clone(),
            });
            dst
        }
        Expr::Binary(parser::BinOp::And, e1, e2) => {
            let src1 = tackify_expr(e1, instructions, name_generator, temp_types);
            let false_label = Identifier(name_generator.next("and_false"));
            instructions.push(Instruction::JumpIfZero {
                condition: src1,
                target: false_label.clone(),
            });
            let src2 = tackify_expr(e2, instructions, name_generator, temp_types);
            instructions.push(Instruction::JumpIfZero {
                condition: src2,
                target: false_label.clone(),
            });
            let result_name = name_generator.next("temp");
            temp_types.insert(result_name.to_string(), e.exp_type.clone());
            let result = Val::Var(result_name);
            instructions.push(Instruction::Copy {
                src: Val::Constant(Const::ConstInt(1)),
                dst: result.clone(),
            });
            let end_label = Identifier(name_generator.next("end"));
            instructions.push(Instruction::Jump {
                target: end_label.clone(),
            });
            instructions.push(Instruction::Label(false_label));
            instructions.push(Instruction::Copy {
                src: Val::Constant(Const::ConstInt(0)),
                dst: result.clone(),
            });
            instructions.push(Instruction::Label(end_label));
            result
        }
        Expr::Binary(parser::BinOp::Or, e1, e2) => {
            let src1 = tackify_expr(e1, instructions, name_generator, temp_types);
            let false_label = Identifier(name_generator.next("or_false"));
            instructions.push(Instruction::JumpIfNotZero {
                condition: src1,
                target: false_label.clone(),
            });
            let src2 = tackify_expr(e2, instructions, name_generator, temp_types);
            instructions.push(Instruction::JumpIfNotZero {
                condition: src2,
                target: false_label.clone(),
            });
            let result_name = name_generator.next("temp");
            temp_types.insert(result_name.to_string(), e.exp_type.clone());
            let result = Val::Var(result_name);
            instructions.push(Instruction::Copy {
                src: Val::Constant(Const::ConstInt(0)),
                dst: result.clone(),
            });
            let end_label = Identifier(name_generator.next("end"));
            instructions.push(Instruction::Jump {
                target: end_label.clone(),
            });
            instructions.push(Instruction::Label(false_label));
            instructions.push(Instruction::Copy {
                src: Val::Constant(Const::ConstInt(1)),
                dst: result.clone(),
            });
            instructions.push(Instruction::Label(end_label));
            result
        }
        Expr::Binary(op, e1, e2) => {
            // Evaluate left-to-right and capture values immediately to avoid undefined behavior
            let mut src1 = tackify_expr(e1, instructions, name_generator, temp_types);
            // If src1 is a variable, copy it to a temp to capture its current value
            // before evaluating src2 (which might modify it)
            if let Val::Var(_) = &src1 {
                let temp_name = name_generator.next("binary_left");
                temp_types.insert(temp_name.to_string(), e1.exp_type.clone());
                let temp = Val::Var(temp_name);
                instructions.push(Instruction::Copy {
                    src: src1.clone(),
                    dst: temp.clone(),
                });
                src1 = temp;
            }
            let src2 = tackify_expr(e2, instructions, name_generator, temp_types);
            let dst_name = name_generator.next("temp");
            temp_types.insert(dst_name.to_string(), e.exp_type.clone());
            let dst = Val::Var(dst_name);
            instructions.push(Instruction::Binary {
                op: BinOp::from(op),
                src1,
                src2,
                dst: dst.clone(),
            });
            dst
        }
        Expr::Var(Identifier(name)) => Val::Var(name.clone()),
        Expr::Assignment(lhs, rhs) => match &lhs.exp {
            Expr::Var(Identifier(name)) => {
                let res = tackify_expr(rhs, instructions, name_generator, temp_types);
                instructions.push(Instruction::Copy {
                    src: res.clone(),
                    dst: Val::Var(name.clone()),
                });
                Val::Var(name.clone())
            }
            _ => unreachable!("Assignment to non-lvalue"),
        }
        Expr::CompoundAssignment(op, lhs, rhs, op_type) => match &lhs.exp {
            Expr::Var(Identifier(name)) => {
                let current_val = Val::Var(name.clone());
                let rhs_val = tackify_expr(rhs, instructions, name_generator, temp_types);

                // Promote RHS to op_type if needed
                let src2 = if *op_type != rhs.exp_type {
                    let cast_name = name_generator.next("cast_tmp");
                    temp_types.insert(cast_name.to_string(), op_type.clone());
                    let cast_dst = Val::Var(cast_name);
                    emit_cast(rhs_val, cast_dst.clone(), &rhs.exp_type, op_type, instructions);
                    cast_dst
                } else {
                    rhs_val
                };

                // Promote LHS to op_type if needed
                let src1 = if *op_type != lhs.exp_type {
                    let cast_name = name_generator.next("cast_tmp");
                    temp_types.insert(cast_name.to_string(), op_type.clone());
                    let cast_dst = Val::Var(cast_name);
                    emit_cast(current_val.clone(), cast_dst.clone(), &lhs.exp_type, op_type, instructions);
                    cast_dst
                } else {
                    current_val.clone()
                };

                // Perform operation; use temp if result needs truncation
                if *op_type != lhs.exp_type {
                    let dst_name = name_generator.next("compound_temp");
                    temp_types.insert(dst_name.to_string(), op_type.clone());
                    let dst = Val::Var(dst_name);
                    instructions.push(Instruction::Binary {
                        op: op.into(),
                        src1,
                        src2,
                        dst: dst.clone(),
                    });
                    emit_cast(dst, current_val.clone(), op_type, &lhs.exp_type, instructions);
                } else {
                    instructions.push(Instruction::Binary {
                        op: op.into(),
                        src1,
                        src2,
                        dst: current_val.clone(),
                    });
                }
                current_val
            }
            _ => unreachable!("Compound assignment to non-lvalue"),
        },
        Expr::PostFixOp(op, e) => match &e.exp {
            Expr::Var(Identifier(name)) => {
                let current_val = Val::Var(name.clone()); // get the original value
                let org_tmp_name = name_generator.next("postfix_org");
                temp_types.insert(org_tmp_name.to_string(), e.exp_type.clone());
                let org_tmp = Val::Var(org_tmp_name);
                instructions.push(Instruction::Copy {
                    src: current_val.clone(),
                    dst: org_tmp.clone(),
                });
                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: current_val.clone(),
                    src2: Val::Constant(e_type.get_1()),
                    dst: current_val.clone(),
                });
                instructions.push(Instruction::Copy {
                    src: current_val.clone(),
                    dst: Val::Var(name.clone()),
                });
                org_tmp
            }
            _ => unreachable!("Postfix on non-lvalue"),
        },
        Expr::PreFixOp(op, e) => match &e.exp {
            Expr::Var(Identifier(name)) => {
                let current_val = Val::Var(name.clone()); // get the original value
                instructions.push(Instruction::Binary {
                    op: op.into(),
                    src1: current_val.clone(),
                    src2: Val::Constant(e_type.get_1()),
                    dst: current_val.clone(),
                });
                instructions.push(Instruction::Copy {
                    src: current_val.clone(),
                    dst: Val::Var(name.clone()),
                });
                current_val
            }
            _ => unreachable!("Prefix on non-lvalue"),
        },
        Expr::Conditional(cond, e1, e2) => {
            let c = tackify_expr(cond, instructions, name_generator, temp_types);
            let result_name = name_generator.next("temp");
            temp_types.insert(result_name.to_string(), e.exp_type.clone());
            let result = Val::Var(result_name);
            let cond_false = Identifier(name_generator.next("cond_false"));
            let cond_end = Identifier(name_generator.next("cond_end"));
            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: cond_false.clone(),
            });
            let left = tackify_expr(e1, instructions, name_generator, temp_types);
            instructions.push(Instruction::Copy {
                src: left,
                dst: result.clone(),
            });
            instructions.push(Instruction::Jump {
                target: cond_end.clone(),
            });
            instructions.push(Instruction::Label(cond_false.clone()));
            let right = tackify_expr(e2, instructions, name_generator, temp_types);
            instructions.push(Instruction::Copy {
                src: right,
                dst: result.clone(),
            });
            instructions.push(Instruction::Label(cond_end.clone()));
            result
        }
        Expr::FunctionCall(fun_name, params) => {
            let mut tacky_params = vec![];
            for param in params {
                let mut tacky_param = tackify_expr(param, instructions, name_generator, temp_types);
                if let Val::Var(_) = &tacky_param {
                    let temp_name = name_generator.next("arg");
                    temp_types.insert(temp_name.to_string(), param.exp_type.clone());
                    let temp = Val::Var(temp_name);
                    instructions.push(Instruction::Copy {
                        src: tacky_param,
                        dst: temp.clone(),
                    });
                    tacky_param = temp;
                }
                tacky_params.push(tacky_param);
            }
            let dst_name = name_generator.next("call");
            temp_types.insert(dst_name.to_string(), e.exp_type.clone());
            let dst = Val::Var(dst_name);
            instructions.push(Instruction::FunCall {
                fun_name: fun_name.clone(),
                args: tacky_params,
                dst: dst.clone(),
            });
            dst
        }
    }
}

fn tackify_stmt(stmt: &Stmt, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator, temp_types: &mut HashMap<String, Type>) {
    match stmt {
        Stmt::Return(expr) => {
            let val = tackify_expr(expr, instructions, name_generator, temp_types);
            instructions.push(Instruction::Return(val));
        }
        Stmt::Expression(e) => {
            tackify_expr(e, instructions, name_generator, temp_types);
        }
        Stmt::Null => (),
        Stmt::If(cond, then_stmt, else_stmt) => {
            let c = tackify_expr(cond, instructions, name_generator, temp_types);
            let end_label = Identifier(name_generator.next("fi"));

            if let Some(e) = else_stmt {
                // if with else
                let else_label = Identifier(name_generator.next("if_else"));
                instructions.push(Instruction::JumpIfZero {
                    condition: c,
                    target: else_label.clone(),
                });
                tackify_stmt(then_stmt, instructions, name_generator, temp_types);
                instructions.push(Instruction::Jump {
                    target: end_label.clone(),
                });
                instructions.push(Instruction::Label(else_label));
                tackify_stmt(e, instructions, name_generator, temp_types);
            } else {
                // no else
                instructions.push(Instruction::JumpIfZero {
                    condition: c,
                    target: end_label.clone(),
                });
                tackify_stmt(then_stmt, instructions, name_generator, temp_types);
            }
            instructions.push(Instruction::Label(end_label.clone()));
        }
        Stmt::Labeled(label_name, stmt) => {
            instructions.push(Instruction::Label(Identifier(label_name.to_string())));
            tackify_stmt(stmt, instructions, name_generator, temp_types);
        }
        Stmt::Goto(label_name) => instructions.push(Instruction::Jump {
            target: Identifier(label_name.to_string()),
        }),
        Stmt::Compound(block) => tackify_block(block, instructions, name_generator, temp_types),
        Stmt::Break(target) | Stmt::Continue(target) => {
            instructions.push(Instruction::Jump { target: target.clone() })
        }
        Stmt::While(condition, body, label) => {
            let continue_label = Identifier(format!("continue_loop.{label}"));
            let break_label = Identifier(format!("break_loop.{label}"));

            instructions.push(Instruction::Label(continue_label.clone()));
            let c = tackify_expr(condition, instructions, name_generator, temp_types);
            instructions.push(Instruction::JumpIfZero {
                condition: c,
                target: break_label.clone(),
            });
            tackify_stmt(body, instructions, name_generator, temp_types);
            instructions.push(Instruction::Jump { target: continue_label });
            instructions.push(Instruction::Label(break_label));
        }
        Stmt::DoWhile(body, condition, label) => {
            let start_label = Identifier(format!("start_loop.{label}"));
            let continue_label = Identifier(format!("continue_loop.{label}"));
            let break_label = Identifier(format!("break_loop.{label}"));

            instructions.push(Instruction::Label(start_label.clone()));
            tackify_stmt(body, instructions, name_generator, temp_types);
            instructions.push(Instruction::Label(continue_label));
            let c = tackify_expr(condition, instructions, name_generator, temp_types);
            instructions.push(Instruction::JumpIfNotZero {
                condition: c,
                target: start_label,
            });
            instructions.push(Instruction::Label(break_label));
        }
        Stmt::For(init, condition, post, body, label) => {
            match init {
                ForInit::InitDecl(dec) => tackify_var_declaration(dec, instructions, name_generator, temp_types),
                ForInit::InitExp(exp) => {
                    if let Some(e) = exp {
                        tackify_expr(e, instructions, name_generator, temp_types);
                    }
                }
            }

            let start_label = Identifier(format!("start_loop.{label}"));
            let continue_label = Identifier(format!("continue_loop.{label}"));
            let break_label = Identifier(format!("break_loop.{label}"));

            instructions.push(Instruction::Label(start_label.clone()));
            if let Some(condition) = condition {
                let c = tackify_expr(condition, instructions, name_generator, temp_types);
                instructions.push(Instruction::JumpIfZero {
                    condition: c,
                    target: break_label.clone(),
                })
            }
            tackify_stmt(body, instructions, name_generator, temp_types);
            instructions.push(Instruction::Label(continue_label));
            if let Some(post) = post {
                tackify_expr(post, instructions, name_generator, temp_types);
            }
            instructions.push(Instruction::Jump { target: start_label });
            instructions.push(Instruction::Label(break_label));
        }
        Stmt::Switch(exp, stmt, switch_num, cases) => {
            let end_label = Identifier(format!("break_switch.{switch_num}"));
            let switch_val = tackify_expr(exp, instructions, name_generator, temp_types);

            // Build conditional jumps for each case
            // this can be optimized with binary search and/or jump tables
            let mut default_label = None;
            for case in cases {
                match case {
                    SwitchIntType::Int(_) | SwitchIntType::Long(_) => {
                        let case_label = Identifier(case.label_str(*switch_num));
                        let case_const = match case {
                            SwitchIntType::Int(c) => Const::ConstInt(*c),
                            SwitchIntType::Long(c) => Const::ConstLong(*c),
                            SwitchIntType::Default => unreachable!()
                        };
                        let cond_name = name_generator.next("case_cond");
                        temp_types.insert(cond_name.to_string(), Type::Int);
                        let cond = Val::Var(cond_name);
                        instructions.push(Instruction::Binary {
                            op: BinOp::Equal,
                            src1: switch_val.clone(),
                            src2: Val::Constant(case_const),
                            dst: cond.clone(),
                        });
                        instructions.push(Instruction::JumpIfNotZero {
                            condition: cond,
                            target: case_label,
                        });
                    }
                    SwitchIntType::Default => {
                        default_label = Some(Identifier(case.label_str(*switch_num)));
                    }
                }
            }

            // Jump to default if it exists, otherwise jump to end
            // cases following default will execute this is expected
            if let Some(default) = default_label {
                instructions.push(Instruction::Jump { target: default });
            } else {
                instructions.push(Instruction::Jump {
                    target: end_label.clone(),
                });
            }

            tackify_stmt(stmt, instructions, name_generator, temp_types);
            instructions.push(Instruction::Label(end_label));
        }
        Stmt::Case(_, stmt, label) | Stmt::Default(stmt, label) => {
            instructions.push(Instruction::Label(label.clone()));
            tackify_stmt(stmt, instructions, name_generator, temp_types);
        }
    }
}

fn tackify_block(block: &Block, instructions: &mut Vec<Instruction>, name_generator: &mut NameGenerator, temp_types: &mut HashMap<String, Type>) {
    for item in block {
        match item {
            BlockItem::Statement(stmt) => tackify_stmt(stmt, instructions, name_generator, temp_types),
            BlockItem::Declaration(dec) => match dec {
                TypedDeclaration::Variable(var) => tackify_var_declaration(var, instructions, name_generator, temp_types),
                TypedDeclaration::Function(_) => {}  // Function declarations in blocks have no body
            },
        }
    }
}

/// Emits runtime initialization code for automatic (local) variables only.
///
/// Static variables are initialized at compile time in the data section.
/// Extern variables have no initialization code emitted.
fn tackify_var_declaration(
    declaration: &TypedVarDeclaration,
    instructions: &mut Vec<Instruction>,
    name_generator: &mut NameGenerator,
    temp_types: &mut HashMap<String, Type>
) {
    if declaration.storage_class.is_none() {
        if let Some(init_exp) = &declaration.init {
            let res = tackify_expr(init_exp, instructions, name_generator, temp_types);
            instructions.push(Instruction::Copy { src: res, dst: Val::Var(declaration.name.0.clone()) });
        }
    }
}

fn tackify_function(
    func: &TypedFunction,
    name_generator: &mut NameGenerator,
) -> FunctionDefinition {
    let mut instructions = Vec::new();
    let mut temp_types = HashMap::new();
    let Identifier(name) = &func.name;
    if let Some(body) = &func.body {
        tackify_block(body, &mut instructions, name_generator, &mut temp_types);
    }

    if !matches!(instructions.last(), Some(Instruction::Return(_))) {
        let ret_type = match &func.fun_type {
            Type::FunType { ret, .. } => ret.as_ref(),
            _ => unreachable!("Function must have FunType"),
        };

        let default_return = match ret_type {
            Type::Int => Val::Constant(Const::ConstInt(0)),
            Type::Long => Val::Constant(Const::ConstLong(0)),
            _ => unreachable!("Function return type must be Int or Long"),
        };
        instructions.push(Instruction::Return(default_return));
    }

    FunctionDefinition {
        name: name.clone(),
        params: func.params.clone(),
        body: instructions,
        global: func.global,
        temp_types,
    }
}

/// Converts static variables from the symbol table into TACKY IR definitions.
///
/// Extracts static variables from the symbol table into TACKY IR `StaticVariable` entries.
///
/// - `Initial` → `VarInit::Defined(value)`
/// - `Tentative` → `VarInit::Defined(0)` (zero-initialized)
/// - `NoInitializer` → `VarInit::Extern` (resolved by linker), but only for file-scope
///   names (skips renamed locals like `i.1` which contain a dot)
fn convert_symbols_to_tacky(symbols: &SymbolTable) -> Vec<StaticVariable> {
    let mut tacky_defs = vec![];
    for (name, entry) in symbols.iter() {
        match &entry.symbol_type {
            Type::Int | Type::Long => {
                match &entry.val {
                    InitialValue::Initial(static_val) => tacky_defs.push(StaticVariable {
                        name: name.clone(),
                        global: entry.global,
                        init: VarInit::Defined(*static_val),
                        var_type: entry.symbol_type.clone(),
                    }),
                    InitialValue::Tentative => {
                        let zero = match &entry.symbol_type {
                            Type::Int => StaticInt::IntInit(0),
                            Type::Long => StaticInt::LongInit(0),
                            Type::FunType { .. } => unreachable!("Tentative must be Int or Long"),
                        };
                        tacky_defs.push(StaticVariable {
                            name: name.clone(),
                            global: entry.global,
                            init: VarInit::Defined(zero),
                            var_type: entry.symbol_type.clone(),
                        })
                    },
                    InitialValue::NoInitializer => {
                        // Only add extern vars if it's not a renamed local variable.
                        // Renamed locals have dots in their names (e.g., "i.1").
                        // File-scope extern declarations keep their original names.
                        if !name.contains('.') {
                            tacky_defs.push(StaticVariable {
                                name: name.clone(),
                                global: entry.global,
                                init: VarInit::Extern,
                                var_type: entry.symbol_type.clone(),
                            });
                        }
                    }
                }
            }
            Type::FunType { .. } => {
                // Skip function types - they're handled separately
            }
        }
    }
    tacky_defs
}

/// Converts a parsed C program into TACKY IR.
///
/// Processes all function definitions (skipping declarations without bodies) and
/// combines them with static variable definitions from the symbol table.
pub fn tackify_program(
    program: TypedProgram,
    name_generator: &mut NameGenerator,
    symbols: &SymbolTable,
) -> Program {
    let mut function_defs = Vec::new();
    for decl in &program.declarations {
        if let TypedDeclaration::Function(func) = decl && func.body.is_some() {
            function_defs.push(tackify_function(func, name_generator));
        }
    }

    let static_vars = convert_symbols_to_tacky(symbols);
    Program { function_defs, static_vars }
}
