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
//!   │                   └─ emit_cast()  — scalar conversions (truncate / sign- / zero-extend, int<->double)
//!   └─ convert_symbols_to_tacky()       — extract static vars from symbol table
//! ```

use crate::parser::{AssignOp, BinOp as ParserBinOp, Const, Identifier, IncDec, SwitchIntType, Type, UnaryOp};
use crate::validate::{
    Block, BlockItem, Expr, ForInit, InitialValue, NameGenerator, StaticInit, Stmt, SymbolTable, TypedDeclaration,
    TypedExpression, TypedFunction, TypedProgram, TypedVarDeclaration,
};
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Val {
    Constant(Const),
    Var(Rc<str>),
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

impl From<&ParserBinOp> for BinOp {
    fn from(op: &ParserBinOp) -> Self {
        match op {
            ParserBinOp::Add => BinOp::Add,
            ParserBinOp::Subtract => BinOp::Subtract,
            ParserBinOp::Multiply => BinOp::Multiply,
            ParserBinOp::Divide => BinOp::Divide,
            ParserBinOp::Remainder => BinOp::Remainder,
            ParserBinOp::BitwiseAnd => BinOp::BitwiseAnd,
            ParserBinOp::BitwiseOr => BinOp::BitwiseOr,
            ParserBinOp::BitwiseXOr => BinOp::BitwiseXOr,
            ParserBinOp::BitwiseLeftShift => BinOp::BitwiseLeftShift,
            ParserBinOp::BitwiseRightShift => BinOp::BitwiseRightShift,
            ParserBinOp::Equal => BinOp::Equal,
            ParserBinOp::NotEqual => BinOp::NotEqual,
            ParserBinOp::LessThan => BinOp::LessThan,
            ParserBinOp::LessOrEqual => BinOp::LessOrEqual,
            ParserBinOp::GreaterThan => BinOp::GreaterThan,
            ParserBinOp::GreaterOrEqual => BinOp::GreaterOrEqual,
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

impl From<&AssignOp> for BinOp {
    fn from(op: &AssignOp) -> Self {
        match op {
            AssignOp::Add => BinOp::Add,
            AssignOp::Subtract => BinOp::Subtract,
            AssignOp::Multiply => BinOp::Multiply,
            AssignOp::Divide => BinOp::Divide,
            AssignOp::Remainder => BinOp::Remainder,
            AssignOp::BitwiseAnd => BinOp::BitwiseAnd,
            AssignOp::BitwiseOr => BinOp::BitwiseOr,
            AssignOp::BitwiseXOr => BinOp::BitwiseXOr,
            AssignOp::BitwiseLeftShift => BinOp::BitwiseLeftShift,
            AssignOp::BitwiseRightShift => BinOp::BitwiseRightShift,
        }
    }
}

/// A TACKY three-address instruction.
///
/// TACKY is roughly **LLVM IR without SSA** — a generic three-address code, not a C-specific format
/// (which is why it's the reuse seam for other frontends). The correspondence to LLVM IR:
///
/// | TACKY | LLVM |
/// |-------|------|
/// | `Return` | `ret` |
/// | `SignExtend` / `ZeroExtend` / `Truncate` | `sext` / `zext` / `trunc` |
/// | `IntToDouble` / `UIntToDouble` | `sitofp` / `uitofp` |
/// | `DoubleToInt` / `DoubleToUInt` | `fptosi` / `fptoui` (saturating variant = `llvm.fptosi.sat` / `.fptoui.sat`) |
/// | `Binary` (arithmetic/bitwise/shift) | `add`/`sub`/`mul`/`sdiv`/`udiv`/`srem`/`urem`/`and`/`or`/`xor`/`shl`/`ashr`/`lshr` (or `fadd`/… for `double`) |
/// | `Binary` (comparison) | `icmp`/`fcmp` **+ `zext i1`** (TACKY yields a full int, not `i1`) |
/// | `Unary::Negate` | `sub 0, x` (int) / `fneg` (double) — LLVM has no integer `neg` |
/// | `Unary::BitwiseComplement` | `xor x, -1` — LLVM has no `not` |
/// | `Unary::Not` | `icmp eq x, 0` **+ `zext i1`** |
/// | `Jump` | unconditional `br` |
/// | `JumpIfZero` / `JumpIfNotZero` | `icmp eq/ne x, 0` **+** conditional `br` |
/// | `Label` | a **basic-block boundary** (LLVM has no label *instruction*; blocks are labels) |
/// | `FunCall` | `call` |
/// | `Copy` | **no equivalent** — an artifact of TACKY's mutable temporaries (see SSA note) |
///
/// Two structural differences from LLVM IR, both deliberate (TACKY sits *before* SSA):
/// 1. **Not SSA.** Temporaries are reassignable, which is why `Copy` exists at all — in SSA there is
///    nothing to copy, you reference the value. A `Var` here is closer to an LLVM `alloca` slot.
///    A `TACKY → SSA → TACKY` pass would introduce `phi`s and match LLVM's middle-end form.
/// 2. **Flat list + labels, not explicit basic blocks.** `Label`/`Jump` describe the CFG implicitly;
///    LLVM makes basic blocks first-class (each ends in exactly one terminator).
#[derive(Clone, Debug)]
pub enum Instruction {
    Return(Val),
    SignExtend {
        // LLVM: sext
        src: Val,
        dst: Val,
    },
    Truncate {
        // LLVM: trunc
        src: Val,
        dst: Val,
    },
    ZeroExtend {
        // LLVM: zext
        src: Val,
        dst: Val,
    },
    DoubleToInt {
        // LLVM: fptosi (saturating variant = llvm.fptosi.sat)
        src: Val,
        dst: Val,
    },
    DoubleToUInt {
        // LLVM: fptoui (saturating variant = llvm.fptoui.sat)
        src: Val,
        dst: Val,
    },
    IntToDouble {
        // LLVM: sitofp
        src: Val,
        dst: Val,
    },
    UIntToDouble {
        // LLVM: uitofp
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
    pub name: Rc<str>,
    pub params: Vec<Identifier>,
    pub body: Vec<Instruction>,
    pub global: bool,
    pub temp_types: HashMap<Rc<str>, Type>,
}

/// Storage initialization for static variables.
#[derive(Debug, Clone)]
pub enum VarInit {
    /// Variable defined here with initial value (0 = .bss, non-zero = .data)
    Defined(StaticInit),
    /// Extern variable - no storage allocated, resolved by linker
    Extern,
}

#[derive(Debug, Clone)]
pub struct StaticVariable {
    pub name: Rc<str>,
    pub global: bool,
    pub init: VarInit,
    pub var_type: Type,
}

#[derive(Debug)]
pub struct Program {
    pub function_defs: Vec<FunctionDefinition>,
    pub static_vars: Vec<StaticVariable>,
}

/// Emits the conversion instruction for casting `src` (`src_type`) into `dst` (`dst_type`),
/// dispatching on whether each side is an integer or `double`.
///
/// **Integer → integer** — chosen by width, with extension keyed on the **source**'s signedness
/// (the value being widened), not the destination's:
/// - **same width** → `Copy` (a reinterpret, e.g. `int`<->`uint`; no bits change)
/// - **narrowing** (dst smaller) → `Truncate` (keep the low bits; signedness-independent)
/// - **widening from a signed source** → `SignExtend` (preserves value, e.g. `int -1` -> `long -1`)
/// - **widening from an unsigned source** → `ZeroExtend` (preserves value, e.g. `uint` -> `long`)
///
/// Keying on the source is what makes mixed-sign widenings correct: `(unsigned long)(-1)`
/// sign-extends to `ULONG_MAX`, while `(long)4294967295u` zero-extends to `4294967295`.
///
/// **`double` → integer** → `DoubleToInt` (signed target) / `DoubleToUInt` (unsigned target).
/// **Integer → `double`** → `IntToDouble` (signed source) / `UIntToDouble` (unsigned source).
///
/// `double` → `double` cannot reach here: a same-type cast is short-circuited by the caller (the
/// `Expr::Cast` arm of `tackify_expr`) before `emit_cast` runs, so that combination is `unreachable!`.
fn emit_cast(src: Val, dst: Val, src_type: &Type, dst_type: &Type, instructions: &mut Vec<Instruction>) {
    match (src_type.is_integer(), dst_type.is_integer()) {
        (true, true) => {
            if src_type.size_bits() == dst_type.size_bits() {
                instructions.push(Instruction::Copy { src, dst });
            } else if dst_type.size_bits() < src_type.size_bits() {
                instructions.push(Instruction::Truncate { src, dst });
            } else if src_type.is_signed() {
                instructions.push(Instruction::SignExtend { src, dst });
            } else {
                instructions.push(Instruction::ZeroExtend { src, dst });
            }
        }
        (false, true) => {
            if dst_type.is_signed() {
                instructions.push(Instruction::DoubleToInt { src, dst })
            } else {
                instructions.push(Instruction::DoubleToUInt { src, dst })
            }
        }
        (true, false) => {
            if src_type.is_signed() {
                instructions.push(Instruction::IntToDouble { src, dst })
            } else {
                instructions.push(Instruction::UIntToDouble { src, dst })
            }
        }
        _ => unreachable!("Double to double no cast"),
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
fn tackify_expr(
    e: &TypedExpression,
    instructions: &mut Vec<Instruction>,
    name_generator: &mut NameGenerator,
    temp_types: &mut HashMap<Rc<str>, Type>,
) -> Val {
    let e_type = e.exp_type.clone();
    match &e.exp {
        Expr::Const(c) => Val::Constant(*c),
        Expr::Cast(target_type, inner) => {
            let source_type = inner.exp_type.clone();
            let result = tackify_expr(inner, instructions, name_generator, temp_types);
            if *target_type == source_type {
                return result;
            }
            let dst_name = name_generator.next("temp");
            temp_types.insert(dst_name.clone(), target_type.clone());
            let dst = Val::Var(dst_name);
            emit_cast(result, dst.clone(), &source_type, target_type, instructions);
            dst
        }
        Expr::Unary(op, inner) => {
            let src = tackify_expr(inner, instructions, name_generator, temp_types);
            let dst_name = name_generator.next("temp");
            temp_types.insert(dst_name.clone(), e.exp_type.clone());
            let dst = Val::Var(dst_name);
            instructions.push(Instruction::Unary {
                op: *op,
                src,
                dst: dst.clone(),
            });
            dst
        }
        Expr::Binary(ParserBinOp::And, e1, e2) => {
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
            temp_types.insert(result_name.clone(), e.exp_type.clone());
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
        Expr::Binary(ParserBinOp::Or, e1, e2) => {
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
            temp_types.insert(result_name.clone(), e.exp_type.clone());
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
                temp_types.insert(temp_name.clone(), e1.exp_type.clone());
                let temp = Val::Var(temp_name);
                instructions.push(Instruction::Copy {
                    src: src1.clone(),
                    dst: temp.clone(),
                });
                src1 = temp;
            }
            let src2 = tackify_expr(e2, instructions, name_generator, temp_types);
            let dst_name = name_generator.next("temp");
            temp_types.insert(dst_name.clone(), e.exp_type.clone());
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
        },
        Expr::CompoundAssignment(op, lhs, rhs, op_type) => match &lhs.exp {
            Expr::Var(Identifier(name)) => {
                let current_val = Val::Var(name.clone());
                let rhs_val = tackify_expr(rhs, instructions, name_generator, temp_types);

                // Promote RHS to op_type if needed
                let src2 = if *op_type != rhs.exp_type {
                    let cast_name = name_generator.next("cast_tmp");
                    temp_types.insert(cast_name.clone(), op_type.clone());
                    let cast_dst = Val::Var(cast_name);
                    emit_cast(rhs_val, cast_dst.clone(), &rhs.exp_type, op_type, instructions);
                    cast_dst
                } else {
                    rhs_val
                };

                // Promote LHS to op_type if needed
                let src1 = if *op_type != lhs.exp_type {
                    let cast_name = name_generator.next("cast_tmp");
                    temp_types.insert(cast_name.clone(), op_type.clone());
                    let cast_dst = Val::Var(cast_name);
                    emit_cast(
                        current_val.clone(),
                        cast_dst.clone(),
                        &lhs.exp_type,
                        op_type,
                        instructions,
                    );
                    cast_dst
                } else {
                    current_val.clone()
                };

                // Perform operation; use temp if result needs truncation
                if *op_type != lhs.exp_type {
                    let dst_name = name_generator.next("compound_temp");
                    temp_types.insert(dst_name.clone(), op_type.clone());
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
                temp_types.insert(org_tmp_name.clone(), e.exp_type.clone());
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
            temp_types.insert(result_name.clone(), e.exp_type.clone());
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
                    temp_types.insert(temp_name.clone(), param.exp_type.clone());
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
            temp_types.insert(dst_name.clone(), e.exp_type.clone());
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

fn tackify_stmt(
    stmt: &Stmt,
    instructions: &mut Vec<Instruction>,
    name_generator: &mut NameGenerator,
    temp_types: &mut HashMap<Rc<str>, Type>,
) {
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
            instructions.push(Instruction::Label(label_name.clone()));
            tackify_stmt(stmt, instructions, name_generator, temp_types);
        }
        Stmt::Goto(label_name) => instructions.push(Instruction::Jump {
            target: label_name.clone(),
        }),
        Stmt::Compound(block) => tackify_block(block, instructions, name_generator, temp_types),
        Stmt::Break(target) | Stmt::Continue(target) => instructions.push(Instruction::Jump { target: target.clone() }),
        Stmt::While(condition, body, label) => {
            let continue_label = Identifier(Rc::from(format!("continue_loop.{label}")));
            let break_label = Identifier(Rc::from(format!("break_loop.{label}")));

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
            let start_label = Identifier(Rc::from(format!("start_loop.{label}")));
            let continue_label = Identifier(Rc::from(format!("continue_loop.{label}")));
            let break_label = Identifier(Rc::from(format!("break_loop.{label}")));

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
            match init.as_ref() {
                ForInit::InitDecl(dec) => tackify_var_declaration(dec, instructions, name_generator, temp_types),
                ForInit::InitExp(exp) => {
                    if let Some(e) = exp {
                        tackify_expr(e, instructions, name_generator, temp_types);
                    }
                }
            }

            let start_label = Identifier(Rc::from(format!("start_loop.{label}")));
            let continue_label = Identifier(Rc::from(format!("continue_loop.{label}")));
            let break_label = Identifier(Rc::from(format!("break_loop.{label}")));

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
            let end_label = Identifier(Rc::from(format!("break_switch.{switch_num}")));
            let switch_val = tackify_expr(exp, instructions, name_generator, temp_types);

            // Build conditional jumps for each case
            // this can be optimized with binary search and/or jump tables
            let mut default_label = None;
            for case in cases {
                match case {
                    SwitchIntType::Int(_)
                    | SwitchIntType::Long(_)
                    | SwitchIntType::UInt(_)
                    | SwitchIntType::ULong(_) => {
                        let case_label = Identifier(Rc::from(case.label_str(*switch_num)));
                        let case_const = match case {
                            SwitchIntType::Int(c) => Const::ConstInt(*c),
                            SwitchIntType::Long(c) => Const::ConstLong(*c),
                            SwitchIntType::UInt(c) => Const::ConstUInt(*c),
                            SwitchIntType::ULong(c) => Const::ConstULong(*c),
                            SwitchIntType::Default => unreachable!(),
                        };
                        let cond_name = name_generator.next("case_cond");
                        temp_types.insert(cond_name.clone(), Type::Int);
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
                        default_label = Some(Identifier(Rc::from(case.label_str(*switch_num))));
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

fn tackify_block(
    block: &Block,
    instructions: &mut Vec<Instruction>,
    name_generator: &mut NameGenerator,
    temp_types: &mut HashMap<Rc<str>, Type>,
) {
    for item in block {
        match item {
            BlockItem::Statement(stmt) => tackify_stmt(stmt, instructions, name_generator, temp_types),
            BlockItem::Declaration(dec) => match dec {
                TypedDeclaration::Variable(var) => {
                    tackify_var_declaration(var, instructions, name_generator, temp_types)
                }
                TypedDeclaration::Function(_) => {} // Function declarations in blocks have no body
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
    temp_types: &mut HashMap<Rc<str>, Type>,
) {
    if declaration.storage_class.is_none()
        && let Some(init_exp) = &declaration.init
    {
        let res = tackify_expr(init_exp, instructions, name_generator, temp_types);
        instructions.push(Instruction::Copy {
            src: res,
            dst: Val::Var(declaration.name.0.clone()),
        });
    }
}

/// Lowers a typed function into its TACKY [`FunctionDefinition`]: tackifies the body into a flat
/// instruction list and collects the types of the temporaries generated along the way.
///
/// # Synthetic return
///
/// If the body doesn't already end in a `Return`, one is appended — for **every** function, not just
/// `main`. Codegen emits the terminating `ret` only when it lowers an [`Instruction::Return`], so a
/// function with no trailing return would otherwise fall through into the next function's code.
///
/// The return *value* is only meaningful for `main`, where C99 §5.1.2.2.3 mandates an implicit
/// `return 0`. For any other function, reaching `}` and using the result is undefined behavior
/// (§6.9.1p12): the standard defines no default, and gcc/clang leave the return register as-is
/// (garbage). NCC instead returns a deterministic `0`/`0.0` of the return type, consistent with its
/// policy of making UB deterministic; the value is unobservable in defined programs.
fn tackify_function(func: &TypedFunction, name_generator: &mut NameGenerator) -> FunctionDefinition {
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
            Type::ULong => Val::Constant(Const::ConstULong(0)),
            Type::UInt => Val::Constant(Const::ConstUInt(0)),
            Type::Double => Val::Constant(Const::ConstDouble(0.0)),
            Type::FunType { .. } => unreachable!("function return type cannot be a function type"),
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
            Type::Int | Type::Long | Type::UInt | Type::ULong | Type::Double => {
                match &entry.val {
                    InitialValue::Initial(static_val) => tacky_defs.push(StaticVariable {
                        name: name.clone(),
                        global: entry.global,
                        init: VarInit::Defined(*static_val),
                        var_type: entry.symbol_type.clone(),
                    }),
                    InitialValue::Tentative => {
                        let zero = match &entry.symbol_type {
                            Type::Int => StaticInit::IntInit(0),
                            Type::Long => StaticInit::LongInit(0),
                            Type::UInt => StaticInit::UIntInit(0),
                            Type::ULong => StaticInit::ULongInit(0),
                            Type::Double => StaticInit::DoubleInit(0.0),
                            Type::FunType { .. } => unreachable!("a function type cannot have a tentative definition"),
                        };
                        tacky_defs.push(StaticVariable {
                            name: name.clone(),
                            global: entry.global,
                            init: VarInit::Defined(zero),
                            var_type: entry.symbol_type.clone(),
                        })
                    }
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
pub fn tackify_program(program: TypedProgram, name_generator: &mut NameGenerator, symbols: &SymbolTable) -> Program {
    let mut function_defs = Vec::new();
    for decl in &program.declarations {
        if let TypedDeclaration::Function(func) = decl
            && func.body.is_some()
        {
            function_defs.push(tackify_function(func, name_generator));
        }
    }

    let static_vars = convert_symbols_to_tacky(symbols);
    Program {
        function_defs,
        static_vars,
    }
}
