//! # Codegen — TACKY IR to x86-64 Assembly AST
//!
//! Lowers TACKY three-address code into an x86-64 assembly AST through a four-step
//! pipeline, producing valid instruction sequences ready for machine code emission.
//!
//! ## Technical Approach
//!
//! The lowering runs four sequential passes, each refining the representation:
//!
//! 1. **Instruction selection** ([`convert_instruction`]) — pattern-matches each TACKY
//!    instruction into one or more assembly instructions using pseudo-registers
//! 2. **Pseudo-register replacement** ([`replace_pseudo_registers`]) — assigns stack
//!    slots (negative RBP offsets) to locals, and RIP-relative [`Operand::Data`]
//!    references to static/extern variables
//! 3. **Instruction fix-up** ([`fix_invalid`]) — rewrites operand combinations that
//!    violate x86-64 encoding rules (e.g. memory-to-memory moves) using GP scratch
//!    registers R10/R11/CX (and XMM14/XMM15 for SSE/`double` cases), and inserts the
//!    stack allocation prologue
//! 4. **Label coalescing** ([`coalesce_labels`]) — merges consecutive labels to reduce
//!    redundant jump targets
//!
//! ## What This Pass Accomplishes
//!
//! - Translates all TACKY operations to concrete x86-64 instructions
//! - Implements the System V AMD64 calling convention:
//!   - Integer args in RDI, RSI, RDX, RCX, R8, R9 (up to 6); `double` args in XMM0–XMM7 (up to 8);
//!     remainder on the stack
//!   - 16-byte stack alignment before `call`
//!   - Return value in RAX (integer) or XMM0 (`double`)
//! - Allocates stack frames: locals grow downward from RBP, 16-byte aligned
//! - Produces a [`Program`] of [`FunctionDefinition`]s, [`StaticVariable`]s, and
//!   [`StaticConstant`]s (the `.rodata` `double` pool)
//! - Produces a [`BackendSymbolTable`] mapping names to their types (size, signedness, `double`)
//!
//! ## Call Order
//!
//! ```text
//! generate()                              — public entry point, orchestrates all 4 passes
//!   ├─ build_backend_symbol_table()       — map all vars/fns to their types
//!   ├─ convert_function()                 — per function: lower params + body
//!   │    └─ convert_instruction()         — per instruction: TACKY -> assembly
//!   │         └─ convert_function_call()  — System V ABI argument passing
//!   ├─ convert_static_var()               — per static variable
//!   ├─ replace_pseudo_registers()         — assign stack slots / data operands
//!   ├─ fix_invalid()                      — rewrite illegal operand combos, add prologue
//!   └─ coalesce_labels()                  — merge consecutive labels
//! ```
//!
//! ## Stack Frame Layout
//!
//! ```text
//!         ┌──────────────────┐  higher addresses
//!         │ 8th+ arg (caller)│  RBP + 24, +32, ...
//!         │ 7th arg (caller) │  RBP + 16
//!         │ return address   │  RBP + 8
//!         │ saved RBP        │  RBP + 0  <── RBP
//!         │ local var (int)  │  RBP - 4
//!         │ local var (long) │  RBP - 12
//!         │ ...              │  ... grows downward
//!         │ (16-byte aligned)│  <── RSP
//!         └──────────────────┘  lower addresses
//! ```

use crate::parser;
use crate::parser::{Const, Identifier, Type};
use crate::tacky;
use crate::tacky::{BinOp, StaticVariable as TackyStaticVariable, Val, VarInit};
use crate::validate::{NameGenerator, StaticInit, SymbolTable};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    SP,
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssemblyType {
    Longword,
    Quadword,
    Double,
}

impl AssemblyType {
    pub fn size(&self) -> u64 {
        match self {
            AssemblyType::Longword => 4,
            AssemblyType::Quadword | AssemblyType::Double => 8,
        }
    }
}

impl From<&Type> for AssemblyType {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int | Type::UInt => AssemblyType::Longword,
            Type::Long | Type::ULong => AssemblyType::Quadword,
            Type::Double => AssemblyType::Double,
            Type::FunType { .. } => {
                panic!("Cannot convert function type to assembly type")
            }
        }
    }
}

impl From<Type> for AssemblyType {
    fn from(ty: Type) -> Self {
        AssemblyType::from(&ty)
    }
}

#[derive(Debug)]
pub struct StaticVariable {
    pub name: Rc<str>,
    pub global: bool,
    pub alignment: u64,
    pub init: VarInit,
}
#[derive(Debug)]
pub(crate) struct StaticConstant {
    pub name: Rc<str>,
    pub alignment: u64,
    pub init: StaticInit,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(i64),
    Reg(Reg),
    Pseudo(Rc<str>),
    Stack(i32),
    Data(Rc<str>),
}

impl Operand {
    pub fn is_memory(&self) -> bool {
        matches!(self, Operand::Data(_) | Operand::Stack(_))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Shr,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    DivDouble,
    BitAnd,
    BitOr,
    BitXOr,
    BitShl,
    BitSar,
    BitShr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Mov {
        src: Operand,
        dst: Operand,
        size: AssemblyType,
    },
    Movsx {
        src: Operand,
        dst: Operand,
    },
    MovZeroExtend {
        src: Operand,
        dst: Operand,
    },
    Cvttsd2si {
        src: Operand,
        dst: Operand,
        size: AssemblyType,
    },
    Cvtsi2sd {
        src: Operand,
        dst: Operand,
        size: AssemblyType,
    },
    Unary {
        op: UnaryOp,
        dst: Operand,
        size: AssemblyType,
    },
    Binary {
        op: BinaryOp,
        src: Operand,
        dst: Operand,
        size: AssemblyType,
    },
    Cmp {
        v1: Operand,
        v2: Operand,
        size: AssemblyType,
    },
    Idiv(Operand, AssemblyType),
    Div(Operand, AssemblyType),
    Cdq(AssemblyType),
    Jmp(Identifier),
    JmpCC {
        code: CondCode,
        label: Identifier,
    },
    SetCC {
        code: CondCode,
        op: Operand,
    },
    Label(Identifier),
    Push(Operand),
    Call(Identifier),
    Ret,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CondCode {
    E,
    NE,
    A,
    AE,
    B,
    BE,
    G,
    GE,
    L,
    LE,
}

impl CondCode {
    pub fn ins_suffix(&self) -> &'static str {
        match self {
            CondCode::E => "e",
            CondCode::NE => "ne",
            CondCode::G => "g",
            CondCode::GE => "ge",
            CondCode::L => "l",
            CondCode::LE => "le",
            CondCode::B => "b",
            CondCode::BE => "be",
            CondCode::A => "a",
            CondCode::AE => "ae",
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Rc<str>,
    pub body: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug)]
pub(crate) struct Program {
    pub functions: Vec<FunctionDefinition>,
    pub static_vars: Vec<StaticVariable>,
    pub static_constants: Vec<StaticConstant>,
}

/// Emits instructions for a function call following the System V AMD64 ABI.
///
/// Arguments are classified by [`classify_operands`]: integer args fill RDI/RSI/RDX/RCX/R8/R9
/// (up to 6), `double` args fill XMM0–XMM7 (up to 8), and the overflow of either kind is pushed
/// onto the stack in reverse order. The stack is padded with an extra 8 bytes when there is an odd
/// number of stack args, keeping it 16-byte aligned at the `call`; the padding plus the pushed
/// args are reclaimed afterward. The return value is read from XMM0 for a `double` result and from
/// RAX otherwise.
fn convert_function_call(
    fun_name: &Identifier,
    args: &[Val],
    dst: &Val,
    symbols: &BackendSymbolTable,
    constants: &mut ConstantPool,
) -> Vec<Instruction> {
    let int_regs = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
    let double_regs = [ Reg::XMM0, Reg::XMM1, Reg::XMM2, Reg::XMM3, Reg::XMM4, Reg::XMM5, Reg::XMM6, Reg::XMM7 ];
    let operands: Vec<(AssemblyType, Operand)> = args
        .iter()
        .map(|v| (symbols.get_assembly_type(v), val_operand(v, constants, 8)))
        .collect();

    let (int_reg_args, double_reg_args, stack_args) = classify_operands(&operands);

    let mut instructions = vec![];

    let stack_padding = if stack_args.len() % 2 != 0 {
        instructions.push(Instruction::Binary {
            op: BinaryOp::Sub,
            src: Operand::Imm(8),
            dst: Operand::Reg(Reg::SP),
            size: AssemblyType::Quadword,
        });
        8
    } else {
        0
    };

    for ((param_ty, dst), reg) in int_reg_args.iter().zip(int_regs).chain(double_reg_args.iter().zip(double_regs)) {
        instructions.push(Instruction::Mov {
            src: dst.clone(),
            dst: Operand::Reg(reg),
            size: *param_ty,
        });
    }

    for (param_ty, dst) in stack_args.iter().rev() {
        // 8-byte operands (Quadword/Double) and Reg/Imm operands can be pushed directly; a
        // narrower memory operand is first widened through AX so the full 8-byte slot is defined.
        if matches!(dst, Operand::Reg(_) | Operand::Imm(_)) || matches!(param_ty, AssemblyType::Quadword | AssemblyType::Double) {
            instructions.push(Instruction::Push(dst.clone()))
        } else {
            instructions.push(Instruction::Mov {src: dst.clone(), dst: Operand::Reg(Reg::AX), size: *param_ty});
            instructions.push(Instruction::Push(Operand::Reg(Reg::AX)))
        }
    }

    instructions.push(Instruction::Call(fun_name.clone()));

    let bytes_to_remove = 8 * stack_args.len() as i64 + stack_padding;
    if bytes_to_remove > 0 {
        instructions.push(Instruction::Binary {
            op: BinaryOp::Add,
            src: Operand::Imm(bytes_to_remove),
            dst: Operand::Reg(Reg::SP),
            size: AssemblyType::Quadword,
        });
    }

    let assembly_dst = val_operand(dst, constants, 8);
    let return_ty = symbols.get_assembly_type(dst);

    if matches!(return_ty, AssemblyType::Double) {
        instructions.push(Instruction::Mov {src: Operand::Reg(Reg::XMM0), dst: assembly_dst, size: AssemblyType::Double})
    } else {
        instructions.push(Instruction::Mov {src: Operand::Reg(Reg::AX), dst: assembly_dst, size: return_ty})
    }

    instructions
}

struct ConstantPool {
    map: HashMap<u64, (Rc<str>, u64)>, // f64 bits -> (.rodata label, alignment)
}

impl ConstantPool {
    fn intern(&mut self, value: f64, alignment: u64) -> Rc<str> {
        let next = self.map.len();
        let entry = self
            .map
            .entry(value.to_bits())
            .or_insert_with(|| (Rc::from(format!("double.{next}")), alignment));
        entry.1 = entry.1.max(alignment);
        entry.0.clone()
    }

    fn new() -> ConstantPool {
        ConstantPool { map: HashMap::new() }
    }

    /// Drains the interned constants into `StaticConstant`s for `.rodata` emission.
    ///
    /// Sorted by bit pattern so the emitted constant section is deterministic (`HashMap` iteration
    /// order is randomized). Each constant carries the strictest alignment any use requested: 8 for a
    /// plain `movsd` load, 16 for the `xorpd` sign-flip mask (see [`ConstantPool::intern`]).
    fn into_static_constants(self) -> Vec<StaticConstant> {
        let mut entries: Vec<(u64, (Rc<str>, u64))> = self.map.into_iter().collect();
        entries.sort_by_key(|(bits, _)| *bits);
        entries
            .into_iter()
            .map(|(bits, (name, alignment))| StaticConstant {
                name,
                alignment,
                init: StaticInit::DoubleInit(f64::from_bits(bits)),
            })
            .collect()
    }
}

/// Converts a TACKY [`Val`] to an assembly [`Operand`].
///
/// Integer constants become `Imm`; variables become `Pseudo` (resolved to a stack slot or
/// `Data` reference later). A double constant has no immediate form on x86-64, so it is interned
/// into `constants` and referenced as `Operand::Data` in `.rodata`.
///
/// `alignment` is forwarded to [`ConstantPool::intern`] and applies *only* to double constants
/// (it's ignored for every other variant). Pass `8` for an ordinary `movsd` load; pass `16` for
/// the `xorpd` sign-flip mask, which requires 16-byte alignment.
fn val_operand(val: &Val, constants: &mut ConstantPool, alignment: u64) -> Operand {
    match val {
        Val::Constant(Const::ConstDouble(d)) => Operand::Data(constants.intern(*d, alignment)),
        Val::Constant(Const::ConstInt(i)) => Operand::Imm(*i as i64),
        Val::Constant(Const::ConstLong(l)) => Operand::Imm(*l),
        Val::Var(s) => Operand::Pseudo(s.clone()),
        Val::Constant(Const::ConstUInt(i)) => Operand::Imm(*i as i64),
        Val::Constant(Const::ConstULong(l)) => Operand::Imm(*l as i64),
    }
}

/// Lowers a single TACKY instruction to one or more x86-64 assembly instructions
/// (instruction selection, pass 1), still operating on pseudo-registers.
///
/// Most ops map straightforwardly; the type-dependent ones consult `symbols` for operand
/// size ([`BackendSymbolTable::get_assembly_type`]), signedness ([`BackendSymbolTable::is_signed`]),
/// and whether the operand is a `double` ([`BackendSymbolTable::is_double`]):
/// - **Return** — value in RAX, or XMM0 for a `double`.
/// - **Divide / Remainder** — `idiv` (signed) vs `div` (unsigned); the dividend is set up with
///   `cdq`/`cqo` (signed) or a zeroed RDX (unsigned). Result taken from RAX (quotient) or RDX
///   (remainder). `double` division is the unrelated SSE `divsd` (`BinaryOp::DivDouble`).
/// - **Right shift** — `sar` (signed, arithmetic) vs `shr` (unsigned, logical).
/// - **Comparisons** — signed (`L`/`G`/…) vs unsigned (`B`/`A`/…) condition codes via `SetCC`.
///   `double` comparisons use `comisd` and the *unsigned* codes (it sets ZF/CF/PF like an
///   unsigned compare). NaN ordering is not yet handled — see the `nan` TODOs.
/// - **`double` arithmetic** — `addsd`/`subsd`/`mulsd`/`divsd` on XMM registers (same `BinaryOp`s
///   as the integer forms, selected by `AssemblyType::Double`); negation is `xorpd` with a
///   16-byte-aligned `-0.0` sign mask; logical `!`/zero-tests compare against a zeroed XMM via
///   `comisd`.
/// - **Integer conversions** — `SignExtend` → `Movsx`, `ZeroExtend` → `MovZeroExtend`,
///   `Truncate` → `Mov`.
/// - **`double` ↔ integer conversions** — `cvtsi2sd`/`cvttsd2si` for the signed cases; the
///   unsigned cases need SSE2 workarounds (no native unsigned convert before AVX-512). See the
///   per-arm comments on `DoubleToUInt` / `UIntToDouble` for the algorithms.
fn convert_instruction(
    instruction: &tacky::Instruction,
    symbols: &BackendSymbolTable,
    constants: &mut ConstantPool,
    name_gen: &mut NameGenerator,
) -> Vec<Instruction> {
    match instruction {
        tacky::Instruction::Return(x) => {
            let size = symbols.get_assembly_type(x);
            let ret_reg = if matches!(size, AssemblyType::Double) { Reg::XMM0} else {Reg::AX};
            vec![
                Instruction::Mov {
                    src: val_operand(x, constants, 8),
                    dst: Operand::Reg(ret_reg),
                    size,
                },
                Instruction::Ret,
            ]
        }
        tacky::Instruction::Unary {
            op: parser::UnaryOp::Not,
            src,
            dst,
        } if symbols.is_double(src) => {
            vec![
                Instruction::Binary {
                    op: BinaryOp::BitXOr,
                    src: Operand::Reg(Reg::XMM0),
                    dst: Operand::Reg(Reg::XMM0),
                    size: AssemblyType::Double,
                },
                Instruction::Cmp {
                    v1: val_operand(src, constants, 8),
                    v2: Operand::Reg(Reg::XMM0),
                    size: AssemblyType::Double,
                },
                Instruction::Mov {
                    src: Operand::Imm(0),
                    dst: val_operand(dst, constants, 8),
                    size: symbols.get_assembly_type(dst),
                },
                Instruction::SetCC {
                    code: CondCode::E,
                    op: val_operand(dst, constants, 8),
                },
            ]
        }
        tacky::Instruction::Unary {
            op: parser::UnaryOp::Not,
            src,
            dst,
        } => {
            let size = symbols.get_assembly_type(src);
            vec![
                Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: val_operand(src, constants, 8),
                    size,
                },
                Instruction::Mov {
                    src: Operand::Imm(0),
                    dst: val_operand(dst, constants, 8),
                    size,
                },
                Instruction::SetCC {
                    code: CondCode::E,
                    op: val_operand(dst, constants, 8),
                },
            ]
        }
        tacky::Instruction::Unary {
            op: parser::UnaryOp::Negate,
            src,
            dst,
        } if symbols.is_double(src) => {
            let sign_mask = Val::Constant(Const::ConstDouble(-0.0));
            vec![
                Instruction::Mov {
                    src: val_operand(src, constants, 8),
                    dst: val_operand(dst, constants, 8),
                    size: AssemblyType::Double,
                },
                Instruction::Binary {
                    op: BinaryOp::BitXOr,
                    src: val_operand(&sign_mask, constants, 16),
                    dst: val_operand(dst, constants, 8),
                    size: AssemblyType::Double,
                },
            ]
        }
        tacky::Instruction::Unary { op, src, dst } => {
            let size = symbols.get_assembly_type(src);
            let op = convert_unary_op(op);
            vec![
                Instruction::Mov {
                    src: val_operand(src, constants, 8),
                    dst: val_operand(dst, constants, 8),
                    size,
                },
                Instruction::Unary {
                    op,
                    dst: val_operand(dst, constants, 8),
                    size,
                },
            ]
        }
        tacky::Instruction::Binary { op, src1, src2, dst } => {
            let size = symbols.get_assembly_type(src1);
            match op {
                BinOp::Add
                | BinOp::Subtract
                | BinOp::Multiply
                | BinOp::BitwiseAnd
                | BinOp::BitwiseOr
                | BinOp::BitwiseXOr
                | BinOp::BitwiseLeftShift
                | BinOp::BitwiseRightShift => {
                    let asm_op = match op {
                        BinOp::BitwiseRightShift => {
                            if symbols.is_signed(src1) {
                                BinaryOp::BitSar
                            } else {
                                BinaryOp::BitShr
                            }
                        }
                        _ => BinaryOp::from(op), // BitSar / everything else
                    };
                    vec![
                        Instruction::Mov {
                            src: val_operand(src1, constants, 8),
                            dst: val_operand(dst, constants, 8),
                            size,
                        },
                        Instruction::Binary {
                            op: asm_op,
                            src: val_operand(src2, constants, 8),
                            dst: val_operand(dst, constants, 8),
                            size,
                        },
                    ]
                }
                BinOp::Divide if symbols.is_double(src1) => {
                    vec![
                        Instruction::Mov {
                            src: val_operand(src1, constants, 8),
                            dst: val_operand(dst, constants, 8),
                            size,
                        },
                        Instruction::Binary {
                            op: BinaryOp::DivDouble,
                            src: val_operand(src2, constants, 8),
                            dst: val_operand(dst, constants, 8),
                            size,
                        },
                    ]
                }
                BinOp::Divide | BinOp::Remainder => {
                    let result_reg = if *op == BinOp::Divide { Reg::AX } else { Reg::DX };
                    let signed = symbols.is_signed(dst);
                    let mut ins = vec![Instruction::Mov {
                        src: val_operand(src1, constants, 8),
                        dst: Operand::Reg(Reg::AX),
                        size,
                    }];
                    if signed {
                        ins.push(Instruction::Cdq(size));
                        ins.push(Instruction::Idiv(val_operand(src2, constants, 8), size));
                    } else {
                        ins.push(Instruction::Mov {
                            src: Operand::Imm(0),
                            dst: Operand::Reg(Reg::DX),
                            size,
                        }); // zero-extend
                        ins.push(Instruction::Div(val_operand(src2, constants, 8), size));
                    }
                    ins.push(Instruction::Mov {
                        src: Operand::Reg(result_reg),
                        dst: val_operand(dst, constants, 8),
                        size,
                    });
                    ins
                }
                BinOp::Equal
                | BinOp::NotEqual
                | BinOp::LessThan
                | BinOp::LessOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterOrEqual => {
                    // signedness comes from the operands, not dst (a comparison's result is always int)
                    // todo will revisit when adding nan
                    let signed = !symbols.is_double(src1) && symbols.is_signed(src1);
                    let code = match (op, signed) {
                        (BinOp::Equal, _) => CondCode::E,
                        (BinOp::NotEqual, _) => CondCode::NE,
                        (BinOp::LessThan, true) => CondCode::L,
                        (BinOp::LessThan, false) => CondCode::B,
                        (BinOp::LessOrEqual, true) => CondCode::LE,
                        (BinOp::LessOrEqual, false) => CondCode::BE,
                        (BinOp::GreaterThan, true) => CondCode::G,
                        (BinOp::GreaterThan, false) => CondCode::A,
                        (BinOp::GreaterOrEqual, true) => CondCode::GE,
                        (BinOp::GreaterOrEqual, false) => CondCode::AE,
                        _ => unreachable!(),
                    };
                    vec![
                        Instruction::Cmp {
                            v1: val_operand(src2, constants, 8),
                            v2: val_operand(src1, constants, 8),
                            size,
                        },
                        Instruction::Mov {
                            src: Operand::Imm(0),
                            dst: val_operand(dst, constants, 8),
                            size,
                        },
                        Instruction::SetCC {
                            code,
                            op: val_operand(dst, constants, 8),
                        },
                    ]
                }
            }
        }
        tacky::Instruction::JumpIfZero { condition, target }
        | tacky::Instruction::JumpIfNotZero { condition, target } => {
            let code = if matches!(instruction, tacky::Instruction::JumpIfZero { .. }) {
                CondCode::E
            } else {
                CondCode::NE
            };
            let size = symbols.get_assembly_type(condition);
            let mut cmp_ins = if matches!(size, AssemblyType::Double) {
                // todo revist with nan
                vec![
                    Instruction::Binary {
                        op: BinaryOp::BitXOr,
                        src: Operand::Reg(Reg::XMM0),
                        dst: Operand::Reg(Reg::XMM0),
                        size: AssemblyType::Double,
                    },
                    Instruction::Cmp {
                        v1: val_operand(condition, constants, 8),
                        v2: Operand::Reg(Reg::XMM0),
                        size: AssemblyType::Double,
                    },
                ]
            } else {
                vec![Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: val_operand(condition, constants, 8),
                    size,
                }]
            };
            cmp_ins.push(Instruction::JmpCC {
                code,
                label: target.clone(),
            });
            cmp_ins
        }
        tacky::Instruction::Jump { target } => {
            vec![Instruction::Jmp(target.clone())]
        }
        tacky::Instruction::Label(label) => {
            vec![Instruction::Label(label.clone())]
        }
        tacky::Instruction::Copy { src, dst } => {
            let size = symbols.get_assembly_type(src);
            vec![Instruction::Mov {
                src: val_operand(src, constants, 8),
                dst: val_operand(dst, constants, 8),
                size,
            }]
        }
        tacky::Instruction::FunCall { fun_name, args, dst } => {
            convert_function_call(fun_name, args, dst, symbols, constants)
        }
        tacky::Instruction::SignExtend { src, dst } => {
            vec![Instruction::Movsx {
                src: val_operand(src, constants, 8),
                dst: val_operand(dst, constants, 8),
            }]
        }
        tacky::Instruction::Truncate { src, dst } => {
            vec![Instruction::Mov {
                src: val_operand(src, constants, 8),
                dst: val_operand(dst, constants, 8),
                size: AssemblyType::Longword,
            }]
        }
        tacky::Instruction::ZeroExtend { src, dst } => {
            vec![Instruction::MovZeroExtend {
                src: val_operand(src, constants, 8),
                dst: val_operand(dst, constants, 8),
            }]
        }
        // Signed int -> double: `cvtsi2sd` is native and exact (no rounding mode needed). The
        // `size` is the *source* integer width (32 vs 64-bit), since the double dest is implicit.
        tacky::Instruction::IntToDouble { src, dst } => {
            vec![Instruction::Cvtsi2sd {
                src: val_operand(src, constants, 8),
                dst: val_operand(dst, constants, 8),
                size: symbols.get_assembly_type(src),
            }]
        }
        // double -> signed int: `cvttsd2si` truncates toward zero. The `size` is the *destination*
        // integer width. Out-of-range/NaN yield the "integer indefinite" value (target MIN) — a
        // deliberate, deterministic edge (see the module/README notes on float-cast behavior).
        tacky::Instruction::DoubleToInt { src, dst } => {
            vec![Instruction::Cvttsd2si {
                src: val_operand(src, constants, 8),
                dst: val_operand(dst, constants, 8),
                size: symbols.get_assembly_type(dst),
            }]
        }
        // double -> unsigned int. There is no native unsigned conversion before AVX-512, so we
        // route through the signed `cvttsd2si` and split on the destination width.
        #[rustfmt::skip]
        tacky::Instruction::DoubleToUInt { src: src_raw, dst: dst_raw } => {
            let src = val_operand(src_raw, constants, 8);
            let dst = val_operand(dst_raw, constants, 8);
            if matches!(symbols.get_assembly_type(src_raw), AssemblyType::Longword) {
                // u32: every value in range fits the positive i64 range, so convert to a full
                // 64-bit signed int and keep the low 32 bits.
                vec![
                    Instruction::Cvttsd2si { src, dst: Operand::Reg(Reg::AX), size: AssemblyType::Quadword },
                    Instruction::Mov { src: Operand::Reg(Reg::AX), dst, size: AssemblyType::Longword }
                ]
            } else {
                // u64: `cvttsd2si` only does signed i64, so values in [2^63, 2^64) overflow it.
                // If src >= 2^63, subtract 2^63, convert the now-in-range value, then add 2^63
                // back (as the bit pattern 0x8000_0000_0000_0000 = i64::MIN); otherwise convert
                // directly.
                let long_max_plus_1 = val_operand(&Val::Constant(Const::ConstDouble(9223372036854775808.0)), constants, 8);
                let oor = Identifier(name_gen.next("d2u_oor"));
                let end = Identifier(name_gen.next("d2u_end"));
                vec![
                    Instruction::Cmp { v1: long_max_plus_1.clone(), v2: src.clone(), size: AssemblyType::Double }, // src vs 2^63
                    Instruction::JmpCC { code: CondCode::AE, label: oor.clone() },                                 // src >= 2^63 -> fixup
                    Instruction::Cvttsd2si { src: src.clone(), dst: dst.clone(), size: AssemblyType::Quadword },    // in range: direct
                    Instruction::Jmp(end.clone()),
                    Instruction::Label(oor),                                                                        // src >= 2^63
                    Instruction::Mov { src: src.clone(), dst: Operand::Reg(Reg::XMM0), size: AssemblyType::Double },
                    Instruction::Binary { op: BinaryOp::Sub, src: long_max_plus_1, dst: Operand::Reg(Reg::XMM0), size: AssemblyType::Double }, // src - 2^63
                    Instruction::Cvttsd2si { src: Operand::Reg(Reg::XMM0), dst: dst.clone(), size: AssemblyType::Quadword },                   // now in i64 range
                    Instruction::Mov {src: Operand::Imm(i64::MIN), dst: Operand::Reg(Reg::AX), size: AssemblyType::Quadword },                 // 2^63 bit pattern
                    Instruction::Binary { op: BinaryOp::Add, src: Operand::Reg(Reg::AX), dst, size: AssemblyType::Quadword },                  // add 2^63 back
                    Instruction::Label(end)
                ]
            }
        }
        // unsigned int -> double. `cvtsi2sd` only reads signed integers, so we split on the
        // source width.
        #[rustfmt::skip]
        tacky::Instruction::UIntToDouble { src: src_raw, dst } => {
            let src = val_operand(src_raw, constants, 8);
            if matches!(symbols.get_assembly_type(src_raw), AssemblyType::Longword) {
                // u32: zero-extend to 64 bits (always positive as signed i64), then convert.
                // MovZeroExtend rather than Mov is deliberate — it matters for register allocation in Part III.
                vec![
                    Instruction::MovZeroExtend { src, dst: Operand::Reg(Reg::AX) },
                    Instruction::Cvtsi2sd { src: Operand::Reg(Reg::AX), dst: Operand::Reg(Reg::XMM0), size: AssemblyType::Quadword },
                    Instruction::Mov { src: Operand::Reg(Reg::XMM0), dst: val_operand(dst, constants, 8), size: AssemblyType::Double }
                ]
            } else {
                // u64: if the top bit is set the value reads as negative to `cvtsi2sd`. Halve it
                // (so it fits the i64 range), convert, then double the result. The halving rounds
                // to odd (`>>1` then OR in the low bit) so the single rounding in `cvtsi2sd`
                // matches what a correct u64->double would produce. Top bit clear -> convert directly.
                let oor = Identifier(name_gen.next("u2d_oor"));
                let end = Identifier(name_gen.next("u2d_end"));
                vec![
                    Instruction::Cmp { v1: Operand::Imm(0), v2: src.clone(), size: AssemblyType::Quadword }, // src vs 0 (signed)
                    Instruction::JmpCC { code: CondCode::L, label: oor.clone() },                            // top bit set -> halve
                    Instruction::Cvtsi2sd { src: src.clone(), dst: Operand::Reg(Reg::XMM0), size: AssemblyType::Quadword }, // direct
                    Instruction::Jmp(end.clone()),
                    Instruction::Label(oor),                                                                 // top bit set
                    Instruction::Mov { src, dst: Operand::Reg(Reg::AX), size: AssemblyType::Quadword },
                    Instruction::Mov { src: Operand::Reg(Reg::AX), dst: Operand::Reg(Reg::DX), size: AssemblyType::Quadword },
                    Instruction::Unary { op: UnaryOp::Shr, dst: Operand::Reg(Reg::DX), size: AssemblyType::Quadword },        // DX = src >> 1
                    Instruction::Binary { op: BinaryOp::BitAnd, src: Operand::Imm(1), dst: Operand::Reg(Reg::AX), size: AssemblyType::Quadword }, // AX = src & 1
                    Instruction::Binary { op: BinaryOp::BitOr, src: Operand::Reg(Reg::AX), dst: Operand::Reg(Reg::DX), size: AssemblyType::Quadword },  // round to odd
                    Instruction::Cvtsi2sd { src: Operand::Reg(Reg::DX), dst: Operand::Reg(Reg::XMM0), size: AssemblyType::Quadword },
                    Instruction::Binary { op: BinaryOp::Add, src: Operand::Reg(Reg::XMM0), dst: Operand::Reg(Reg::XMM0), size: AssemblyType::Double }, // *2
                    Instruction::Label(end),
                    Instruction::Mov { src: Operand::Reg(Reg::XMM0), dst: val_operand(dst, constants, 8), size: AssemblyType::Double }
                ]
            }
        }
    }
}

/// Maps an arithmetic/bitwise TACKY `BinOp` to its assembly `BinaryOp`.
///
/// Division/remainder are `unreachable!` here — they need signedness (`idiv`/`div`) plus a
/// dividend setup, so they're handled directly in [`convert_instruction`]. Right shift maps to
/// the signed `BitSar` by default; `convert_instruction` overrides it to `BitShr` for unsigned
/// operands. Comparisons are likewise handled there (they select condition codes, not a `BinaryOp`).
impl From<&BinOp> for BinaryOp {
    fn from(op: &BinOp) -> Self {
        match op {
            BinOp::Add => BinaryOp::Add,
            BinOp::Subtract => BinaryOp::Sub,
            BinOp::Multiply => BinaryOp::Mult,
            BinOp::Divide | BinOp::Remainder => {
                unreachable!("Special case for division and remainder should be handled in the instruction conversion")
            }
            BinOp::BitwiseAnd => BinaryOp::BitAnd,
            BinOp::BitwiseOr => BinaryOp::BitOr,
            BinOp::BitwiseXOr => BinaryOp::BitXOr,
            BinOp::BitwiseLeftShift => BinaryOp::BitShl,
            BinOp::BitwiseRightShift => unreachable!("right shift needs signedness; handled in convert_instruction"),
            BinOp::Equal
            | BinOp::NotEqual
            | BinOp::LessThan
            | BinOp::LessOrEqual
            | BinOp::GreaterThan
            | BinOp::GreaterOrEqual => unreachable!("Removed in convert_instruction"),
        }
    }
}

fn convert_unary_op(op: &parser::UnaryOp) -> UnaryOp {
    match op {
        parser::UnaryOp::Negate => UnaryOp::Neg,
        parser::UnaryOp::BitwiseComplement => UnaryOp::Not,
        _ => unreachable!("Unary operation not implemented: {:?}", op),
    }
}

/// Splits typed operands into the three System V AMD64 argument-passing classes:
/// - **integer register** — `Longword`/`Quadword` operands, first 6 (→ RDI, RSI, RDX, RCX, R8, R9)
/// - **SSE register** — `Double` operands, first 8 (→ XMM0–XMM7)
/// - **stack** — everything that overflows either register limit, in operand order
///
/// The two register limits are independent (6 GP and 8 XMM), matching the ABI. Shared by the
/// callee prologue ([`set_up_parameters`]) and the caller argument setup
/// ([`convert_function_call`]) so both sides necessarily agree on register assignment — the calling
/// convention requires it, and a single classifier makes disagreement unrepresentable. Each side
/// builds the `(AssemblyType, Operand)` pairs from its own source (param pseudos vs argument
/// operands) and emits its own direction of moves.
type ArgClass = Vec<(AssemblyType, Operand)>;

fn classify_operands(operands: &[(AssemblyType, Operand)]) -> (ArgClass, ArgClass, ArgClass) {
    let mut int_reg = vec![];
    let mut double_reg = vec![];
    let mut stack = vec![];

    for (ty, operand) in operands {
        let bucket = match ty {
            AssemblyType::Double if double_reg.len() < 8 => &mut double_reg,
            AssemblyType::Longword | AssemblyType::Quadword if int_reg.len() < 6 => &mut int_reg,
            _ => &mut stack,
        };
        bucket.push((*ty, operand.clone()));
    }
    (int_reg, double_reg, stack)
}

/// Emits the function prologue moves that copy incoming parameters into their pseudo-registers.
///
/// The mirror of [`convert_function_call`]'s argument setup: parameters are classified by
/// [`classify_operands`] into the same register/stack classes, then each is moved *from* its
/// incoming location *into* its pseudo. Integer-register and SSE-register params come from
/// RDI/…/R9 and XMM0–XMM7; stack params are read from positive RBP offsets (`+16` for the first,
/// then `+8` each — above the saved RBP and return address). Both sides share `classify_operands`,
/// so the callee and caller necessarily agree on where each argument lives.
fn set_up_parameters(params: &[Identifier], symbols: &BackendSymbolTable) -> Vec<Instruction> {
    let operands: Vec<(AssemblyType, Operand)> = params
        .iter()
        .map(|Identifier(p)| (symbols.get_obj_type(p), Operand::Pseudo(p.clone())))
        .collect();
    let (int_reg_args, double_reg_args, stack_args) = classify_operands(&operands);
    let int_regs = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
    let double_regs = [Reg::XMM0, Reg::XMM1, Reg::XMM2, Reg::XMM3, Reg::XMM4, Reg::XMM5, Reg::XMM6, Reg::XMM7];
    let mut instructions: Vec<Instruction> = Vec::with_capacity(params.len());

    for ((param_ty, dst), reg) in int_reg_args.iter().zip(int_regs).chain(double_reg_args.iter().zip(double_regs)) {
        instructions.push(Instruction::Mov {
            src: Operand::Reg(reg),
            dst: dst.clone(),
            size: *param_ty,
        });
    }

    for (i, (param_ty, dst)) in stack_args.iter().enumerate() {
        let stack_offset = 16 + (i as i32 * 8); // +16 for saved RBP and return address
        instructions.push(Instruction::Mov {
            src: Operand::Stack(stack_offset),
            dst: dst.clone(),
            size: *param_ty,
        });
    }
    instructions
}

/// Converts a TACKY function definition to x86-64 assembly instructions.
///
/// Emits the parameter prologue ([`set_up_parameters`], which moves incoming arguments from their
/// ABI register/stack locations into pseudo-registers) followed by the converted body instructions.
fn convert_function(
    ast: &tacky::FunctionDefinition,
    symbols: &BackendSymbolTable,
    constants: &mut ConstantPool,
    name_gen: &mut NameGenerator,
) -> FunctionDefinition {
    let tacky::FunctionDefinition {
        name,
        params,
        body,
        global,
        temp_types: _,
    } = ast;
    let mut instructions = set_up_parameters(params, symbols);

    instructions.extend(
        body.iter()
            .flat_map(|ins| convert_instruction(ins, symbols, constants, name_gen)),
    );
    FunctionDefinition {
        name: name.clone(),
        body: instructions,
        global: *global,
    }
}

/// Backend symbol table entry mapping identifiers to their assembly-level properties.
///
/// Tracks whether a symbol is an object (variable) or function. For objects it stores the
/// variable's `Type`, from which both the assembly type (size — Longword/Quadword) and
/// signedness (for `idiv`/`div`, `sar`/`shr`, signed/unsigned condition codes) are derived.
pub enum AsmSymbolEntry {
    // `is_static`/`defined` will pick direct RIP-relative vs GOT-indirect (@GOTPCREL)
    // addressing for a symbol (and enable link-time checks); NCC currently emits direct
    // addressing for all data, so these aren't read yet.
    Obj {
        var_type: Type,
        #[allow(dead_code)]
        is_static: bool,
    },
    Fun {
        #[allow(dead_code)]
        defined: bool,
    },
}

/// Backend symbol table: maps each name to its [`AsmSymbolEntry`], with helpers to look up a
/// variable's `Type` and classify an operand's size/signedness for instruction selection.
pub struct BackendSymbolTable(HashMap<Rc<str>, AsmSymbolEntry>);

impl BackendSymbolTable {
    fn new() -> Self {
        Self(HashMap::new())
    }
    fn insert(&mut self, name: Rc<str>, entry: AsmSymbolEntry) {
        self.0.insert(name, entry);
    }

    /// The declared `Type` of variable `name` (panics if absent or a function).
    fn get_var_type(&self, name: &str) -> &Type {
        match self.0.get(name).expect("Variable not in symbol table") {
            AsmSymbolEntry::Obj { var_type, .. } => var_type,
            AsmSymbolEntry::Fun { .. } => unreachable!("Expected object type, found function: {}", name),
        }
    }

    /// Operand size (`Longword`/`Quadword`) for `val` — constants by their `Const` variant,
    /// variables by their type in the backend symbol table. Drives instruction sizing/suffixes.
    fn get_assembly_type(&self, val: &Val) -> AssemblyType {
        match val {
            Val::Constant(Const::ConstInt(_) | Const::ConstUInt(_)) => AssemblyType::Longword,
            Val::Constant(Const::ConstLong(_) | Const::ConstULong(_)) => AssemblyType::Quadword,
            Val::Var(name) => self.get_var_type(name).into(),
            Val::Constant(Const::ConstDouble(_)) => AssemblyType::Double,
        }
    }

    /// Whether `val` has a signed type — selects signed vs unsigned instructions (`idiv`/`div`,
    /// signed/unsigned condition codes). Constants are classified by their `Const` variant;
    /// variables delegate to [`Type::is_signed`] via the backend symbol table.
    fn is_signed(&self, val: &Val) -> bool {
        match val {
            Val::Constant(Const::ConstInt(_) | Const::ConstLong(_)) => true,
            Val::Constant(Const::ConstUInt(_) | Const::ConstULong(_)) => false,
            Val::Var(name) => self.get_var_type(name).is_signed(),
            Val::Constant(Const::ConstDouble(_)) => unreachable!("helper for integer types"),
        }
    }

    fn is_double(&self, val: &Val) -> bool {
        matches!(self.get_assembly_type(val), AssemblyType::Double)
    }

    /// Assembly type (size) of the named object — `get_var_type` reduced to its `AssemblyType`.
    /// Use when you hold a name (param/variable); use `get_assembly_type` for a [`Val`].
    fn get_obj_type(&self, name: &str) -> AssemblyType {
        self.get_var_type(name).into()
    }
}

/// Builds the backend symbol table from the frontend symbol table and TACKY IR.
///
/// Maps all symbols to their types: frontend symbols (variables and functions) from the
/// validator's symbol table, plus TACKY temporaries from each function definition.
fn build_backend_symbol_table(ast: &tacky::Program, symbols: &SymbolTable) -> BackendSymbolTable {
    let mut backend = BackendSymbolTable::new();
    let static_names: HashSet<&str> = ast.static_vars.iter().map(|sv| &*sv.name).collect();

    for (name, symbol) in symbols.iter() {
        let backend_entry = match &symbol.symbol_type {
            Type::FunType { defined, .. } => AsmSymbolEntry::Fun { defined: *defined },
            ty => AsmSymbolEntry::Obj {
                var_type: ty.clone(),
                is_static: static_names.contains(&**name),
            },
        };
        backend.insert(name.clone(), backend_entry);
    }

    for func in &ast.function_defs {
        for (temp_name, temp_type) in &func.temp_types {
            backend.insert(
                temp_name.clone(),
                AsmSymbolEntry::Obj {
                    var_type: temp_type.clone(),
                    is_static: false,
                },
            );
        }
    }

    backend
}

/// Converts a TACKY static variable to the codegen [`StaticVariable`], deriving its
/// alignment from the variable's type (4 for int/uint, 8 for long/ulong).
fn convert_static_var(static_var: TackyStaticVariable) -> StaticVariable {
    let TackyStaticVariable {
        name,
        global,
        init,
        var_type,
    } = static_var;
    StaticVariable {
        name,
        global,
        alignment: AssemblyType::from(&var_type).size(),
        init,
    }
}

/// Converts TACKY IR to x86-64 assembly AST.
///
/// Performs four passes:
/// 1. Instruction selection: converts TACKY instructions to assembly
/// 2. Pseudo-register replacement: assigns stack slots to locals, Data operands to statics
/// 3. Fix-up: rewrites invalid x86-64 operand combinations (e.g., memory-to-memory)
/// 4. Label coalescing: merges consecutive labels to reduce jump targets
pub fn generate(
    ast: tacky::Program,
    symbols: &SymbolTable,
    name_gen: &mut NameGenerator,
) -> (Program, BackendSymbolTable) {
    let mut constants = ConstantPool::new();
    let backend_symbol_table = build_backend_symbol_table(&ast, symbols);
    let functions = ast
        .function_defs
        .iter()
        .map(|f| convert_function(f, &backend_symbol_table, &mut constants, name_gen))
        .collect();
    let static_vars = ast.static_vars.into_iter().map(convert_static_var).collect();

    let mut p = Program {
        functions,
        static_vars,
        static_constants: constants.into_static_constants(),
    };
    let stack_offsets = replace_pseudo_registers(&mut p, &backend_symbol_table);
    fix_invalid(&mut p, &stack_offsets);
    coalesce_labels(&mut p);
    (p, backend_symbol_table)
}

struct StackMapping<'a> {
    stack_mapping: HashMap<Rc<str>, i32>,
    offset: i32,
    /// Names of variables that should use Data operands (RIP-relative addressing).
    /// Includes both static variables defined in this file and extern variables.
    data_vars: &'a HashSet<Rc<str>>,
}

impl<'a> StackMapping<'a> {
    fn from_data_vars(data_vars: &'a HashSet<Rc<str>>) -> Self {
        StackMapping {
            stack_mapping: HashMap::new(),
            offset: 0,
            data_vars,
        }
    }

    /// Returns the stack operand for a pseudo-register, allocating a new slot if needed.
    ///
    /// Stack grows downward: each new allocation decrements the offset by the type's size.
    fn get_stack_location(&mut self, pseudo: &str, asm_type: AssemblyType) -> Operand {
        let offset_option = self.stack_mapping.get(pseudo);
        let offset = match offset_option {
            Some(offset) => offset,
            None => {
                self.offset -= asm_type.size() as i32;
                self.stack_mapping.insert(Rc::from(pseudo), self.offset);
                &self.offset
            }
        };
        Operand::Stack(*offset)
    }

    /// Replaces pseudo-register operands with their actual locations.
    ///
    /// Static/extern variables become `Data` operands (RIP-relative addressing).
    /// Local variables become `Stack` operands (RBP-relative addressing).
    fn replace_pseudo(&mut self, operand: &Operand, asm_type: AssemblyType) -> Operand {
        match operand {
            Operand::Pseudo(pseudo) => {
                if self.data_vars.contains(pseudo) {
                    Operand::Data(pseudo.clone())
                } else {
                    self.get_stack_location(pseudo, asm_type)
                }
            }
            _ => operand.clone(),
        }
    }
}

/// Replaces all pseudo-register operands with concrete locations.
///
/// For each function, assigns stack slots to local variables and converts
/// static variable references to Data operands. Returns a map of function
/// names to their total stack space used (as negative offsets from RBP).
fn replace_pseudo_registers(program: &mut Program, symbols: &BackendSymbolTable) -> HashMap<Rc<str>, i32> {
    // Collect data vars (static + extern) upfront to avoid borrow conflict in the loop
    let data_vars: HashSet<Rc<str>> = program.static_vars.iter().map(|v| v.name.clone()).collect();

    let mut offsets = HashMap::new();
    for FunctionDefinition { name, body, global: _ } in program.functions.iter_mut() {
        let mut stack_mapping = StackMapping::from_data_vars(&data_vars);

        for ins in body.iter_mut() {
            match ins {
                Instruction::Mov { src, dst, size } | Instruction::Binary { op: _, src, dst, size } => {
                    *src = stack_mapping.replace_pseudo(src, *size);
                    *dst = stack_mapping.replace_pseudo(dst, *size);
                }
                Instruction::Cvttsd2si { src, dst, size } => {
                    *src = stack_mapping.replace_pseudo(src, AssemblyType::Double); // src is double
                    *dst = stack_mapping.replace_pseudo(dst, *size);                // dst is integer
                }
                Instruction::Cvtsi2sd { src, dst, size } => {
                    *src = stack_mapping.replace_pseudo(src, *size);                // src is integer
                    *dst = stack_mapping.replace_pseudo(dst, AssemblyType::Double); // dst is double
                }
                Instruction::Unary { op: _, dst, size } => *dst = stack_mapping.replace_pseudo(dst, *size),
                Instruction::Movsx { src, dst } | Instruction::MovZeroExtend { src, dst } => {
                    *src = stack_mapping.replace_pseudo(src, AssemblyType::Longword);
                    *dst = stack_mapping.replace_pseudo(dst, AssemblyType::Quadword);
                }
                Instruction::Idiv(src, size) | Instruction::Div(src, size) => {
                    *src = stack_mapping.replace_pseudo(src, *size);
                }
                Instruction::Cmp { v1, v2, size } => {
                    *v1 = stack_mapping.replace_pseudo(v1, *size);
                    *v2 = stack_mapping.replace_pseudo(v2, *size);
                }
                Instruction::SetCC { op, .. } => {
                    if let Operand::Pseudo(name) = op {
                        let size = symbols.get_obj_type(name);
                        *op = stack_mapping.replace_pseudo(op, size);
                    }
                }
                Instruction::Push(op) => *op = stack_mapping.replace_pseudo(op, AssemblyType::Quadword),
                Instruction::Ret | Instruction::Call(_) | Instruction::Label(_) | Instruction::JmpCC {..} | Instruction::Jmp(_) | Instruction::Cdq(_)  => {}
            }
        }
        offsets.insert(name.clone(), stack_mapping.offset);
    }
    offsets
}

/// Fixes invalid x86-64 instruction operand combinations.
///
/// Rewrites instructions that violate x86-64 encoding rules. Integer/general cases use the GP
/// scratch registers R10/R11 (and CX for shift counts); SSE cases use the reserved XMM scratch
/// registers XMM14/XMM15:
/// - Memory-to-memory moves (intermediate: R10, or XMM14 for a `double` `movsd`)
/// - Immediate operand to idiv (moves to R10 first)
/// - imul with memory destination (uses R11 as intermediate)
/// - Integer binary ops with both operands in memory (uses R10)
/// - `double` binary ops (`addsd`/`subsd`/`mulsd`/`divsd`/`xorpd`) with a non-register
///   destination — SSE requires an XMM-register destination, so the value is routed through XMM15
/// - Shift with memory source (moves count to CX)
/// - Compare with immediate as destination operand (v2), or both operands in memory, or large
///   immediates; `double` compares (`comisd`) additionally require an XMM-register second operand,
///   routed through XMM15
/// - `cvttsd2si` with a non-register (memory) destination — converts into R11, then stores
/// - `cvtsi2sd` with an immediate source (moved through R10) and/or a non-register destination
///   (converts into XMM15, then stores the `double` back)
/// - Large immediates that don't fit in i32 for quadword operations
/// - Movsx with immediate source or pseudo-register destination
///
/// Also inserts stack allocation at the start of each function.
fn fix_invalid(program: &mut Program, stack_offsets: &HashMap<Rc<str>, i32>) {
    for FunctionDefinition { name, body, global: _ } in program.functions.iter_mut() {
        // stack_offset is negative, so convert to positive, round up to 16, then negate for AllocateStack
        let mut positive_offset = -stack_offsets[name];
        if positive_offset % 16 != 0 {
            positive_offset = ((positive_offset / 16) + 1) * 16;
        }
        let mut new_ins = vec![];
        if positive_offset > 0 {
            new_ins.push(Instruction::Binary {
                op: BinaryOp::Sub,
                src: Operand::Imm(positive_offset as i64),
                dst: Operand::Reg(Reg::SP),
                size: AssemblyType::Quadword,
            })
        }
        for ins in body.drain(..) {
            match ins {
                Instruction::Mov { ref src, ref dst, size } if src.is_memory() && dst.is_memory() => {
                    let scratch = if matches!(size, AssemblyType::Double) {Operand::Reg(Reg::XMM14)} else {Operand::Reg(Reg::R10)};
                    new_ins.push(Instruction::Mov {
                        src: src.clone(),
                        dst: scratch.clone(),
                        size,
                    });
                    new_ins.push(Instruction::Mov {
                        src: scratch,
                        dst: dst.clone(),
                        size,
                    });
                }
                Instruction::Mov {
                    src: Operand::Imm(val),
                    dst,
                    size: AssemblyType::Longword,
                } if val < i32::MIN as i64 || val > i32::MAX as i64 => {
                    // handle case where quadwords imm are being moved into longword. Avoids linker warnings
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(val as i32 as i64),
                        dst,
                        size: AssemblyType::Longword,
                    });
                }
                Instruction::Mov {
                    // iced caught this missing case, can't mov imm quadword to memory
                    src: Operand::Imm(val),
                    ref dst,
                    size: AssemblyType::Quadword,
                } if matches!(dst, Operand::Stack(_) | Operand::Data(_))
                    && (val < i32::MIN as i64 || val > i32::MAX as i64) =>
                {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(val),
                        dst: Operand::Reg(Reg::R10),
                        size: AssemblyType::Quadword,
                    });
                    new_ins.push(Instruction::Mov {
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
                        size: AssemblyType::Quadword,
                    });
                }
                Instruction::MovZeroExtend { ref src, ref dst } => {
                    if dst.is_memory() {
                        new_ins.push(Instruction::Mov {
                            src: src.clone(),
                            dst: Operand::Reg(Reg::R11),
                            size: AssemblyType::Longword,
                        });
                        new_ins.push(Instruction::Mov {
                            src: Operand::Reg(Reg::R11),
                            dst: dst.clone(),
                            size: AssemblyType::Quadword,
                        });
                    } else {
                        new_ins.push(Instruction::Mov {
                            src: src.clone(),
                            dst: dst.clone(),
                            size: AssemblyType::Longword,
                        })
                    }
                }
                Instruction::Idiv(Operand::Imm(c), size) => {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(c),
                        dst: Operand::Reg(Reg::R10),
                        size,
                    });
                    new_ins.push(Instruction::Idiv(Operand::Reg(Reg::R10), size));
                }
                Instruction::Div(Operand::Imm(c), size) => {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(c),
                        dst: Operand::Reg(Reg::R10),
                        size,
                    });
                    new_ins.push(Instruction::Div(Operand::Reg(Reg::R10), size))
                }
                Instruction::Binary { op, ref src, ref dst, size: AssemblyType::Double }
                if !matches!(dst, Operand::Reg(_)) =>
                    {
                        new_ins.push(Instruction::Mov { src: dst.clone(), dst: Operand::Reg(Reg::XMM15), size: AssemblyType::Double });
                        new_ins.push(Instruction::Binary { op, src: src.clone(), dst: Operand::Reg(Reg::XMM15), size: AssemblyType::Double });
                        new_ins.push(Instruction::Mov { src: Operand::Reg(Reg::XMM15), dst: dst.clone(), size: AssemblyType::Double });
                    }
                Instruction::Binary {
                    op: BinaryOp::Mult,
                    ref src,
                    ref dst,
                    size,
                } if dst.is_memory() => {
                    let new_src = if matches!(src, Operand::Imm(val) if *val < i32::MIN as i64 || *val > i32::MAX as i64)
                    {
                        new_ins.push(Instruction::Mov {
                            src: src.clone(),
                            dst: Operand::Reg(Reg::R10),
                            size: AssemblyType::Quadword,
                        });
                        Operand::Reg(Reg::R10)
                    } else {
                        src.clone()
                    };

                    new_ins.push(Instruction::Mov {
                        src: dst.clone(),
                        dst: Operand::Reg(Reg::R11),
                        size,
                    });
                    new_ins.push(Instruction::Binary {
                        op: BinaryOp::Mult,
                        src: new_src,
                        dst: Operand::Reg(Reg::R11),
                        size,
                    });
                    new_ins.push(Instruction::Mov {
                        src: Operand::Reg(Reg::R11),
                        dst: dst.clone(),
                        size,
                    });
                }
                Instruction::Binary {
                    op: op @ (BinaryOp::Add | BinaryOp::Sub | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXOr),
                    src,
                    ref dst,
                    size,
                } if src.is_memory() && dst.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src,
                        dst: Operand::Reg(Reg::R10),
                        size,
                    });
                    new_ins.push(Instruction::Binary {
                        op,
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
                        size,
                    });
                }
                Instruction::Binary {
                    op: op @ (BinaryOp::BitShl | BinaryOp::BitSar | BinaryOp::BitShr),
                    src,
                    dst,
                    size,
                } if src.is_memory() || matches!(src, Operand::Imm(val) if !(0..=255).contains(&val)) => {
                    new_ins.push(Instruction::Mov {
                        src,
                        dst: Operand::Reg(Reg::CX),
                        size,
                    });
                    new_ins.push(Instruction::Binary {
                        op,
                        src: Operand::Reg(Reg::CX),
                        dst,
                        size,
                    });
                }
                Instruction::Binary {
                    op,
                    src: Operand::Imm(val),
                    dst,
                    size: AssemblyType::Quadword,
                } if matches!(
                    op,
                    BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Mult
                        | BinaryOp::BitAnd
                        | BinaryOp::BitOr
                        | BinaryOp::BitXOr
                ) && (val < i32::MIN as i64 || val > i32::MAX as i64) =>
                {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(val),
                        dst: Operand::Reg(Reg::R10),
                        size: AssemblyType::Quadword,
                    });
                    new_ins.push(Instruction::Binary {
                        op,
                        src: Operand::Reg(Reg::R10),
                        dst,
                        size: AssemblyType::Quadword,
                    })
                }
                Instruction::Cmp { ref v1, ref v2, size: AssemblyType::Double } => {
                    if let Operand::Reg(_) = v2 {
                        new_ins.push(ins.clone())
                    } else {
                        new_ins.push(Instruction::Mov {src: v2.clone(), dst: Operand::Reg(Reg::XMM15), size: AssemblyType::Double});
                        new_ins.push(Instruction::Cmp {v1: v1.clone(), v2: Operand::Reg(Reg::XMM15), size: AssemblyType::Double})
                    }
                }
                Instruction::Cmp { ref v1, ref v2, size } => {
                    let new_v1 = if (v1.is_memory() && v2.is_memory())
                        || matches!((v1, size), (Operand::Imm(c), AssemblyType::Quadword) if *c < i32::MIN as i64 || *c > i32::MAX as i64)
                    {
                        new_ins.push(Instruction::Mov {
                            src: v1.clone(),
                            dst: Operand::Reg(Reg::R10),
                            size,
                        });
                        Operand::Reg(Reg::R10)
                    } else {
                        v1.clone()
                    };

                    let new_v2 = if matches!(v2, Operand::Imm(_)) {
                        new_ins.push(Instruction::Mov {
                            src: v2.clone(),
                            dst: Operand::Reg(Reg::R11),
                            size,
                        });
                        Operand::Reg(Reg::R11)
                    } else {
                        v2.clone()
                    };

                    new_ins.push(Instruction::Cmp {
                        v1: new_v1,
                        v2: new_v2,
                        size,
                    });
                }
                Instruction::Push(Operand::Imm(c)) if c < i32::MIN as i64 || c > i32::MAX as i64 => {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(c),
                        dst: Operand::Reg(Reg::R10),
                        size: AssemblyType::Quadword,
                    });
                    new_ins.push(Instruction::Push(Operand::Reg(Reg::R10)));
                }
                Instruction::Cvttsd2si {src, dst, size} if !matches!(dst, Operand::Reg(_)) => {
                    new_ins.push(Instruction::Cvttsd2si { src, dst: Operand::Reg(Reg::R11), size });
                    new_ins.push(Instruction::Mov {src: Operand::Reg(Reg::R11), dst, size})
                }
                Instruction::Cvtsi2sd {src, dst, size} => {
                    let new_src = if let Operand::Imm(_) = src {
                        new_ins.push(Instruction::Mov {src, dst: Operand::Reg(Reg::R10), size});
                        Operand::Reg(Reg::R10)
                    } else { src };

                    if !matches!(dst, Operand::Reg(_)) {
                        new_ins.push(Instruction::Cvtsi2sd {src: new_src, dst: Operand::Reg(Reg::XMM15), size });
                        new_ins.push(Instruction::Mov {src: Operand::Reg(Reg::XMM15), dst, size: AssemblyType::Double})
                    } else {
                        new_ins.push(Instruction::Cvtsi2sd {src: new_src, dst, size})
                    }

                }
                Instruction::Movsx { ref src, ref dst } => {
                    //extends longword src to quadword dst
                    if let &Operand::Imm(val) = src {
                        new_ins.push(Instruction::Mov {
                            // move src to r10
                            src: Operand::Imm(val),
                            dst: Operand::Reg(Reg::R10),
                            size: AssemblyType::Longword,
                        });
                        if dst.is_memory() {
                            // src and dst are invalid
                            new_ins.push(Instruction::Movsx {
                                src: Operand::Reg(Reg::R10),
                                dst: Operand::Reg(Reg::R11),
                            });
                            new_ins.push(Instruction::Mov {
                                src: Operand::Reg(Reg::R11),
                                dst: dst.clone(),
                                size: AssemblyType::Quadword,
                            })
                        } else {
                            // just src is invalid
                            new_ins.push(Instruction::Movsx {
                                src: Operand::Reg(Reg::R10),
                                dst: dst.clone(),
                            });
                        }
                    } else if dst.is_memory() {
                        // just dst is invalid, put result in r10 then mov to dst
                        new_ins.push(Instruction::Movsx {
                            src: src.clone(),
                            dst: Operand::Reg(Reg::R11),
                        });
                        new_ins.push(Instruction::Mov {
                            src: Operand::Reg(Reg::R11),
                            dst: dst.clone(),
                            size: AssemblyType::Quadword,
                        });
                    } else {
                        // valid
                        new_ins.push(ins);
                    }
                }
                _ => new_ins.push(ins),
            }
        }
        *body = new_ins;
    }
}

/// Coalesces consecutive labels by mapping subsequent labels to the first one
fn coalesce_labels(program: &mut Program) {
    for FunctionDefinition {
        name: _name,
        body,
        global: _,
    } in program.functions.iter_mut()
    {
        let mut label_map: std::collections::HashMap<Rc<str>, Rc<str>> = std::collections::HashMap::new();
        let mut new_ins = Vec::new();
        let mut current_label: Option<Rc<str>> = None;
        // First pass: build label mapping
        for ins in body.drain(..) {
            match &ins {
                Instruction::Label(Identifier(label)) => {
                    if let Some(first_label) = &current_label {
                        // Map this label to the first label in the sequence
                        // Do not push to new_ins
                        label_map.insert(label.clone(), first_label.clone());
                    } else {
                        // This is the first label in a potential sequence
                        current_label = Some(label.clone());
                        new_ins.push(ins);
                    }
                }
                _ => {
                    // Non-label instruction, reset the sequence
                    current_label = None;
                    new_ins.push(ins);
                }
            }
        }

        // Second pass: update jump targets
        for ins in new_ins.iter_mut() {
            match ins {
                Instruction::Jmp(Identifier(label)) => {
                    if let Some(new_label) = label_map.get(label) {
                        *label = new_label.clone();
                    }
                }
                Instruction::JmpCC {
                    label: Identifier(label),
                    ..
                } => {
                    if let Some(new_label) = label_map.get(label) {
                        *label = new_label.clone();
                    }
                }
                _ => {}
            }
        }
        *body = new_ins;
    }
}
