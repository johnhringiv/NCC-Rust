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
//!    violate x86-64 encoding rules (e.g. memory-to-memory moves) using scratch
//!    registers R10/R11/CX, and inserts the stack allocation prologue
//! 4. **Label coalescing** ([`coalesce_labels`]) — merges consecutive labels to reduce
//!    redundant jump targets
//!
//! ## What This Pass Accomplishes
//!
//! - Translates all TACKY operations to concrete x86-64 instructions
//! - Implements the System V AMD64 calling convention:
//!   - Arguments 1-6 in RDI, RSI, RDX, RCX, R8, R9; remainder on stack
//!   - 16-byte stack alignment before `call`
//!   - Return value in RAX
//! - Allocates stack frames: locals grow downward from RBP, 16-byte aligned
//! - Produces a [`Program`] of [`FunctionDefinition`]s and [`StaticVariable`]s
//! - Produces a [`BackendSymbolTable`] mapping names to assembly types
//!
//! ## Call Order
//!
//! ```text
//! generate()                              — public entry point, orchestrates all 4 passes
//!   ├─ build_backend_symbol_table()       — map all vars/fns to assembly types
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
use crate::tacky::{BinOp, Val, VarInit, StaticVariable as TackyStaticVariable};
use std::collections::{HashMap, HashSet};
use crate::validate::SymbolTable;

#[derive(Clone, Debug, PartialEq)]
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
    SP
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssemblyType {
    Longword,
    Quadword
}

impl AssemblyType {
    pub fn size(&self) -> u64 {
        match self {
            AssemblyType::Longword => 4,
            AssemblyType::Quadword => 8,
        }
    }
}

impl From<&Type> for AssemblyType {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int => AssemblyType::Longword,
            Type::Long => AssemblyType::Quadword,
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
    pub name: String,
    pub global: bool,
    pub alignment: u64,
    pub init: VarInit,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Imm(i64),
    Reg(Reg),
    Pseudo(String),
    Stack(i32),
    Data(String),
}

impl Operand {
    pub fn is_memory(&self) -> bool {
        matches!(self, Operand::Data(_) | Operand::Stack(_))
    }
}

impl From<&Val> for Operand {
    fn from(val: &Val) -> Self {
        match val {
            Val::Constant(Const::ConstInt(i)) => Operand::Imm(*i as i64),
            Val::Constant(Const::ConstLong(l)) => Operand::Imm(*l),
            Val::Var(s) => Operand::Pseudo(s.clone()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    BitAnd,
    BitOr,
    BitXOr,
    BitShl,
    BitSar,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand, size: AssemblyType },
    Movsx { src: Operand, dst: Operand },
    Unary { op: UnaryOp, dst: Operand, size: AssemblyType },
    Binary { op: BinaryOp, src: Operand, dst: Operand, size: AssemblyType },
    Cmp { v1: Operand, v2: Operand, size: AssemblyType },
    Idiv(Operand, AssemblyType),
    Cdq(AssemblyType),
    Jmp(Identifier),
    JmpCC { code: CondCode, label: Identifier },
    SetCC { code: CondCode, op: Operand },
    Label(Identifier),
    Push(Operand),
    Call(Identifier),
    Ret,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CondCode {
    E,
    NE,
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
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
    pub static_vars: Vec<StaticVariable>,
}

fn get_assembly_type(
    val: &Val,
    symbols: &BackendSymbolTable
) -> AssemblyType {
    match val {
        Val::Constant(Const::ConstInt(_)) => AssemblyType::Longword,
        Val::Constant(Const::ConstLong(_)) => AssemblyType::Quadword,
        Val::Var(name) => {
            match symbols.get(name).expect("Variable should be in backend symbol table") {
                AsmSymbolEntry::Obj { asm_type, .. } => *asm_type,
                AsmSymbolEntry::Fun { .. } => unreachable!("Cannot use function as value"),
            }
        }
    }
}

/// Emits instructions for a function call following the System V AMD64 ABI.
///
/// First 6 integer arguments go in registers (RDI, RSI, RDX, RCX, R8, R9).
/// Remaining arguments are pushed onto the stack in reverse order.
/// Stack is padded to maintain 16-byte alignment before the call.
/// The return value is moved from RAX to the destination.
fn convert_function_call(fun_name: &Identifier, args: &[Val], dst: &Val, symbols: &BackendSymbolTable) -> Vec<Instruction> {
    let arg_registers = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
    let (register_args, stack_args) = args.split_at(args.len().min(6));
    let mut instructions = Vec::new();
    let stack_padding = if stack_args.len() % 2 != 0 {
        instructions.push(Instruction::Binary {
                    op: BinaryOp::Sub,
                    src: Operand::Imm(8),
                    dst: Operand::Reg(Reg::SP),
                    size: AssemblyType::Quadword
                });
        8
    } else {
        0
    };
    for (tacky_arg, reg) in register_args.iter().zip(arg_registers) {
        instructions.push(Instruction::Mov {
            src: tacky_arg.into(),
            dst: Operand::Reg(reg),
            size: get_assembly_type(tacky_arg, symbols)
        });
    }
    for tacky_arg in stack_args.iter().rev() {
        let assembly_arg = tacky_arg.into();
        let asm_type = get_assembly_type(tacky_arg, symbols);
        if asm_type == AssemblyType::Quadword || matches!(assembly_arg, Operand::Imm(_) | Operand::Reg(_)) {
            instructions.push(Instruction::Push(assembly_arg));
        } else {
            instructions.push(Instruction::Mov {
                src: assembly_arg,
                dst: Operand::Reg(Reg::AX),
                size: AssemblyType::Longword,
            });
            instructions.push(Instruction::Push(Operand::Reg(Reg::AX)))
        }
    }
    instructions.push(Instruction::Call(fun_name.clone()));

    let bytes_to_remove = 8 * stack_args.len() as i64 + stack_padding;
    if bytes_to_remove > 0 {
        instructions.push(Instruction::Binary {op: BinaryOp::Add, src: Operand::Imm(bytes_to_remove), dst: Operand::Reg(Reg::SP), size: AssemblyType::Quadword});
    }
    instructions.push(Instruction::Mov {
        src: Operand::Reg(Reg::AX),
        dst: dst.into(),
        size: get_assembly_type(dst, symbols)
    });

    instructions
}

fn convert_instruction(instruction: &tacky::Instruction, symbols: &BackendSymbolTable) -> Vec<Instruction> {
    match instruction {
        tacky::Instruction::Return(x) => {
            let size = get_assembly_type(x, symbols);
            vec![
                Instruction::Mov {
                    src: x.into(),
                    dst: Operand::Reg(Reg::AX),
                    size,
                },
                Instruction::Ret,
            ]
        }
        tacky::Instruction::Unary {
            op: parser::UnaryOp::Not,
            src,
            dst,
        } => {
            let size = get_assembly_type(src, symbols);
            vec![
                Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: src.into(),
                    size,
                },
                Instruction::Mov {
                    src: Operand::Imm(0),
                    dst: dst.into(),
                    size,
                },
                Instruction::SetCC {
                    code: CondCode::E,
                    op: dst.into(),
                },
            ]
        }
        tacky::Instruction::Unary { op, src, dst } => {
            let size = get_assembly_type(src, symbols);
            let op = convert_unary_op(op);
            vec![
                Instruction::Mov { src: src.into(), dst: dst.into(), size },
                Instruction::Unary { op, dst: dst.into(), size },
            ]
        }
        tacky::Instruction::Binary { op, src1, src2, dst } => {
            let size = get_assembly_type(src1, symbols);
            match op {
                BinOp::Add
                | BinOp::Subtract
                | BinOp::Multiply
                | BinOp::BitwiseAnd
                | BinOp::BitwiseOr
                | BinOp::BitwiseXOr
                | BinOp::BitwiseLeftShift
                | BinOp::BitwiseRightShift => {
                    vec![
                        Instruction::Mov {
                            src: src1.into(),
                            dst: dst.into(),
                            size
                        },
                        Instruction::Binary {
                            op: BinaryOp::from(op),
                            src: src2.into(),
                            dst: dst.into(),
                            size
                        },
                    ]
                }
                BinOp::Divide | BinOp::Remainder => {
                    let result_reg = if *op == BinOp::Divide { Reg::AX } else { Reg::DX };
                    vec![
                        Instruction::Mov {
                            src: src1.into(),
                            dst: Operand::Reg(Reg::AX),
                            size
                        },
                        Instruction::Cdq(size),
                        Instruction::Idiv(src2.into(), size),
                        Instruction::Mov {
                            src: Operand::Reg(result_reg),
                            dst: dst.into(),
                            size
                        },
                    ]
                }
                BinOp::Equal
                | BinOp::NotEqual
                | BinOp::LessThan
                | BinOp::LessOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterOrEqual => {
                    let code = match op {
                        BinOp::Equal => CondCode::E,
                        BinOp::NotEqual => CondCode::NE,
                        BinOp::LessThan => CondCode::L,
                        BinOp::LessOrEqual => CondCode::LE,
                        BinOp::GreaterThan => CondCode::G,
                        BinOp::GreaterOrEqual => CondCode::GE,
                        _ => unreachable!(),
                    };
                    vec![
                        Instruction::Cmp { v1: src2.into(), v2: src1.into(), size },
                        Instruction::Mov {
                            src: Operand::Imm(0),
                            dst: dst.into(),
                            size
                        },
                        Instruction::SetCC { code, op: dst.into() },
                    ]
                }
            }
        }
        tacky::Instruction::JumpIfZero { condition, target } => {
            let size = get_assembly_type(condition, symbols);
            vec![
                Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: condition.into(),
                    size
                },
                Instruction::JmpCC {
                    code: CondCode::E,
                    label: target.clone(),
                },
            ]
        }
        tacky::Instruction::JumpIfNotZero { condition, target } => {
            let size = get_assembly_type(condition, symbols);
            vec![
                Instruction::Cmp {
                    v1: Operand::Imm(0),
                    v2: condition.into(),
                    size
                },
                Instruction::JmpCC {
                    code: CondCode::NE,
                    label: target.clone(),
                },
            ]
        }
        tacky::Instruction::Jump { target } => {
            vec![Instruction::Jmp(target.clone())]
        }
        tacky::Instruction::Label(label) => {
            vec![Instruction::Label(label.clone())]
        }
        tacky::Instruction::Copy { src, dst } => {
            let size = get_assembly_type(src, symbols);
            vec![Instruction::Mov { src: src.into(), dst: dst.into(), size }]
        }
        tacky::Instruction::FunCall { fun_name, args, dst } => convert_function_call(fun_name, args, dst, symbols),
        tacky::Instruction::SignExtend { src, dst } => {
            vec![Instruction::Movsx {src: src.into(), dst: dst.into()}]
        }
        tacky::Instruction::Truncate { src, dst } => {
            vec![Instruction::Mov {src: src.into(), dst: dst.into(), size: AssemblyType::Longword}]
        }
    }
}

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
            BinOp::BitwiseRightShift => BinaryOp::BitSar,
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

/// Converts a TACKY function definition to x86-64 assembly instructions.
///
/// Emits parameter moves (from registers/stack to pseudo-registers) followed
/// by the converted body instructions. First 6 params come from registers,
/// the rest from stack positions above the saved RBP and return address.
//todo should this consume FunctionDefination
fn convert_function(ast: &tacky::FunctionDefinition, symbols: &BackendSymbolTable) -> FunctionDefinition {
    let tacky::FunctionDefinition {
        name,
        params,
        body,
        global,
        temp_types
    } = ast;
    let arg_registers = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
    let mut instructions = vec![];

    for (Identifier(param), reg) in params.iter().zip(arg_registers) {
        let param_ty = symbols.get_obj_type(param);

        instructions.push(Instruction::Mov {
            src: Operand::Reg(reg),
            dst: Operand::Pseudo(param.clone()),
            size: *param_ty
        });
    }

    for (i, Identifier(param)) in params.iter().skip(6).enumerate() {
        let stack_offset = 16 + (i as i32 * 8); // +16 for saved RBP and return address
        let param_ty = symbols.get_obj_type(param);
        instructions.push(Instruction::Mov {
            src: Operand::Stack(stack_offset),
            dst: Operand::Pseudo(param.clone()),
            size: *param_ty
        });
    }

    instructions.extend(body.iter().flat_map(|ins| {
        convert_instruction(ins, symbols)
    }  ));
    {
        FunctionDefinition {
            name: name.to_string(),
            body: instructions,
            global: *global,
        }
    }
}

/// Backend symbol table entry mapping identifiers to their assembly-level properties.
///
/// Tracks whether a symbol is an object (variable) or function, along with
/// the assembly type (Longword/Quadword) needed for instruction sizing.
pub enum AsmSymbolEntry {
    Obj {
        asm_type: AssemblyType,
        is_static: bool
    },
    Fun {
        defined: bool
    },
}

pub type BackendSymbolTable = HashMap<String, AsmSymbolEntry>;

pub trait BackendSymbolTableExt {
    fn get_obj_type(&self, name: &str) -> &AssemblyType;
}

impl BackendSymbolTableExt for BackendSymbolTable {
    fn get_obj_type(&self, name: &str) -> &AssemblyType {
        match self.get(name).unwrap() {
            AsmSymbolEntry::Obj { asm_type, .. } => asm_type,
            AsmSymbolEntry::Fun { .. } => panic!("Expected object type, found function: {}", name),
        }
    }
}

/// Builds the backend symbol table from the frontend symbol table and TACKY IR.
///
/// Maps all symbols to their assembly types: frontend symbols (variables and functions)
/// from the validator's symbol table, plus TACKY temporaries from each function definition.
fn build_backend_symbol_table(ast: &tacky::Program, symbols: &SymbolTable) -> BackendSymbolTable {
    let mut backend = BackendSymbolTable::new();
    let static_names: HashSet<&str> = ast.static_vars.iter()
        .map(|sv| sv.name.as_str())
        .collect();

    for (name, symbol) in symbols.iter() {
        let backend_entry = match &symbol.symbol_type {
            Type::FunType { defined, .. } => AsmSymbolEntry::Fun {
                defined: *defined,
            },
            ty => AsmSymbolEntry::Obj {
                    asm_type: ty.into(),
                    is_static: static_names.contains(name.as_str()),
            }
        };
        backend.insert(name.clone(), backend_entry);
    }

    for func in &ast.function_defs {
        for (temp_name, temp_type) in &func.temp_types {
            backend.insert(temp_name.clone(), AsmSymbolEntry::Obj {
                asm_type: temp_type.into(),
                is_static: false,
            });
        }
    }

    backend
}

fn convert_static_var(static_var:TackyStaticVariable) -> StaticVariable {
    let TackyStaticVariable{ name, global, init, var_type } = static_var;
    StaticVariable {
        name,
        global,
        alignment:  AssemblyType::from(&var_type).size(),
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
pub fn generate(ast: tacky::Program, symbols: &SymbolTable) -> (Program, BackendSymbolTable) {
    let backend_symbol_table = build_backend_symbol_table(&ast, symbols);
    let functions = ast.function_defs.iter().map(|f| {convert_function(f, &backend_symbol_table)}).collect();
    let static_vars = ast.static_vars.into_iter().map(convert_static_var).collect();

    let mut p = Program { functions, static_vars };
    let stack_offsets = replace_pseudo_registers(&mut p, &backend_symbol_table);
    fix_invalid(&mut p, &stack_offsets);
    coalesce_labels(&mut p);
    (p, backend_symbol_table)
}

struct StackMapping<'a> {
    stack_mapping: HashMap<String, i32>,
    offset: i32,
    /// Names of variables that should use Data operands (RIP-relative addressing).
    /// Includes both static variables defined in this file and extern variables.
    data_vars: &'a HashSet<String>,
}

impl<'a> StackMapping<'a> {
    fn from_data_vars(data_vars: &'a HashSet<String>) -> Self {
        StackMapping {
            stack_mapping: HashMap::new(),
            offset: 0,
            data_vars,
        }
    }

    /// Returns the stack operand for a pseudo-register, allocating a new slot if needed.
    ///
    /// Stack grows downward: each new allocation decrements the offset by the type's size.
    fn get_stack_location(&mut self, pseudo: &str, asm_type: AssemblyType ) -> Operand {
        let offset_option = self.stack_mapping.get(pseudo);
        let offset = match offset_option {
            Some(offset) => offset,
            None => {
                self.offset -= asm_type.size() as i32;
                self.stack_mapping.insert(pseudo.to_string(), self.offset);
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
fn replace_pseudo_registers(program: &mut Program, symbols: &BackendSymbolTable) -> HashMap<String, i32> {
    // Collect data vars (static + extern) upfront to avoid borrow conflict in the loop
    let data_vars: HashSet<String> = program.static_vars.iter().map(|v| v.name.clone()).collect();

    let mut offsets = HashMap::new();
    for FunctionDefinition { name, body, global: _ } in program.functions.iter_mut() {
        let mut stack_mapping = StackMapping::from_data_vars(&data_vars);

        for ins in body.iter_mut() {
            match ins {
                Instruction::Mov { src, dst, size } | Instruction::Binary { op: _, src, dst, size } => {
                    *src = stack_mapping.replace_pseudo(src, *size);
                    *dst = stack_mapping.replace_pseudo(dst, *size);
                }
                Instruction::Unary { op: _, dst, size } => *dst = stack_mapping.replace_pseudo(dst, *size),
                Instruction::Movsx { src, dst } => {
                    *src = stack_mapping.replace_pseudo(src, AssemblyType::Longword);
                    *dst = stack_mapping.replace_pseudo(dst, AssemblyType::Quadword);
                }
                Instruction::Idiv(src, size) => {
                    *src = stack_mapping.replace_pseudo(src, *size);
                }
                Instruction::Cmp { v1, v2, size } => {
                    *v1 = stack_mapping.replace_pseudo(v1, *size);
                    *v2 = stack_mapping.replace_pseudo(v2, *size);
                }
                Instruction::SetCC{ op, ..} => {
                    if let Operand::Pseudo(name) = op {
                        let size = *symbols.get_obj_type(name);
                        *op = stack_mapping.replace_pseudo(op, size);
                    }
                }
                Instruction::Push(op) => {
                    *op = stack_mapping.replace_pseudo(op, AssemblyType::Quadword)
                }
                _ => {}
            }
        }
        offsets.insert(name.to_string(), stack_mapping.offset);
    }
    offsets
}

/// Fixes invalid x86-64 instruction operand combinations.
///
/// Rewrites instructions that violate x86-64 encoding rules:
/// - Memory-to-memory moves (uses R10 as intermediate)
/// - Immediate operand to idiv (moves to R10 first)
/// - imul with memory destination (uses R11 as intermediate)
/// - Binary ops with both operands in memory (uses R10)
/// - Shift with memory source (moves count to CX)
/// - Compare with immediate as destination operand (v2), or both operands in memory, or large immediates
/// - Large immediates that don't fit in i32 for quadword operations
/// - Movsx with immediate source or pseudo-register destination
///
/// Also inserts stack allocation at the start of each function.
fn fix_invalid(program: &mut Program, stack_offsets: &HashMap<String, i32>) {
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
                size: AssemblyType::Quadword
            })
        }
        for ins in body.iter() {
            match ins {
                Instruction::Mov { src, dst, size } if src.is_memory() && dst.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Reg::R10),
                        size: *size,
                    });
                    new_ins.push(Instruction::Mov {
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
                        size: *size,
                    });
                }
                Instruction::Mov { src: Operand::Imm(val), dst, size: AssemblyType::Longword } if *val < i32::MIN as i64 || *val > i32::MAX as i64 => { // handle case where quadwords imm are being moved into longword. Avoids linker warnings
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(*val as i32 as i64),
                        dst: dst.clone(),
                        size: AssemblyType::Longword,
                    });
                }
                Instruction::Mov { // iced caught this missing case, can't mov imm quadword to memory
                    src: Operand::Imm(val),
                    dst: dst @ (Operand::Stack(_) | Operand::Data(_)),
                    size: AssemblyType::Quadword,
                } if *val < i32::MIN as i64 || *val > i32::MAX as i64 => {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(*val),
                        dst: Operand::Reg(Reg::R10),
                        size: AssemblyType::Quadword,
                    });
                    new_ins.push(Instruction::Mov {
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
                        size: AssemblyType::Quadword,
                    });
                }
                Instruction::Idiv(Operand::Imm(c), size) => {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(*c),
                        dst: Operand::Reg(Reg::R10),
                        size: *size
                    });
                    new_ins.push(Instruction::Idiv(Operand::Reg(Reg::R10), *size));
                }
                Instruction::Binary {
                    op: BinaryOp::Mult,
                    src,
                    dst,
                    size
                } if dst.is_memory() => {
                    let new_src = if matches!(src, Operand::Imm(val) if *val < i32::MIN as i64 || *val > i32::MAX as i64) {
                        new_ins.push(Instruction::Mov {
                            src: src.clone(),
                            dst: Operand::Reg(Reg::R10),
                            size: AssemblyType::Quadword
                        });
                        Operand::Reg(Reg::R10)
                    } else {
                        src.clone()
                    };

                    new_ins.push(Instruction::Mov {
                        src: dst.clone(),
                        dst: Operand::Reg(Reg::R11),
                        size: *size
                    });
                    new_ins.push(Instruction::Binary {
                        op: BinaryOp::Mult,
                        src: new_src.clone(),
                        dst: Operand::Reg(Reg::R11),
                        size: *size
                    });
                    new_ins.push(Instruction::Mov {
                        src: Operand::Reg(Reg::R11),
                        dst: dst.clone(),
                        size: *size,
                    });
                }
                Instruction::Binary {
                    op: op @ (BinaryOp::Add | BinaryOp::Sub | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXOr),
                    src,
                    dst,
                    size
                } if src.is_memory() && dst.is_memory() => {
                    new_ins.push(Instruction::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Reg::R10),
                        size: *size
                    });
                    new_ins.push(Instruction::Binary {
                        op: op.clone(),
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
                        size: *size
                    });
                }
                Instruction::Binary {
                    op: op @ (BinaryOp::BitShl | BinaryOp::BitSar),
                    src,
                    dst,
                    size
                } if src.is_memory() || matches!(src, Operand::Imm(val) if *val > 255 || *val < 0) => {
                    new_ins.push(Instruction::Mov {
                        src: src.clone(),
                        dst: Operand::Reg(Reg::CX),
                        size: *size
                    });
                    new_ins.push(Instruction::Binary {
                        op: op.clone(),
                        src: Operand::Reg(Reg::CX),
                        dst: dst.clone(),
                        size: *size
                    });
                }
                Instruction::Binary {
                    op: op @ (BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mult | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXOr),
                    src: Operand::Imm(val),
                    dst,
                    size: AssemblyType::Quadword
                } if *val < i32::MIN as i64 || *val > i32::MAX as i64 => {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(*val),
                        dst: Operand::Reg(Reg::R10),
                        size: AssemblyType::Quadword,
                    });
                    new_ins.push(Instruction::Binary {
                        op: op.clone(),
                        src: Operand::Reg(Reg::R10),
                        dst: dst.clone(),
                        size: AssemblyType::Quadword,
                    })
                }
                Instruction::Cmp {v1, v2, size} => {
                    let new_v1 = if (v1.is_memory() && v2.is_memory())
                        || matches!((v1, size), (Operand::Imm(c), AssemblyType::Quadword) if *c < i32::MIN as i64 || *c > i32::MAX as i64) {
                        new_ins.push(Instruction::Mov {
                            src: v1.clone(),
                            dst: Operand::Reg(Reg::R10),
                            size: *size,
                        });
                        Operand::Reg(Reg::R10)
                    } else { v1.clone() };

                    let new_v2 = if matches!(v2, Operand::Imm(_)) {
                        new_ins.push(Instruction::Mov {
                            src: v2.clone(),
                            dst: Operand::Reg(Reg::R11),
                            size: *size,
                        });
                        Operand::Reg(Reg::R11)
                    } else { v2.clone() };

                    new_ins.push(Instruction::Cmp {
                        v1: new_v1,
                        v2: new_v2,
                        size: *size,
                    });
                }
                Instruction::Push(Operand::Imm(c)) if *c < i32::MIN as i64 || *c > i32::MAX as i64 => {
                    new_ins.push(Instruction::Mov {
                        src: Operand::Imm(*c),
                        dst: Operand::Reg(Reg::R10),
                        size: AssemblyType::Quadword,
                    });
                    new_ins.push(Instruction::Push(Operand::Reg(Reg::R10)));
                }
                Instruction::Movsx {src, dst} => { //extends longword src to quadword dst
                    if let &Operand::Imm(val) = src {
                        new_ins.push(Instruction::Mov { // move src to r10
                            src: Operand::Imm(val),
                            dst: Operand::Reg(Reg::R10),
                            size: AssemblyType::Longword,
                        });
                        if dst.is_memory() { // src and dst are invalid
                            new_ins.push(Instruction::Movsx { src: Operand::Reg(Reg::R10), dst: Operand::Reg(Reg::R11)  });
                            new_ins.push(Instruction::Mov {
                                src: Operand::Reg(Reg::R11),
                                dst: dst.clone(),
                                size: AssemblyType::Quadword,
                            })
                        } else  { // just src is invalid
                            new_ins.push(Instruction::Movsx {src: Operand::Reg(Reg::R10), dst: dst.clone() });
                        }
                    } else if dst.is_memory() { // just dst is invalid, put result in r10 then mov to dst
                        new_ins.push(Instruction::Movsx {
                            src: src.clone(),
                            dst: Operand::Reg(Reg::R11)
                        });
                        new_ins.push(Instruction::Mov {src: Operand::Reg(Reg::R11), dst: dst.clone(), size: AssemblyType::Quadword });
                    } else { // valid
                        new_ins.push(ins.clone());
                    }
                }
                _ => new_ins.push(ins.clone()),
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
        let mut label_map: std::collections::HashMap<String, String> = std::collections::HashMap::new();
        let mut new_ins = Vec::new();
        let mut current_label: Option<String> = None;
        // First pass: build label mapping
        for ins in body.iter() {
            match ins {
                Instruction::Label(Identifier(label)) => {
                    if let Some(first_label) = &current_label {
                        // Map this label to the first label in the sequence
                        // Do not push to new_ins
                        label_map.insert(label.clone(), first_label.clone());
                    } else {
                        // This is the first label in a potential sequence
                        current_label = Some(label.clone());
                        new_ins.push(ins.clone());
                    }
                }
                _ => {
                    // Non-label instruction, reset the sequence
                    current_label = None;
                    new_ins.push(ins.clone());
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
