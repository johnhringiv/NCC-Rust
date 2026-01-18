use std::fmt;

use colored::Colorize;

use crate::codegen::{
    BinaryOp, CondCode, FunctionDefinition as CodegenFunctionDefinition, Instruction as CodegenInstruction, Operand,
    Program as CodegenProgram, Reg, UnaryOp,
};
use crate::parser::{
    AssignOp, BinOp as ParserBinOp, BlockItem, Declaration, Expr, ForInit, FunDeclaration, Identifier, IncDec,
    Program as ParserProgram, SpannedStmt, Stmt, UnaryOp as ParserUnaryOp, VarDeclaration,
};
use crate::tacky::{
    BinOp, FunctionDefinition as TackyFunctionDefinition, Instruction as TackyInstruction, Program as TackyProgram,
    StaticVariable, TopLevel, Val, VarInit,
};

// =============================================================================
// Color Scheme: vitesse-dark theme (TrueColor 24-bit RGB)
// =============================================================================
//
// This module uses the vitesse-dark color palette to provide rich syntax
// highlighting for the compiler's AST pretty printer across all stages:
// Parser, Tacky IR, and Codegen.
//
// Color Mapping:
//
// 1. TEAL (93, 169, 148) - Instructions & Declarations
//    - Instructions: Mov, Binary, Copy, Jump, Call, Label, Return (Tacky), etc.
//    - Declarations: Program, FunctionDefinition, VarDeclaration, FunDeclaration
//    - Expression types: PostFix, Assignment, Conditional, Unary, Binary (Parser)
//
// 2. TEAL_GREEN (77, 147, 117) - Control Flow & Registers
//    - Control flow keywords: Return, If, While, For, Break, Continue, Switch, etc.
//    - CPU Registers: AX, R10, SP, BP, etc.
//
// 3. MUTED_RED (203, 118, 118) - Operators & Type Qualifiers
//    - Operators: Add, Multiply, NotEqual, BitwiseAnd, Increment, etc.
//    - Operand type keywords: Imm, Stack, Data, Pseudo
//    - Condition codes: E, NE, L, LE, G, GE, etc.
//
// 4. YELLOW_GREEN (128, 166, 101) - Names & Labels
//    - Function names and identifiers
//    - Jump target labels (.Lstart_loop, .Lbreak, etc.)
//    - Data labels (global variable names)
//
// 5. CREAM (219, 215, 202) - Variables
//    - Local variable names
//    - Temporary variables (temp.1, binary_left.2, etc.)
//    - Pseudo register names
//
// 6. CYAN_TEAL (76, 154, 145) - Numeric Values
//    - Integer literals
//    - Immediate values
//    - Stack offsets (-4, -8, etc.)
//    - Memory offsets and allocation sizes
//
// 7. YELLOW_GOLD (230, 204, 119) - String Constants
//    - Constant expressions in Parser AST
//    - String literal values
//
// =============================================================================

const TEAL_GREEN: (u8, u8, u8) = (77, 147, 117);    // s5: #4d9375
const TEAL: (u8, u8, u8) = (93, 169, 148);          // s12: #5da994
const MUTED_RED: (u8, u8, u8) = (203, 118, 118);    // s6: #cb7676
const YELLOW_GREEN: (u8, u8, u8) = (128, 166, 101); // s3: #80a665
const CREAM: (u8, u8, u8) = (219, 215, 202);        // s4: #dbd7caee
const CYAN_TEAL: (u8, u8, u8) = (76, 154, 145);     // s18: #4c9a91
const YELLOW_GOLD: (u8, u8, u8) = (230, 204, 119);  // s17: #e6cc77

#[derive(Clone)]
pub struct Node {
    pub text: String,
    pub children: Vec<Node>,
}

impl Node {
    pub fn leaf<T: Into<String>>(text: T) -> Self {
        Node {
            text: text.into(),
            children: Vec::new(),
        }
    }

    pub fn branch<T: Into<String>>(text: T, children: Vec<Node>) -> Self {
        Node {
            text: text.into(),
            children,
        }
    }

    fn fmt_impl(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool, root: bool) -> fmt::Result {
        if root {
            writeln!(f, "{}", self.text)?;
        } else {
            write!(f, "{}{} ", prefix, if last { "└──" } else { "├──" })?;
            writeln!(f, "{}", self.text)?;
        }
        let new_prefix = if root {
            String::new()
        } else if last {
            format!("{prefix}    ")
        } else {
            format!("{prefix}│   ")
        };
        for (i, child) in self.children.iter().enumerate() {
            child.fmt_impl(f, &new_prefix, i + 1 == self.children.len(), false)?;
        }
        Ok(())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_impl(f, "", true, true)
    }
}

pub trait ItfDisplay {
    fn itf_node(&self) -> Node;
    fn itf_string(&self) -> String {
        self.itf_node().to_string()
    }
}

macro_rules! colored_node {
    ($r:expr, $g:expr, $b:expr; $($ty:ty),* $(,)?) => {
        $(
            impl ItfDisplay for $ty {
                fn itf_node(&self) -> Node {
                    Node::leaf(format!("{:?}", self).truecolor($r, $g, $b).to_string())
                }
            }
        )*
    };
}

// Operators and condition codes (muted red)
colored_node!(
    MUTED_RED.0, MUTED_RED.1, MUTED_RED.2;
    ParserUnaryOp,
    ParserBinOp,
    AssignOp,
    IncDec,    // Parser
    BinOp,     // Tacky
    UnaryOp,
    BinaryOp,
    CondCode,  // Codegen
);

// Registers (teal-green)
colored_node!(
    TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2;
    Reg,
);

// TACKY function definition - compact format
impl ItfDisplay for TackyFunctionDefinition {
    fn itf_node(&self) -> Node {
        let body_children: Vec<Node> = self.body.iter().map(|i| i.itf_node()).collect();
        Node::branch(format!("{}: {}", "FunctionDefinition".truecolor(TEAL.0, TEAL.1, TEAL.2), self.name), body_children)
    }
}

// Codegen function definition - compact format
impl ItfDisplay for CodegenFunctionDefinition {
    fn itf_node(&self) -> Node {
        let body_children: Vec<Node> = self.body.iter().map(|i| i.itf_node()).collect();
        Node::branch(format!("{}: {}", "FunctionDefinition".truecolor(TEAL.0, TEAL.1, TEAL.2), self.name), body_children)
    }
}

// Macro for types with `top_level` field (Program pattern)
macro_rules! impl_program {
    ($($ty:ty),* $(,)?) => {
        $(
            impl ItfDisplay for $ty {
                fn itf_node(&self) -> Node {
                    let children: Vec<Node> = self.top_level.iter().map(|t| t.itf_node()).collect();
                    Node::branch("Program".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), children)
                }
            }
        )*
    };
}

impl_program!(TackyProgram);

impl ItfDisplay for CodegenProgram {
    fn itf_node(&self) -> Node {
        let mut children: Vec<Node> = self.functions.iter().map(|f| f.itf_node()).collect();
        children.extend(self.static_vars.iter().map(|v| v.itf_node()));
        Node::branch("Program".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), children)
    }
}

impl ItfDisplay for ParserProgram {
    fn itf_node(&self) -> Node {
        let children: Vec<Node> = self.declarations.iter().map(|d| d.itf_node()).collect();
        Node::branch("Program".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), children)
    }
}

// Macro for enums that just delegate to inner types
macro_rules! delegate_variants {
    ($ty:ty { $($variant:ident),* $(,)? }) => {
        impl ItfDisplay for $ty {
            fn itf_node(&self) -> Node {
                match self {
                    $( Self::$variant(inner) => inner.itf_node(), )*
                }
            }
        }
    };
}

delegate_variants!(Declaration {
    VarDeclaration,
    FunDeclaration
});
delegate_variants!(BlockItem { Statement, Declaration });
delegate_variants!(TopLevel {
    Function,
    StaticVariable
});

// Macro for wrapper types that delegate to a single field
macro_rules! delegate_field {
    ($ty:ty => $field:ident) => {
        impl ItfDisplay for $ty {
            fn itf_node(&self) -> Node {
                self.$field.itf_node()
            }
        }
    };
}

delegate_field!(SpannedStmt => stmt);

impl ItfDisplay for String {
    fn itf_node(&self) -> Node {
        Node::leaf(self.truecolor(CREAM.0, CREAM.1, CREAM.2).to_string())
    }
}

impl ItfDisplay for &str {
    fn itf_node(&self) -> Node {
        Node::leaf(self.truecolor(CREAM.0, CREAM.1, CREAM.2).to_string())
    }
}

// Macro to implement ItfDisplay for all integer types
macro_rules! impl_itf_display_for_int {
    ($($t:ty),*) => {
        $(
            impl ItfDisplay for $t {
                fn itf_node(&self) -> Node {
                    Node::leaf(self.to_string().truecolor(CYAN_TEAL.0, CYAN_TEAL.1, CYAN_TEAL.2).to_string())
                }
            }
        )*
    }
}

// Implement for all integer types
impl_itf_display_for_int!(i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);

impl<T: ItfDisplay> ItfDisplay for Vec<T> {
    fn itf_node(&self) -> Node {
        let children: Vec<Node> = self.iter().map(|item| item.itf_node()).collect();
        Node::branch(format!("[{}]", self.len()).truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), children)
    }
}

// =============================================================================
// Parser types
// =============================================================================

impl ItfDisplay for ForInit {
    fn itf_node(&self) -> Node {
        match self {
            ForInit::InitDecl(decl) => decl.itf_node(),
            ForInit::InitExp(opt_expr) => match opt_expr {
                Some(expr) => expr.itf_node(),
                None => Node::leaf("None".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string()),
            },
        }
    }
}

impl ItfDisplay for Identifier {
    fn itf_node(&self) -> Node {
        Node::leaf(format!("Identifier(\"{}\")", self.0).truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2).to_string())
    }
}

impl ItfDisplay for Expr {
    fn itf_node(&self) -> Node {
        match self {
            Expr::Constant(c) => Node::leaf(format!("Constant({c})").truecolor(YELLOW_GOLD.0, YELLOW_GOLD.1, YELLOW_GOLD.2).to_string()),
            Expr::Var(id, _span) => id.itf_node(),
            Expr::Unary(op, e) => Node::branch(format!("Unary ({op:?})").truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), vec![e.itf_node()]),
            Expr::Binary(op, e1, e2) => Node::branch(
                format!("Binary ({op:?})").truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(),
                vec![e1.itf_node(), e2.itf_node()],
            ),
            Expr::Assignment(lhs, rhs, _span) => {
                Node::branch("Assignment".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), vec![lhs.itf_node(), rhs.itf_node()])
            }
            Expr::CompoundAssignment(op, lhs, rhs, _span) => Node::branch(
                format!("CompoundAssignment ({op:?})").truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(),
                vec![lhs.itf_node(), rhs.itf_node()],
            ),
            Expr::PostFixOp(op, e, _span) => {
                Node::branch(format!("PostFix ({op:?})").truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), vec![e.itf_node()])
            }
            Expr::PreFixOp(op, e, _span) => {
                Node::branch(format!("PreFix ({op:?})").truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), vec![e.itf_node()])
            }
            Expr::Conditional(condition, then_expr, else_expr) => Node::branch(
                "Conditional".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(),
                vec![
                    Node::branch("condition:", vec![condition.itf_node()]),
                    Node::branch("then:", vec![then_expr.itf_node()]),
                    Node::branch("else:", vec![else_expr.itf_node()]),
                ],
            ),
            Expr::FunctionCall(name, args, _span) => {
                let mut children = vec![Node::leaf(format!("name: {}", name.itf_node().text))];
                if !args.is_empty() {
                    let arg_nodes: Vec<Node> = args.iter().map(|a| a.itf_node()).collect();
                    children.push(Node::branch("args:", arg_nodes));
                }
                Node::branch("FunctionCall".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), children)
            }
        }
    }
}

impl ItfDisplay for Stmt {
    fn itf_node(&self) -> Node {
        match self {
            Stmt::Return(expr) => Node::branch("Return".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string(), vec![expr.itf_node()]),
            Stmt::Expression(expr) => Node::branch("Expression".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string(), vec![expr.itf_node()]),
            Stmt::If(condition, then_stmt, else_stmt) => {
                let mut children = vec![
                    Node::branch("condition:", vec![condition.itf_node()]),
                    Node::branch("then:", vec![then_stmt.stmt.itf_node()]),
                ];
                if let Some(else_s) = else_stmt.as_ref() {
                    children.push(Node::branch("else:", vec![else_s.stmt.itf_node()]));
                }
                Node::branch("If".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string(), children)
            }
            Stmt::Null => Node::leaf("Null".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string()),
            Stmt::Goto(label) => Node::branch("Goto".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string(), vec![label.itf_node()]),
            Stmt::Labeled(label, stmt) => Node::branch(
                "Labeled".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string(),
                vec![label.itf_node(), stmt.stmt.itf_node()],
            ),
            Stmt::Compound(block) => {
                let children: Vec<Node> = block.iter().map(|item| item.itf_node()).collect();
                Node::branch("Compound".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string(), children)
            }
            Stmt::Break(target) => {
                let label_str = if target.0.is_empty() {
                    "Break".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string()
                } else {
                    format!("{} [{}]", "Break".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), target.0)
                };
                Node::leaf(label_str)
            }
            Stmt::Continue(target) => {
                let label_str = if target.0.is_empty() {
                    "Continue".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string()
                } else {
                    format!("{} [{}]", "Continue".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), target.0)
                };
                Node::leaf(label_str)
            }
            Stmt::While(condition, body, label) => Node::branch(
                format!("{} [label: {}]", "While".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), label),
                vec![
                    Node::branch("condition:", vec![condition.itf_node()]),
                    Node::branch("body:", vec![body.stmt.itf_node()]),
                ],
            ),
            Stmt::DoWhile(body, condition, label) => Node::branch(
                format!("{} [label: {}]", "DoWhile".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), label),
                vec![
                    Node::branch("body:", vec![body.stmt.itf_node()]),
                    Node::branch("condition:", vec![condition.itf_node()]),
                ],
            ),
            Stmt::For(init, condition, post, body, label) => {
                let mut children = vec![Node::branch("init:", vec![init.itf_node()])];
                if let Some(cond) = condition {
                    children.push(Node::branch("condition:", vec![cond.itf_node()]));
                }
                if let Some(post_expr) = post {
                    children.push(Node::branch("post:", vec![post_expr.itf_node()]));
                }
                children.push(Node::branch("body:", vec![body.stmt.itf_node()]));
                Node::branch(format!("{} [label: {}]", "For".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), label), children)
            }
            Stmt::Switch(expr, body, label, _) => Node::branch(
                format!("{} [label: {}]", "Switch".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), label),
                vec![
                    Node::branch("expr:", vec![expr.itf_node()]),
                    Node::branch("body:", vec![body.stmt.itf_node()]),
                ],
            ),
            Stmt::Case(expr, stmt, label) => {
                let header = if label.0.is_empty() {
                    "Case".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string()
                } else {
                    format!("{} [{}]", "Case".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), label.0)
                };
                Node::branch(
                    header,
                    vec![
                        Node::branch("expr:", vec![expr.itf_node()]),
                        Node::branch("stmt:", vec![stmt.stmt.itf_node()]),
                    ],
                )
            }
            Stmt::Default(stmt, label) => {
                let header = if label.0.is_empty() {
                    "Default".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string()
                } else {
                    format!("{} [{}]", "Default".truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2), label.0)
                };
                Node::branch(header, vec![stmt.stmt.itf_node()])
            }
        }
    }
}

impl ItfDisplay for VarDeclaration {
    fn itf_node(&self) -> Node {
        let name_node = Node::leaf(format!("name: {}", self.name.itf_node().text));
        match &self.init {
            Some(init_expr) => {
                let init_node = Node::branch("init:", vec![init_expr.itf_node()]);
                Node::branch("VarDeclaration".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), vec![name_node, init_node])
            }
            None => Node::branch("VarDeclaration".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), vec![name_node]),
        }
    }
}

impl ItfDisplay for FunDeclaration {
    fn itf_node(&self) -> Node {
        let mut children = vec![Node::leaf(format!("name: {}", self.name.itf_node().text))];
        if !self.params.is_empty() {
            let param_nodes: Vec<Node> = self.params.iter().map(|p| p.itf_node()).collect();
            children.push(Node::branch("params:", param_nodes));
        }
        if let Some(body) = &self.body {
            let body_nodes: Vec<Node> = body.iter().map(|b| b.itf_node()).collect();
            children.push(Node::branch("body:", body_nodes));
        }
        Node::branch("FunDeclaration".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string(), children)
    }
}

// =============================================================================
// Tacky types
// =============================================================================

impl ItfDisplay for Val {
    fn itf_node(&self) -> Node {
        match self {
            Val::Constant(c) => Node::leaf(c.to_string().truecolor(CYAN_TEAL.0, CYAN_TEAL.1, CYAN_TEAL.2).to_string()),
            Val::Var(s) => Node::leaf(s.truecolor(CREAM.0, CREAM.1, CREAM.2).to_string()),
        }
    }
}

// Helper to format Val compactly as string
fn val_str(v: &Val) -> String {
    match v {
        Val::Constant(c) => c.to_string().truecolor(CYAN_TEAL.0, CYAN_TEAL.1, CYAN_TEAL.2).to_string(),
        Val::Var(s) => s.truecolor(CREAM.0, CREAM.1, CREAM.2).to_string(),
    }
}

impl ItfDisplay for TackyInstruction {
    fn itf_node(&self) -> Node {
        match self {
            TackyInstruction::Return(v) => {
                Node::leaf(format!("{} {}", "Return".truecolor(TEAL.0, TEAL.1, TEAL.2), val_str(v)))
            }
            TackyInstruction::Unary { op, src, dst } => {
                Node::leaf(format!("{} {:?} {} -> {}", "Unary".truecolor(TEAL.0, TEAL.1, TEAL.2), op, val_str(src), val_str(dst)))
            }
            TackyInstruction::Binary { op, src1, src2, dst } => {
                Node::leaf(format!("{} {:?} {}, {} -> {}", "Binary".truecolor(TEAL.0, TEAL.1, TEAL.2), op, val_str(src1), val_str(src2), val_str(dst)))
            }
            TackyInstruction::Copy { src, dst } => {
                Node::leaf(format!("{} {} -> {}", "Copy".truecolor(TEAL.0, TEAL.1, TEAL.2), val_str(src), val_str(dst)))
            }
            TackyInstruction::Jump { target } => {
                Node::leaf(format!("{} -> {}", "Jump".truecolor(TEAL.0, TEAL.1, TEAL.2), target.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
            TackyInstruction::JumpIfZero { condition, target } => {
                Node::leaf(format!("{} {} -> {}", "JumpIfZero".truecolor(TEAL.0, TEAL.1, TEAL.2), val_str(condition), target.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
            TackyInstruction::JumpIfNotZero { condition, target } => {
                Node::leaf(format!("{} {} -> {}", "JumpIfNotZero".truecolor(TEAL.0, TEAL.1, TEAL.2), val_str(condition), target.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
            TackyInstruction::Label(lbl) => {
                Node::leaf(format!("{}({})", "Label".truecolor(TEAL.0, TEAL.1, TEAL.2), lbl.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
            TackyInstruction::FunCall { fun_name, args, dst } => {
                let args_str = args.iter().map(val_str).collect::<Vec<_>>().join(", ");
                Node::leaf(format!("{} {}({}) -> {}", "FunCall".truecolor(TEAL.0, TEAL.1, TEAL.2), fun_name.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2), args_str, val_str(dst)))
            }
        }
    }
}

impl ItfDisplay for StaticVariable {
    fn itf_node(&self) -> Node {
        let init_str = match &self.init {
            VarInit::Defined(v) => format!("init: {}", v),
            VarInit::Extern => "extern".to_string(),
        };
        let global_str = if self.global { ", global" } else { "" };
        Node::leaf(format!("{}: {} ({}{})", "StaticVariable".truecolor(TEAL.0, TEAL.1, TEAL.2), self.name, init_str, global_str))
    }
}

// =============================================================================
// Codegen types
// =============================================================================

// Helper to format Operand compactly as string
fn operand_str(op: &Operand) -> String {
    match op {
        Operand::Imm(i) => format!("{}({})",
            "Imm".truecolor(MUTED_RED.0, MUTED_RED.1, MUTED_RED.2),
            i.to_string().truecolor(CYAN_TEAL.0, CYAN_TEAL.1, CYAN_TEAL.2)),
        Operand::Reg(r) => format!("{:?}", r).truecolor(TEAL_GREEN.0, TEAL_GREEN.1, TEAL_GREEN.2).to_string(),
        Operand::Pseudo(s) => format!("{}({})",
            "Pseudo".truecolor(MUTED_RED.0, MUTED_RED.1, MUTED_RED.2),
            s.truecolor(CREAM.0, CREAM.1, CREAM.2)),
        Operand::Stack(o) => format!("{}({})",
            "Stack".truecolor(MUTED_RED.0, MUTED_RED.1, MUTED_RED.2),
            o.to_string().truecolor(CYAN_TEAL.0, CYAN_TEAL.1, CYAN_TEAL.2)),
        Operand::Data(s) => format!("{}({})",
            "Data".truecolor(MUTED_RED.0, MUTED_RED.1, MUTED_RED.2),
            s.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)),
    }
}

impl ItfDisplay for Operand {
    fn itf_node(&self) -> Node {
        Node::leaf(operand_str(self))
    }
}

impl ItfDisplay for CodegenInstruction {
    fn itf_node(&self) -> Node {
        match self {
            CodegenInstruction::Mov { src, dst } => {
                Node::leaf(format!("{} {} -> {}", "Mov".truecolor(TEAL.0, TEAL.1, TEAL.2), operand_str(src), operand_str(dst)))
            }
            CodegenInstruction::Unary { op, dst } => {
                Node::leaf(format!("{} {:?} {}", "Unary".truecolor(TEAL.0, TEAL.1, TEAL.2), op, operand_str(dst)))
            }
            CodegenInstruction::Binary { op, src, dst } => {
                Node::leaf(format!("{} {:?} {} -> {}", "Binary".truecolor(TEAL.0, TEAL.1, TEAL.2), op, operand_str(src), operand_str(dst)))
            }
            CodegenInstruction::Cmp { v1, v2 } => {
                Node::leaf(format!("{} {}, {}", "Cmp".truecolor(TEAL.0, TEAL.1, TEAL.2), operand_str(v1), operand_str(v2)))
            }
            CodegenInstruction::Idiv(op) => {
                Node::leaf(format!("{} {}", "Idiv".truecolor(TEAL.0, TEAL.1, TEAL.2), operand_str(op)))
            }
            CodegenInstruction::Cdq => Node::leaf("Cdq".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string()),
            CodegenInstruction::Jmp(label) => {
                Node::leaf(format!("{} -> {}", "Jmp".truecolor(TEAL.0, TEAL.1, TEAL.2), label.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
            CodegenInstruction::JmpCC { code, label } => {
                Node::leaf(format!("{} {:?} -> {}", "JmpCC".truecolor(TEAL.0, TEAL.1, TEAL.2), code, label.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
            CodegenInstruction::SetCC { code, op } => {
                Node::leaf(format!("{} {:?} {}", "SetCC".truecolor(TEAL.0, TEAL.1, TEAL.2), code, operand_str(op)))
            }
            CodegenInstruction::Label(l) => {
                Node::leaf(format!("{}({})", "Label".truecolor(TEAL.0, TEAL.1, TEAL.2), l.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
            CodegenInstruction::AllocateStack(off) => {
                Node::leaf(format!("{} {}", "AllocateStack".truecolor(TEAL.0, TEAL.1, TEAL.2), off.to_string().truecolor(CYAN_TEAL.0, CYAN_TEAL.1, CYAN_TEAL.2)))
            }
            CodegenInstruction::Ret => Node::leaf("Ret".truecolor(TEAL.0, TEAL.1, TEAL.2).to_string()),
            CodegenInstruction::DeallocateStack(off) => {
                Node::leaf(format!("{} {}", "DeallocateStack".truecolor(TEAL.0, TEAL.1, TEAL.2), off.to_string().truecolor(CYAN_TEAL.0, CYAN_TEAL.1, CYAN_TEAL.2)))
            }
            CodegenInstruction::Push(op) => {
                Node::leaf(format!("{} {}", "Push".truecolor(TEAL.0, TEAL.1, TEAL.2), operand_str(op)))
            }
            CodegenInstruction::Call(name) => {
                Node::leaf(format!("{} {}", "Call".truecolor(TEAL.0, TEAL.1, TEAL.2), name.0.truecolor(YELLOW_GREEN.0, YELLOW_GREEN.1, YELLOW_GREEN.2)))
            }
        }
    }
}
