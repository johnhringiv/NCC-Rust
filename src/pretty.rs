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
    BinOp, FunctionDefinition as TackyFunctionDefinition, Instruction as TackyInstruction,
    Program as TackyProgram, StaticVariable, TopLevel, Val,
};

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

macro_rules! simple_node {
    ($($ty:ty),* $(,)?) => {
        $(
            impl ItfDisplay for $ty {
                fn itf_node(&self) -> Node {
                    Node::leaf(format!("{:?}", self).cyan().to_string())
                }
            }
        )*
    };
}

// All simple Debug-based display types
simple_node!(
    ParserUnaryOp,
    ParserBinOp,
    AssignOp,
    IncDec, // Parser
    BinOp,  // Tacky
    Reg,
    UnaryOp,
    BinaryOp,
    CondCode, // Codegen
);

// Macro for types with `name` and `body` fields (FunctionDefinition pattern)
macro_rules! impl_function_def {
    ($($ty:ty),* $(,)?) => {
        $(
            impl ItfDisplay for $ty {
                fn itf_node(&self) -> Node {
                    let name_line = Node::leaf(format!("name: {}", self.name.itf_node().text));
                    let body_children: Vec<Node> = self.body.iter().map(|i| i.itf_node()).collect();
                    let body_node = Node::branch("body".cyan().to_string(), body_children);
                    Node::branch("FunctionDefinition".cyan().to_string(), vec![name_line, body_node])
                }
            }
        )*
    };
}

impl_function_def!(TackyFunctionDefinition, CodegenFunctionDefinition);

// Macro for types with `top_level` field (Program pattern)
macro_rules! impl_program {
    ($($ty:ty),* $(,)?) => {
        $(
            impl ItfDisplay for $ty {
                fn itf_node(&self) -> Node {
                    let children: Vec<Node> = self.top_level.iter().map(|t| t.itf_node()).collect();
                    Node::branch("Program".cyan().to_string(), children)
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
        Node::branch("Program".cyan().to_string(), children)
    }
}

impl ItfDisplay for ParserProgram {
    fn itf_node(&self) -> Node {
        let children: Vec<Node> = self.declarations.iter().map(|d| d.itf_node()).collect();
        Node::branch("Program".cyan().to_string(), children)
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
delegate_variants!(TopLevel { Function, StaticVariable });

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
        Node::leaf(self.green().to_string())
    }
}

impl ItfDisplay for &str {
    fn itf_node(&self) -> Node {
        Node::leaf(self.green().to_string())
    }
}

// Macro to implement ItfDisplay for all integer types
macro_rules! impl_itf_display_for_int {
    ($($t:ty),*) => {
        $(
            impl ItfDisplay for $t {
                fn itf_node(&self) -> Node {
                    Node::leaf(self.to_string().magenta().to_string())
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
        Node::branch(format!("[{}]", self.len()).cyan().to_string(), children)
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
                None => Node::leaf("None".cyan().to_string()),
            },
        }
    }
}

impl ItfDisplay for Identifier {
    fn itf_node(&self) -> Node {
        Node::leaf(format!("Identifier(\"{}\")", self.0).green().to_string())
    }
}

impl ItfDisplay for Expr {
    fn itf_node(&self) -> Node {
        match self {
            Expr::Constant(c) => Node::leaf(format!("Constant({c})").yellow().to_string()),
            Expr::Var(id, _span) => id.itf_node(),
            Expr::Unary(op, e) => Node::branch(format!("Unary ({op:?})").cyan().to_string(), vec![e.itf_node()]),
            Expr::Binary(op, e1, e2) => Node::branch(
                format!("Binary ({op:?})").cyan().to_string(),
                vec![e1.itf_node(), e2.itf_node()],
            ),
            Expr::Assignment(lhs, rhs, _span) => {
                Node::branch("Assignment".cyan().to_string(), vec![lhs.itf_node(), rhs.itf_node()])
            }
            Expr::CompoundAssignment(op, lhs, rhs, _span) => Node::branch(
                format!("CompoundAssignment ({op:?})").cyan().to_string(),
                vec![lhs.itf_node(), rhs.itf_node()],
            ),
            Expr::PostFixOp(op, e, _span) => {
                Node::branch(format!("PostFix ({op:?})").cyan().to_string(), vec![e.itf_node()])
            }
            Expr::PreFixOp(op, e, _span) => {
                Node::branch(format!("PreFix ({op:?})").cyan().to_string(), vec![e.itf_node()])
            }
            Expr::Conditional(condition, then_expr, else_expr) => Node::branch(
                "Conditional".cyan().to_string(),
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
                Node::branch("FunctionCall".cyan().to_string(), children)
            }
        }
    }
}

impl ItfDisplay for Stmt {
    fn itf_node(&self) -> Node {
        match self {
            Stmt::Return(expr) => Node::branch("Return".cyan().to_string(), vec![expr.itf_node()]),
            Stmt::Expression(expr) => Node::branch("Expression".cyan().to_string(), vec![expr.itf_node()]),
            Stmt::If(condition, then_stmt, else_stmt) => {
                let mut children = vec![
                    Node::branch("condition:", vec![condition.itf_node()]),
                    Node::branch("then:", vec![then_stmt.stmt.itf_node()]),
                ];
                if let Some(else_s) = else_stmt.as_ref() {
                    children.push(Node::branch("else:", vec![else_s.stmt.itf_node()]));
                }
                Node::branch("If".cyan().to_string(), children)
            }
            Stmt::Null => Node::leaf("Null".cyan().to_string()),
            Stmt::Goto(label) => Node::branch("Goto".cyan().to_string(), vec![label.itf_node()]),
            Stmt::Labeled(label, stmt) => Node::branch(
                "Labeled".cyan().to_string(),
                vec![label.itf_node(), stmt.stmt.itf_node()],
            ),
            Stmt::Compound(block) => {
                let children: Vec<Node> = block.iter().map(|item| item.itf_node()).collect();
                Node::branch("Compound".cyan().to_string(), children)
            }
            Stmt::Break(..) => Node::leaf("Break".cyan().to_string()),
            Stmt::Continue(..) => Node::leaf("Continue".cyan().to_string()),
            Stmt::While(condition, body, _) => Node::branch(
                "While".cyan().to_string(),
                vec![
                    Node::branch("condition:", vec![condition.itf_node()]),
                    Node::branch("body:", vec![body.stmt.itf_node()]),
                ],
            ),
            Stmt::DoWhile(body, condition, _) => Node::branch(
                "DoWhile".cyan().to_string(),
                vec![
                    Node::branch("body:", vec![body.stmt.itf_node()]),
                    Node::branch("condition:", vec![condition.itf_node()]),
                ],
            ),
            Stmt::For(init, condition, post, body, _) => {
                let mut children = vec![Node::branch("init:", vec![init.itf_node()])];
                if let Some(cond) = condition {
                    children.push(Node::branch("condition:", vec![cond.itf_node()]));
                }
                if let Some(post_expr) = post {
                    children.push(Node::branch("post:", vec![post_expr.itf_node()]));
                }
                children.push(Node::branch("body:", vec![body.stmt.itf_node()]));
                Node::branch("For".cyan().to_string(), children)
            }
            Stmt::Switch(expr, body, ..) => Node::branch(
                "Switch".cyan().to_string(),
                vec![
                    Node::branch("expr:", vec![expr.itf_node()]),
                    Node::branch("body:", vec![body.stmt.itf_node()]),
                ],
            ),
            Stmt::Case(expr, stmt, _) => Node::branch(
                "Case".cyan().to_string(),
                vec![
                    Node::branch("expr:", vec![expr.itf_node()]),
                    Node::branch("stmt:", vec![stmt.stmt.itf_node()]),
                ],
            ),
            Stmt::Default(stmt, ..) => Node::branch("Default".cyan().to_string(), vec![stmt.stmt.itf_node()]),
        }
    }
}

impl ItfDisplay for VarDeclaration {
    fn itf_node(&self) -> Node {
        let name_node = Node::leaf(format!("name: {}", self.name.itf_node().text));
        match &self.init {
            Some(init_expr) => {
                let init_node = Node::branch("init:", vec![init_expr.itf_node()]);
                Node::branch("VarDeclaration".cyan().to_string(), vec![name_node, init_node])
            }
            None => Node::branch("VarDeclaration".cyan().to_string(), vec![name_node]),
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
        Node::branch("FunDeclaration".cyan().to_string(), children)
    }
}

// =============================================================================
// Tacky types
// =============================================================================

impl ItfDisplay for Val {
    fn itf_node(&self) -> Node {
        match self {
            Val::Constant(c) => Node::branch("Const".cyan().to_string(), vec![c.itf_node()]),
            Val::Var(s) => Node::branch("Var".cyan().to_string(), vec![s.itf_node()]),
        }
    }
}

impl ItfDisplay for TackyInstruction {
    fn itf_node(&self) -> Node {
        match self {
            TackyInstruction::Return(v) => Node::branch("Return".cyan().to_string(), vec![v.itf_node()]),
            TackyInstruction::Unary { op, src, dst } => Node::branch(
                "Unary".cyan().to_string(),
                vec![op.itf_node(), src.itf_node(), dst.itf_node()],
            ),
            TackyInstruction::Binary { op, src1, src2, dst } => Node::branch(
                "Binary".cyan().to_string(),
                vec![op.itf_node(), src1.itf_node(), src2.itf_node(), dst.itf_node()],
            ),
            TackyInstruction::Copy { src, dst } => {
                Node::branch("Copy".cyan().to_string(), vec![src.itf_node(), dst.itf_node()])
            }
            TackyInstruction::Jump { target } => Node::branch("Jump".cyan().to_string(), vec![target.itf_node()]),
            TackyInstruction::JumpIfZero { condition, target } => Node::branch(
                "JumpIfZero".cyan().to_string(),
                vec![condition.itf_node(), target.itf_node()],
            ),
            TackyInstruction::JumpIfNotZero { condition, target } => Node::branch(
                "JumpIfNotZero".cyan().to_string(),
                vec![condition.itf_node(), target.itf_node()],
            ),
            TackyInstruction::Label(lbl) => Node::branch("Label".cyan().to_string(), vec![lbl.itf_node()]),
            TackyInstruction::FunCall { fun_name, args, dst } => {
                let mut children = vec![fun_name.itf_node()];
                children.extend(args.iter().map(|a| a.itf_node()));
                children.push(dst.itf_node());
                Node::branch("FunCall".cyan().to_string(), children)
            }
        }
    }
}

impl ItfDisplay for StaticVariable {
    fn itf_node(&self) -> Node {
        Node::branch(
            "StaticVariable".cyan().to_string(),
            vec![
                Node::leaf(format!("name: {}", self.name.green())),
                Node::leaf(format!("global: {}", self.global)),
                Node::leaf(format!("init: {}", self.init.to_string().magenta())),
            ],
        )
    }
}

// =============================================================================
// Codegen types
// =============================================================================

impl ItfDisplay for Operand {
    fn itf_node(&self) -> Node {
        match self {
            Operand::Imm(i) => Node::branch("Imm".cyan().to_string(), vec![i.itf_node()]),
            Operand::Reg(r) => Node::branch("Reg".cyan().to_string(), vec![r.itf_node()]),
            Operand::Pseudo(s) => Node::branch("Pseudo".cyan().to_string(), vec![s.itf_node()]),
            Operand::Stack(o) => Node::branch("Stack".cyan().to_string(), vec![o.itf_node()]),
            Operand::Data(s) => Node::branch("Data".cyan().to_string(), vec![s.itf_node()]),
        }
    }
}

impl ItfDisplay for CodegenInstruction {
    fn itf_node(&self) -> Node {
        match self {
            CodegenInstruction::Mov { src, dst } => {
                Node::branch("Mov".cyan().to_string(), vec![src.itf_node(), dst.itf_node()])
            }
            CodegenInstruction::Unary { op, dst } => {
                Node::branch("Unary".cyan().to_string(), vec![op.itf_node(), dst.itf_node()])
            }
            CodegenInstruction::Binary { op, src, dst } => Node::branch(
                "Binary".cyan().to_string(),
                vec![op.itf_node(), src.itf_node(), dst.itf_node()],
            ),
            CodegenInstruction::Cmp { v1, v2 } => {
                Node::branch("Cmp".cyan().to_string(), vec![v1.itf_node(), v2.itf_node()])
            }
            CodegenInstruction::Idiv(op) => Node::branch("Idiv".cyan().to_string(), vec![op.itf_node()]),
            CodegenInstruction::Cdq => Node::leaf("Cdq".cyan().to_string()),
            CodegenInstruction::Jmp(label) => Node::branch("Jmp".cyan().to_string(), vec![label.itf_node()]),
            CodegenInstruction::JmpCC { code, label } => {
                Node::branch("JmpCC".cyan().to_string(), vec![code.itf_node(), label.itf_node()])
            }
            CodegenInstruction::SetCC { code, op } => {
                Node::branch("SetCC".cyan().to_string(), vec![code.itf_node(), op.itf_node()])
            }
            CodegenInstruction::Label(l) => Node::branch("Label".cyan().to_string(), vec![l.itf_node()]),
            CodegenInstruction::AllocateStack(off) => {
                Node::branch("AllocateStack".cyan().to_string(), vec![off.itf_node()])
            }
            CodegenInstruction::Ret => Node::leaf("Ret".cyan().to_string()),
            CodegenInstruction::DeallocateStack(off) => {
                Node::branch("DeallocateStack".cyan().to_string(), vec![off.itf_node()])
            }
            CodegenInstruction::Push(op) => Node::branch("Push".cyan().to_string(), vec![op.itf_node()]),
            CodegenInstruction::Call(name) => Node::branch("Call".cyan().to_string(), vec![name.itf_node()]),
        }
    }
}
