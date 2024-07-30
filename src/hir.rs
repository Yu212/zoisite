use ecow::EcoString;
use la_arena::Idx;
use rowan::TextRange;

use crate::resolve_context::FuncInfo;
use crate::scope::{FnId, VarId};

pub type ExprIdx = Idx<Expr>;
pub type StmtIdx = Idx<Stmt>;
pub type FuncIdx = Idx<Func>;

#[derive(Debug, Clone)]
pub struct Root {
    pub stmts: Vec<StmtIdx>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub fn_info: Option<FuncInfo>,
    pub block: ExprIdx,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    LetStmt {
        var_id: Option<VarId>,
        expr: ExprIdx,
        range: TextRange,
    },
    WhileStmt {
        cond: ExprIdx,
        block: ExprIdx,
        range: TextRange,
    },
    BreakStmt {
        range: TextRange,
    },
    ExprStmt {
        expr: ExprIdx,
        range: TextRange,
    },
    FuncDef {
        func: FuncIdx,
        range: TextRange,
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Missing,
    Binary {
        op: BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
        range: TextRange,
    },
    Unary {
        op: UnaryOp,
        expr: ExprIdx,
        range: TextRange,
    },
    Ref {
        var_id: Option<VarId>,
        range: TextRange,
    },
    If {
        cond: ExprIdx,
        then_expr: ExprIdx,
        else_expr: Option<ExprIdx>,
        range: TextRange,
    },
    FnCall {
        fn_id: Option<FnId>,
        args: Vec<ExprIdx>,
        range: TextRange,
    },
    Index {
        main_expr: ExprIdx,
        index_expr: ExprIdx,
        range: TextRange,
    },
    Block {
        stmts: Vec<StmtIdx>,
        range: TextRange,
    },
    NumberLiteral {
        n: Option<u64>,
        range: TextRange,
    },
    BoolLiteral {
        val: bool,
        range: TextRange,
    },
    StringLiteral {
        val: Option<EcoString>,
        range: TextRange,
    },
    ArrayLiteral {
        len: ExprIdx,
        initial: ExprIdx,
        range: TextRange,
    },
}

#[derive(Debug)]
pub struct Identifier {
    pub name: EcoString,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpKind {
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    PostfixOp(PostfixOp),
}

impl OpKind {
    pub fn binding_power(&self) -> (i8, i8) {
        match self {
            OpKind::BinaryOp(BinaryOp::Add) => (5, 6),
            OpKind::BinaryOp(BinaryOp::Sub) => (5, 6),
            OpKind::BinaryOp(BinaryOp::Mul) => (7, 8),
            OpKind::BinaryOp(BinaryOp::Div) => (7, 8),
            OpKind::BinaryOp(BinaryOp::Rem) => (7, 8),
            OpKind::BinaryOp(BinaryOp::Assign) => (2, 1),
            OpKind::BinaryOp(BinaryOp::EqEq) => (3, 4),
            OpKind::BinaryOp(BinaryOp::Neq) => (3, 4),
            OpKind::BinaryOp(BinaryOp::Ge) => (3, 4),
            OpKind::BinaryOp(BinaryOp::Le) => (3, 4),
            OpKind::BinaryOp(BinaryOp::Gt) => (3, 4),
            OpKind::BinaryOp(BinaryOp::Lt) => (3, 4),
            OpKind::PostfixOp(PostfixOp::Index) => (10, -1),
            OpKind::UnaryOp(UnaryOp::Neg) => (-1, 9),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Assign,
    EqEq,
    Neq,
    Ge,
    Le,
    Gt,
    Lt,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PostfixOp {
    Index,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    Neg,
}
