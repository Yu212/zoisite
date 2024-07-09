use ecow::EcoString;
use la_arena::Idx;

use crate::resolve_context::FuncInfo;
use crate::scope::{FnId, VarId};

type ExprIdx = Idx<Expr>;
type StmtIdx = Idx<Stmt>;
type FuncIdx = Idx<Func>;

#[derive(Debug)]
pub struct Root {
    pub stmts: Vec<StmtIdx>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub fn_info: Option<FuncInfo>,
    pub block: ExprIdx,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    LetStmt {
        var_id: Option<VarId>,
        expr: ExprIdx,
    },
    WhileStmt {
        cond: ExprIdx,
        block: ExprIdx,
    },
    BreakStmt {
    },
    ExprStmt {
        expr: ExprIdx,
    },
    FuncDef {
        func: FuncIdx,
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Missing,
    Binary {
        op: BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    Unary {
        op: UnaryOp,
        expr: ExprIdx,
    },
    Ref {
        var_id: Option<VarId>,
    },
    If {
        cond: ExprIdx,
        then_expr: ExprIdx,
        else_expr: Option<ExprIdx>,
    },
    FnCall {
        fn_id: Option<FnId>,
        args: Vec<ExprIdx>,
    },
    Block {
        stmts: Vec<StmtIdx>,
    },
    Literal {
        n: Option<u64>,
    },
    BoolLiteral {
        val: bool,
    },
}

#[derive(Debug)]
pub struct Identifier {
    pub name: EcoString,
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
}

impl BinaryOp {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOp::Add => (5, 6),
            BinaryOp::Sub => (5, 6),
            BinaryOp::Mul => (7, 8),
            BinaryOp::Div => (7, 8),
            BinaryOp::Rem => (7, 8),
            BinaryOp::Assign => (2, 1),
            BinaryOp::EqEq => (3, 4),
            BinaryOp::Neq => (3, 4),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
}

impl UnaryOp {
    pub fn binding_power(&self) -> ((), u8) {
        match self {
            UnaryOp::Neg => ((), 9),
        }
    }
}
