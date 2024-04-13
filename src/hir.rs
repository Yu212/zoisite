use ecow::EcoString;
use la_arena::Idx;

use crate::scope::{FnId, VarId};

type ExprIdx = Idx<Expr>;
type StmtIdx = Idx<Stmt>;

#[derive(Debug)]
pub struct Root {
    pub stmts: Vec<StmtIdx>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    LetStmt {
        var_id: Option<VarId>,
        expr: ExprIdx,
    },
    ExprStmt {
        expr: ExprIdx,
    },
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
}

#[derive(Debug)]
pub struct Identifier {
    pub name: EcoString,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl BinaryOp {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOp::Add => (1, 2),
            BinaryOp::Sub => (1, 2),
            BinaryOp::Mul => (3, 4),
            BinaryOp::Div => (3, 4),
            BinaryOp::Rem => (3, 4),
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
            UnaryOp::Neg => ((), 5),
        }
    }
}
