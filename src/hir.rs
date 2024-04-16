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
    WhileStmt {
        cond: ExprIdx,
        block: ExprIdx,
    },
    BreakStmt {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Assign,
}

impl BinaryOp {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOp::Add => (3, 4),
            BinaryOp::Sub => (3, 4),
            BinaryOp::Mul => (5, 6),
            BinaryOp::Div => (5, 6),
            BinaryOp::Rem => (5, 6),
            BinaryOp::Assign => (2, 1),
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
            UnaryOp::Neg => ((), 7),
        }
    }
}
