use ecow::EcoString;
use la_arena::Idx;

type ExprIdx = Idx<Expr>;
type StmtIdx = Idx<Stmt>;

#[derive(Debug)]
pub struct Root {
    pub stmts: Vec<StmtIdx>,
}

pub enum Stmt {
    LetStmt {
        name: Option<EcoString>,
        expr: ExprIdx,
    },
    ExprStmt {
        expr: ExprIdx,
    },
}

#[derive(Debug)]
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
    Literal {
        n: Option<u64>,
    },
}

#[derive(Debug)]
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

#[derive(Debug)]
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
