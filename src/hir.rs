use la_arena::Idx;

type ExprIdx = Idx<Expr>;

#[derive(Debug)]
pub struct Root {
    pub expr: ExprIdx,
}

#[derive(Debug)]
pub enum Expr {
    Missing,
    Binary {
        op: BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    Literal {
        n: Option<u64>,
    }
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
