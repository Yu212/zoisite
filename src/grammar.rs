use crate::hir::{BinaryOp, OpKind, PostfixOp, UnaryOp};
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::SyntaxKind;
use crate::token_set::TokenSet;

const RECOVERY_SET: TokenSet = TokenSet::new(&[SyntaxKind::Semicolon]);
const BOOL_SET: TokenSet = TokenSet::new(&[SyntaxKind::TrueKw, SyntaxKind::FalseKw]);

pub fn root(p: &mut Parser<'_>) {
    let m = p.start();
    p.eat_trivia();
    while !p.at(SyntaxKind::Eof) {
        stmt(p);
    }
    m.complete(p, SyntaxKind::Root);
}

pub fn stmt(p: &mut Parser<'_>) -> CompletedMarker {
    match p.current() {
        SyntaxKind::LetKw => let_stmt(p),
        SyntaxKind::WhileKw => while_stmt(p),
        SyntaxKind::BreakKw => break_stmt(p),
        SyntaxKind::FunKw => func_stmt(p),
        _ => expr_stmt(p),
    }
}

pub fn func_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::FunKw));
    let m = p.start();
    p.bump();
    p.expect(SyntaxKind::Ident);
    param_list(p);
    p.expect(SyntaxKind::Colon);
    type_spec(p);
    block_expr(p);
    m.complete(p, SyntaxKind::FuncDef)
}

pub fn while_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::WhileKw));
    let m = p.start();
    p.bump();
    expr(p, 0);
    block_expr(p);
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::WhileStmt)
}

pub fn break_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::BreakKw));
    let m = p.start();
    p.bump();
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::BreakStmt)
}

pub fn expr_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    expr(p, 0);
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ExprStmt)
}

pub fn typed_ident(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Colon);
    type_spec(p);
    m.complete(p, SyntaxKind::TypedIdent)
}

pub fn type_spec(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.expect(SyntaxKind::Ident);
    let mut c = m.complete(p, SyntaxKind::IdentTypeSpec);
    while p.at(SyntaxKind::OpenBracket) {
        let m = c.precede(p);
        p.bump();
        p.expect(SyntaxKind::CloseBracket);
        c = m.complete(p, SyntaxKind::ArrayTypeSpec);
    }
    c
}

pub fn param_list(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.expect(SyntaxKind::OpenParen);
    if !p.at(SyntaxKind::CloseParen) {
        typed_ident(p);
        while p.eat(SyntaxKind::Comma) {
            typed_ident(p);
        }
    }
    p.expect(SyntaxKind::CloseParen);
    m.complete(p, SyntaxKind::ParamList)
}

pub fn let_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::LetKw));
    let m = p.start();
    p.bump();
    typed_ident(p);
    p.expect(SyntaxKind::Equals);
    expr(p, 0);
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::LetStmt)
}

pub fn expr(p: &mut Parser<'_>, min_binding_power: i8) -> Option<CompletedMarker> {
    let mut lhs = lhs(p)?;
    loop {
        let op: OpKind = match p.current() {
            SyntaxKind::Plus => OpKind::BinaryOp(BinaryOp::Add),
            SyntaxKind::Minus => OpKind::BinaryOp(BinaryOp::Sub),
            SyntaxKind::Star => OpKind::BinaryOp(BinaryOp::Mul),
            SyntaxKind::Slash => OpKind::BinaryOp(BinaryOp::Div),
            SyntaxKind::Percent => OpKind::BinaryOp(BinaryOp::Rem),
            SyntaxKind::Equals => OpKind::BinaryOp(BinaryOp::Assign),
            SyntaxKind::EqEq => OpKind::BinaryOp(BinaryOp::EqEq),
            SyntaxKind::Neq => OpKind::BinaryOp(BinaryOp::Neq),
            SyntaxKind::Ge => OpKind::BinaryOp(BinaryOp::Ge),
            SyntaxKind::Le => OpKind::BinaryOp(BinaryOp::Le),
            SyntaxKind::Gt => OpKind::BinaryOp(BinaryOp::Gt),
            SyntaxKind::Lt => OpKind::BinaryOp(BinaryOp::Lt),
            SyntaxKind::OpenBracket => OpKind::PostfixOp(PostfixOp::Index),
            _ => break,
        };
        let (left_binding_power, right_binding_power) = op.binding_power();
        if left_binding_power < min_binding_power {
            break;
        }
        p.bump();
        let m = lhs.precede(p);
        if matches!(op, OpKind::PostfixOp(PostfixOp::Index)) {
            expr(p, 0);
            p.expect(SyntaxKind::CloseBracket);
            lhs = m.complete(p, SyntaxKind::IndexExpr);
        } else {
            let rhs = expr(p, right_binding_power);
            lhs = m.complete(p, SyntaxKind::BinaryExpr);
            if rhs.is_none() {
                break;
            }
        }
    }
    Some(lhs)
}

pub fn lhs(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    match p.current() {
        SyntaxKind::Number => Some(number_literal(p)),
        SyntaxKind::TrueKw | SyntaxKind::FalseKw => Some(bool_literal(p)),
        SyntaxKind::String => Some(string_literal(p)),
        SyntaxKind::OpenBracket => Some(array_literal(p)),
        SyntaxKind::Minus => Some(prefix_expr(p)),
        SyntaxKind::OpenParen => Some(paren_expr(p)),
        SyntaxKind::OpenBrace => Some(block_expr(p)),
        SyntaxKind::Ident if p.nth_at(1, SyntaxKind::OpenParen) => Some(fn_call_expr(p)),
        SyntaxKind::Ident => Some(ref_expr(p)),
        SyntaxKind::IfKw => Some(if_expr(p)),
        _ => {
            p.error_and_recover(&[SyntaxKind::Number, SyntaxKind::String, SyntaxKind::TrueKw, SyntaxKind::FalseKw, SyntaxKind::OpenBracket, SyntaxKind::Minus, SyntaxKind::OpenParen, SyntaxKind::OpenBrace, SyntaxKind::Ident, SyntaxKind::IfKw], &RECOVERY_SET);
            None
        }
    }
}

pub fn prefix_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::Minus));
    let m = p.start();
    p.bump();
    let op = OpKind::UnaryOp(UnaryOp::Neg);
    let (_, right_binding_power) = op.binding_power();
    expr(p, right_binding_power);
    m.complete(p, SyntaxKind::PrefixExpr)
}

pub fn paren_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::OpenParen));
    let m = p.start();
    p.bump();
    expr(p, 0);
    p.expect(SyntaxKind::CloseParen);
    m.complete(p, SyntaxKind::ParenExpr)
}

pub fn ref_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::Ident));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::RefExpr)
}

pub fn if_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::IfKw));
    let m = p.start();
    p.bump();
    expr(p, 0);
    block_expr(p);
    if p.eat(SyntaxKind::ElseKw) {
        block_expr(p);
    }
    m.complete(p, SyntaxKind::IfExpr)
}

pub fn fn_call_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::Ident));
    let m = p.start();
    p.bump();
    p.expect(SyntaxKind::OpenParen);
    if !p.at(SyntaxKind::CloseParen) {
        expr(p, 0);
        while p.eat(SyntaxKind::Comma) {
            expr(p, 0);
        }
    }
    p.expect(SyntaxKind::CloseParen);
    m.complete(p, SyntaxKind::FnCallExpr)
}

pub fn block_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.expect(SyntaxKind::OpenBrace);
    while !p.at(SyntaxKind::CloseBrace) && !p.at(SyntaxKind::Eof) {
        stmt(p);
    }
    p.expect(SyntaxKind::CloseBrace);
    m.complete(p, SyntaxKind::BlockExpr)
}

pub fn number_literal(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::Number));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::NumberLiteral)
}

pub fn string_literal(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::String));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::StringLiteral)
}

pub fn bool_literal(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at_set(&BOOL_SET));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::BoolLiteral)
}

pub fn array_literal(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::OpenBracket));
    let m = p.start();
    p.bump();
    expr(p, 0);
    p.expect(SyntaxKind::Semicolon);
    expr(p, 0);
    p.expect(SyntaxKind::CloseBracket);
    m.complete(p, SyntaxKind::ArrayLiteral)
}

#[cfg(test)]
mod tests {
    use crate::diagnostic::Diagnostic;
    use crate::language::SyntaxNode;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse(text: &str) -> (SyntaxNode, Vec<Diagnostic>) {
        let lexer = Lexer::new(text);
        let (tokens, _) = lexer.tokenize();
        let parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn multi_stmt() {
        insta::assert_debug_snapshot!(parse("1; 2; 3;"));
    }

    #[test]
    fn func_def() {
        insta::assert_debug_snapshot!(parse("fun f(n: int): int { 1; }"));
    }

    #[test]
    fn let_stmt() {
        insta::assert_debug_snapshot!(parse("let a: int = 1;"));
    }

    #[test]
    fn expr_stmt() {
        insta::assert_debug_snapshot!(parse("1 + 2 * 3;"));
    }

    #[test]
    fn unary() {
        insta::assert_debug_snapshot!(parse("-1 * -2;"));
    }

    #[test]
    fn paren() {
        insta::assert_debug_snapshot!(parse("(1 + 2) * 3;"));
    }

    #[test]
    fn let_array() {
        insta::assert_debug_snapshot!(parse("let a: int[][] = [[1; 2]; 3];"));
    }

    #[test]
    fn ref_expr() {
        insta::assert_debug_snapshot!(parse("a + bc;"));
    }

    #[test]
    fn fn_call_expr() {
        insta::assert_debug_snapshot!(parse("f(a, 1);"));
    }

    #[test]
    fn assign_expr() {
        insta::assert_debug_snapshot!(parse("a = b[0] = 1;"));
    }

    #[test]
    fn block_expr() {
        insta::assert_debug_snapshot!(parse("{1;} + {2;};"));
    }
}
