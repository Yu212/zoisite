use crate::hir::{BinaryOp, UnaryOp};
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::SyntaxKind;
use crate::token_set::TokenSet;

const RECOVERY_SET: TokenSet = TokenSet::new(&[SyntaxKind::Semicolon]);

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
        _ => expr_stmt(p),
    }
}

pub fn let_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::LetKw));
    let m = p.start();
    p.bump();
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Equals);
    expr(p, 0);
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::LetStmt)
}

pub fn while_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::WhileKw));
    let m = p.start();
    p.bump();
    expr(p, 0);
    block_expr(p);
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

pub fn expr(p: &mut Parser<'_>, min_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = lhs(p)?;
    loop {
        let op = match p.current() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Minus => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            SyntaxKind::Percent => BinaryOp::Rem,
            SyntaxKind::Equals => BinaryOp::Assign,
            _ => break,
        };
        let (left_binding_power, right_binding_power) = op.binding_power();
        if left_binding_power < min_binding_power {
            break;
        }
        p.bump();
        let m = lhs.precede(p);
        let rhs = expr(p, right_binding_power);
        lhs = m.complete(p, SyntaxKind::BinaryExpr);
        if rhs.is_none() {
            break;
        }
    }
    Some(lhs)
}

pub fn lhs(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    match p.current() {
        SyntaxKind::Number => Some(number(p)),
        SyntaxKind::Minus => Some(prefix_expr(p)),
        SyntaxKind::OpenParen => Some(paren_expr(p)),
        SyntaxKind::OpenBrace => Some(block_expr(p)),
        SyntaxKind::Ident if p.nth_at(1, SyntaxKind::OpenParen) => Some(fn_call_expr(p)),
        SyntaxKind::Ident => Some(ref_expr(p)),
        SyntaxKind::IfKw => Some(if_expr(p)),
        _ => {
            p.error_and_recover(&[SyntaxKind::Number, SyntaxKind::Minus, SyntaxKind::OpenParen, SyntaxKind::OpenBrace, SyntaxKind::Ident], &RECOVERY_SET);
            None
        }
    }
}

pub fn prefix_expr(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::Minus));
    let m = p.start();
    p.bump();
    let op = UnaryOp::Neg;
    let ((), right_binding_power) = op.binding_power();
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

pub fn number(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at(SyntaxKind::Number));
    let m = p.start();
    p.bump();
    m.complete(p, SyntaxKind::Literal)
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
    fn let_stmt() {
        insta::assert_debug_snapshot!(parse("let a = 1;"));
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
    fn ref_expr() {
        insta::assert_debug_snapshot!(parse("a + bc;"));
    }

    #[test]
    fn fn_call_expr() {
        insta::assert_debug_snapshot!(parse("f(a, 1);"));
    }

    #[test]
    fn block_expr() {
        insta::assert_debug_snapshot!(parse("{1;} + {2;};"));
    }
}
