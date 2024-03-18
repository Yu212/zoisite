use crate::hir::{BinaryOp, UnaryOp};
use crate::parser::{CompletedMarker, Parser};
use crate::syntax_kind::SyntaxKind;

pub fn root(p: &mut Parser<'_>) {
    let m = p.start();
    p.eat_trivia();
    expr(p, 0);
    m.complete(p, SyntaxKind::Root);
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
    if !p.expect_set(&[SyntaxKind::Number, SyntaxKind::Minus, SyntaxKind::OpenParen]) {
        return None
    }
    match p.current() {
        SyntaxKind::Number => Some(number(p)),
        SyntaxKind::Minus => Some(prefix_expr(p)),
        SyntaxKind::OpenParen => Some(paren_expr(p)),
        _ => unreachable!()
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
    p.expect_eat(SyntaxKind::CloseParen);
    m.complete(p, SyntaxKind::ParenExpr)
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
    fn expr() {
        insta::assert_debug_snapshot!(parse("1 + 2 * 3"));
    }

    #[test]
    fn unary() {
        insta::assert_debug_snapshot!(parse("-1 * -2"));
    }

    #[test]
    fn paren() {
        insta::assert_debug_snapshot!(parse("(1 + 2) * 3"));
    }
}
