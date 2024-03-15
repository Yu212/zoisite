use crate::hir::BinaryOp;
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
    if p.expect(SyntaxKind::Number) {
        Some(number(p))
    } else {
        None
    }
}

pub fn number(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    if p.expect(SyntaxKind::Number) {
        p.bump();
    }
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
}
