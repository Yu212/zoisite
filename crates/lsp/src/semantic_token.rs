use crate::backend::text_range_to_range;
use ropey::Rope;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};
use zoisite::language::SyntaxToken;
use zoisite::syntax_kind::SyntaxKind;

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
];

pub fn get_semantic_tokens(tokens: Vec<SyntaxToken>, rope: &Rope) -> Vec<SemanticToken> {
    let mut prev_line = 0;
    let mut prev_start = 0;
    tokens.iter().flat_map(|token| {
        let semantic_token_type = match token.kind() {
            SyntaxKind::LineComment => SemanticTokenType::COMMENT,
            SyntaxKind::String => SemanticTokenType::STRING,
            SyntaxKind::LetKw => SemanticTokenType::KEYWORD,
            SyntaxKind::IfKw => SemanticTokenType::KEYWORD,
            SyntaxKind::ElseKw => SemanticTokenType::KEYWORD,
            SyntaxKind::WhileKw => SemanticTokenType::KEYWORD,
            SyntaxKind::BreakKw => SemanticTokenType::KEYWORD,
            SyntaxKind::ContinueKw => SemanticTokenType::KEYWORD,
            SyntaxKind::FunKw => SemanticTokenType::KEYWORD,
            SyntaxKind::TrueKw => SemanticTokenType::KEYWORD,
            SyntaxKind::FalseKw => SemanticTokenType::KEYWORD,
            SyntaxKind::Number => SemanticTokenType::NUMBER,
            _ => return None,
        };
        let range = text_range_to_range(token.text_range(), rope);
        let delta_line = range.start.line - prev_line;
        let delta_start = if delta_line == 0 { range.start.character - prev_start } else { range.start.character };
        prev_line = range.start.line;
        prev_start = range.start.character;
        Some(SemanticToken {
            delta_line,
            delta_start,
            length: token.text_range().len().into(),
            token_type: LEGEND_TYPE.iter().position(|ty| ty == &semantic_token_type).unwrap() as u32,
            token_modifiers_bitset: 0,
        })
    }).collect()
}
