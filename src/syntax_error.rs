use rowan::TextRange;

pub struct SyntaxError {
    pub message: String,
    pub range: TextRange,
}
