use rowan::TextRange;

#[derive(Debug)]
pub struct SyntaxError {
    pub message: String,
    pub range: TextRange,
}
