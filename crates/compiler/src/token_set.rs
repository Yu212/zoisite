use crate::syntax_kind::SyntaxKind;

#[derive(Clone, Copy)]
pub struct TokenSet(u128);

impl TokenSet {
    pub const EMPTY: TokenSet = TokenSet(0);
    pub const ALL: TokenSet = TokenSet(!0);

    pub const fn new(kinds: &[SyntaxKind]) -> Self {
        let mut mask = 0_u128;
        let mut i = 0;
        while i < kinds.len() {
            mask |= 1_u128 << (kinds[i] as u16);
            i += 1;
        }
        TokenSet(mask)
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn contains(self, kind: SyntaxKind) -> bool {
        self.0 >> (kind as u16) & 1 == 1
    }
}
