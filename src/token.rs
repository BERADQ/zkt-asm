#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // End-of-file marker
    Eof,

    // --- Operands
    /// An integer literal, like `10` or `0xFF`
    Integer(i64),
    /// A register, like `eax` or `r1`
    Register(String),
    /// An identifier, could be an instruction or a label
    Identifier(String),
    /// A string literal, like `"hello"`
    String(String),

    // --- Punctuation
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `+`
    Plus,
    /// `-`
    Minus,
}
