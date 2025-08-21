use crate::token::Token;
use peg::parser;

parser! {
    pub grammar tokenizer() for str {
        rule whitespace() = quiet!{ [' ' | '\t'] }
        rule comment() = quiet!{ ";" [^'\n']* }
        rule newline() = quiet!{ ['\r' | '\n'] }
        rule separator() = quiet!{ (whitespace() / comment() / newline())+ }

        // --- Token Rules ---

        rule integer() -> Token
            = "0x" s:$(HEX_DIGIT()+) { Token::Integer(i64::from_str_radix(s, 16).unwrap()) }
            / s:$(DIGIT()+) { Token::Integer(s.parse().unwrap()) }

        rule HEX_DIGIT() = ['0'..='9' | 'a'..='f' | 'A'..='F']
        rule DIGIT() = ['0'..='9']

        rule register() -> Token = s:$(REGISTER_NAME()) { Token::Register(s.to_string()) }

        rule REGISTER_NAME() =
            "eax" / "ebx" / "ecx" / "edx" / "esp" / "ebp" / "esi" / "edi" /
            "rax" / "rbx" / "rcx" / "rdx" / "rsp" / "rbp" / "rsi" / "rdi" /
            "r" DIGIT()+

        rule string() -> Token
            = "\"" s:(string_character())* "\"" { Token::String(s.into_iter().collect()) }

        rule string_character() -> char
            = !['"' | '\\'] c:[_] { c }
            / "\\" c:escape_sequence() { c }

        rule escape_sequence() -> char
            = "\"" { '\"' }
            / "\\" { '\\' }
            / "n" { '\n' }
            / "t" { '\t' }
            / "r" { '\r' }

        rule identifier() -> Token = s:$(IDENTIFIER_START() (IDENTIFIER_PART())*) {
            Token::Identifier(s.to_string())
        }

        rule IDENTIFIER_START() = ['a'..='z' | 'A'..='Z' | '_']
        rule IDENTIFIER_PART() = IDENTIFIER_START() / DIGIT()

        rule punctuation() -> Token
            = ":" { Token::Colon }
            / "," { Token::Comma }
            / "." { Token::Dot }
            / "[" { Token::LBracket }
            / "]" { Token::RBracket }
            / "+" { Token::Plus }
            / "-" { Token::Minus }

        rule a_token() -> Token
            = integer()
            / register()
            / string()
            / identifier()
            / punctuation()

        rule token_and_sep() -> Token = t:a_token() separator()? { t }

        pub rule tokenize() -> Vec<Token> =
            separator()? tokens:token_and_sep()* {
                let mut ts = tokens;
                ts.push(Token::Eof);
                ts
            }
    }
}

/// The public interface to the tokenizer.
pub fn tokenize(input: &str) -> Result<Vec<Token>, peg::error::ParseError<peg::str::LineCol>> {
    tokenizer::tokenize(input)
}
