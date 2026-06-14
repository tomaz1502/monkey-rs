use crate::mods::lib::utils::unescape;

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum Token {
    // Identifiers and literals
    Identifier(String),
    IntLit(i64),
    CharLit(char),
    StrLit(String),
    Unit,

    // Delimeters
    LPar,
    RPar,
    LSqBrack,
    RSqBrack,
    LBrack,
    RBrack,
    Semicolon,
    Comma,
    Colon,

    // Operators (also delimiters)
    Plus,
    Assign,
    Minus,
    Bang,
    Mult,
    Slash,
    Modulus,
    LT,
    GT,
    Eq,
    Neq,

    // Keywords
    Let,
    Fn,
    Return,
    If,
    Else,
    True,
    False,

    // Types
    IntType,
    BoolType,
    CharType,
    StrType,
    UnitType,
    ArrowType,

    // EOF
    Eof
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LexErrorKind {
    UnrecognizedToken,
    SingleQuoteString,
    UnclosedQuote,
    UnclosedDoubleQuote,
}

use LexErrorKind::*;

pub struct LexError {
    kind: LexErrorKind,
    line: u32,
    col: u32,
}

impl LexError {
    pub fn kind(&self) -> LexErrorKind {
        self.kind.clone()
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn col(&self) -> u32 {
        self.col
    }
}

use Token::*;

pub struct Lexer {
    input: Vec<u8>,
    ptr: usize,
    line: u32,
    col: u32,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer { input: input.as_bytes().to_vec(), ptr: 0, line: 1, col: 1, }
    }

    pub fn mk_error(&self, kind: LexErrorKind) -> LexError {
        LexError { kind, line: self.line, col: self.col }
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn col(&self) -> u32 {
        self.col
    }

    fn is_space(byt: u8) -> bool {
        byt == b' ' || byt == b'\n' || byt == b'\t'
    }

    fn peek(&self) -> Option<char> {
        if self.ptr == self.input.len() {
            None
        } else {
            Some(self.input[self.ptr] as char)
        }
    }

    fn read_char(&mut self) -> Option<char> {
        if self.ptr == self.input.len() {
            None
        } else {
            let ch = self.input[self.ptr] as char;
            self.col += 1;
            if ch == '\n' {
                self.line += 1;
                self.col = 1;
            }
            self.ptr += 1;
            Some(ch)
        }
    }

    fn read_while(&mut self, pred: fn(u8) -> bool) -> String {
        let mut bytes = vec![];
        while self.ptr < self.input.len() && pred(self.input[self.ptr]) {
            let ch = self.input[self.ptr];
            bytes.push(ch);
            self.col += 1;
            if ch == b'\n' {
                self.line += 1;
                self.col = 1;
            }
            self.ptr += 1;
        }
        let escaped_tok = String::from_utf8(bytes).unwrap();
        unescape(&escaped_tok)
    }

    fn get_next_aux(&mut self) -> Result<Token, LexError> {
        while self.ptr < self.input.len() && Self::is_space(self.input[self.ptr]) {
            self.col += 1;
            if self.input[self.ptr] == b'\n' {
                self.line += 1;
                self.col = 1;
            }
            self.ptr += 1;
        }
        let ch = match self.read_char() {
            None => return Ok(Eof),
            Some(ch) => ch,
        };

        match ch {
            ':' => Ok(Colon),
            ';' => Ok(Semicolon),
            ',' => Ok(Comma),
            '(' => Ok(LPar),
            ')' => Ok(RPar),
            '[' => Ok(LSqBrack),
            ']' => Ok(RSqBrack),
            '{' => Ok(LBrack),
            '}' => Ok(RBrack),
            '+' => Ok(Plus),
            '=' => {
                if self.peek() == Some('=') {
                    self.read_char();
                    Ok(Eq)
                } else {
                    Ok(Assign)
                }
            }
            '-' => {
                if self.peek() == Some('>') {
                    self.read_char();
                    Ok(ArrowType)
                } else {
                    Ok(Minus)
                }
            }
            '*' => Ok(Mult),
            '<' => Ok(LT),
            '>' => Ok(GT),
            '!' => {
                if self.peek() == Some('=') {
                    self.read_char();
                    Ok(Neq)
                } else {
                    Ok(Bang)
                }
            },
            '/' => Ok(Slash),
            '%' => Ok(Modulus),
            // TODO: Don't accept line break inside single or double quote
            '\'' => {
                // TODO: `c` must be escaped if necessary
                let c = self.read_char().ok_or(self.mk_error(UnclosedQuote))?;
                let q = self.read_char().ok_or(self.mk_error(UnclosedQuote))?;
                if q != '\'' {
                    Err(self.mk_error(SingleQuoteString))
                } else {
                    Ok(CharLit(c))
                }
            }
            '\"' => {
                let s = self.read_while(|b: u8| { b != b'\"' });
                let _ = self.read_char().ok_or(self.mk_error(UnclosedDoubleQuote))?;
                Ok(StrLit(s))
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let rest = self.read_while(|b: u8| { b.is_ascii_alphanumeric() || b == b'_' });
                let word = String::from(ch) + &rest;
                match word.as_str() {
                    "let"    => Ok(Let),
                    "fn"     => Ok(Fn),
                    "true"   => Ok(True),
                    "false"  => Ok(False),
                    "return" => Ok(Return),
                    "if"     => Ok(If),
                    "else"   => Ok(Else),
                    "int"    => Ok(IntType),
                    "bool"   => Ok(BoolType),
                    "char"   => Ok(CharType),
                    "string" => Ok(StrType),
                    "unit"   => Ok(UnitType),
                    "uu"     => Ok(Unit),
                    _        => Ok(Identifier(word))
                }
            }
            '0'..='9' => {
                let rest = self.read_while(|b| { (b as char).is_numeric() });
                let num_str = String::from(ch) + &rest;
                match num_str.parse::<i64>() {
                    Ok(num) => Ok(IntLit(num)),
                    Err(_) => Err(self.mk_error(UnrecognizedToken)) // TODO: Create a token error for this
                }
            }
            _ => Err(self.mk_error(UnrecognizedToken)),
        }
    }

    // TODO: Iterator trait?
    pub fn get_next_token(&mut self) -> Result<Token, LexError> {
        self.get_next_aux()
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use super::Token::*;
    use super::LexErrorKind::*;

    #[test]
    fn tokenize_simple_program() {
        let program = "
            let dummy = 'a';
            let dummy_str = \"foo\";
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y * 3;
            };
            let result = add(five, ten);";
        let mut tkn = Lexer::new(program.to_string());
        let mut tokens = vec![];
        loop {
            match tkn.get_next_token() {
                Ok(Eof) => { break; }
                Ok(tk)  => { tokens.push(tk); }
                Err(lex_err) =>
                    match lex_err.kind {
                        UnrecognizedToken   => panic!("Unexpected error in tokenizer"),
                        SingleQuoteString   => panic!("Strings cannot be enclosed by a single quote"),
                        UnclosedQuote       => panic!("Unclosed quote"),
                        UnclosedDoubleQuote => panic!("Unclosed double quote"),
                    }
            }
        }
        assert!(tokens ==
          [Let, Identifier("dummy".to_string()), Assign, CharLit('a'), Semicolon,
           Let, Identifier("dummy_str".to_string()), Assign, StrLit("foo".to_string()), Semicolon,
           Let, Identifier("five".to_string()), Assign, IntLit(5), Semicolon, Let,
           Identifier("ten".to_string()), Assign, IntLit(10), Semicolon, Let,
           Identifier("add".to_string()), Assign, Fn, LPar, Identifier("x".to_string()),
           Comma, Identifier("y".to_string()), RPar, LBrack, Identifier("x".to_string()), Plus,
           Identifier("y".to_string()), Mult, IntLit(3), Semicolon, RBrack, Semicolon, Let,
           Identifier("result".to_string()), Assign, Identifier("add".to_string()),
           LPar, Identifier("five".to_string()), Comma, Identifier("ten".to_string()),
           RPar, Semicolon
          ]);
    }
}
