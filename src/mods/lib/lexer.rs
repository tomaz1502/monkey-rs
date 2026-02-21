use crate::mods::lib::utils::unescape;
use std::hash::Hasher;

#[derive(Debug, Clone)]
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

impl Token {
    fn discriminant(&self) -> u8 {
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }
}

// we can't derive because we need Id(_) = Id(_) and Integer(_) = Integer(_) for the hashmap
// note that this is effectively changing the behaviour of `t1 == t2` for tokens, but we
// only compare tokens for their discriminant in the parser. but maybe we should take more
// care?
impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.discriminant() == other.discriminant()
    }
}

impl std::cmp::Eq for Token {}

impl std::hash::Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u8(self.discriminant());
    }
}

#[derive(PartialEq, Debug)]
pub enum LexError { UnrecognizedToken, SingleQuoteString, UnclosedQuote }

use Token::*;
use LexError::*;

pub struct Lexer {
    input: Vec<u8>,
    ptr: usize
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer { input: input.as_bytes().to_vec(), ptr: 0 }
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
            self.ptr += 1;
            Some(ch)
        }
    }

    fn read_while(&mut self, pred: fn(u8) -> bool) -> String {
        let mut bytes = vec![];
        while self.ptr < self.input.len() && pred(self.input[self.ptr]) {
            bytes.push(self.input[self.ptr]);
            self.ptr += 1;
        }
        let escaped_tok = String::from_utf8(bytes).unwrap();
        let tok = unescape(&escaped_tok);
        tok
    }

    fn get_next_aux(&mut self) -> Result<Token, LexError> {
        while self.ptr < self.input.len() && Self::is_space(self.input[self.ptr]) {
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
                let c = self.read_char().ok_or(UnclosedQuote)?;
                let q = self.read_char().ok_or(UnclosedQuote)?;
                if q != '\'' {
                    Err(SingleQuoteString)
                } else {
                    Ok(CharLit(c))
                }
            }
            '\"' => {
                let s = self.read_while(|b: u8| { b != b'\"' });
                let _ = self.read_char();
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
                    Err(_) => Err(UnrecognizedToken) // TODO: Create a token error for this
                }
            }
            _ => Err(UnrecognizedToken),
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
    use super::LexError::*;

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
                Err(UnrecognizedToken) => panic!("Unexpected error in tokenizer"),
                Err(SingleQuoteString) => panic!("Strings cannot be enclosed by a single quote"),
                Err(UnclosedQuote)     => panic!("Unclosed quote"),
            }
        }
        println!("{:?}", tokens);
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
