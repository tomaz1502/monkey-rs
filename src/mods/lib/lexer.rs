use std::hash::Hasher;

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Token {
    // Identifiers and literals
    Identifier(String),
    IntLit(i64),
    CharLit(char),
    StrLit(String),

    // Delimeters
    LPar,
    RPar,
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

// NOTE: why aren't you using self?
impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer { input: input.as_bytes().to_vec(), ptr: 0 }
    }

    fn is_space(byt: u8) -> bool {
        byt == b' ' || byt == b'\n' || byt == b'\t'
    }

    fn peek(input: &[u8], ptr: &usize) -> Option<char> {
        if *ptr == input.len() {
            None
        } else {
            Some(input[*ptr] as char)
        }
    }

    fn read_char(input: &[u8], ptr: &mut usize) -> Option<char> {
        if *ptr == input.len() {
            None
        } else {
            let ch = input[*ptr] as char;
            *ptr += 1;
            Some(ch)
        }
    }

    fn read_while(input: &[u8], ptr: &mut usize, pred: fn(u8) -> bool) -> String {
        let mut tok = String::from("");
        while *ptr < input.len() && pred(input[*ptr]) {
            tok.push(input[*ptr] as char);
            *ptr += 1;
        }
        tok
    }

    fn get_next_aux(input: &[u8], ptr: &mut usize) -> Result<Token, LexError> {
        while *ptr < input.len() && Self::is_space(input[*ptr]) {
            *ptr += 1;
        }

        let ch = match Self::read_char(input, ptr) {
            None => return Ok(Eof),
            Some(ch) => ch
        };

        match ch {
            ':' => Ok(Colon),
            ';' => Ok(Semicolon),
            ',' => Ok(Comma),
            '(' => Ok(LPar),
            ')' => Ok(RPar),
            '{' => Ok(LBrack),
            '}' => Ok(RBrack),
            '+' => Ok(Plus),
            '=' => {
                if Self::peek(input, ptr) == Some('=') {
                    Self::read_char(input, ptr);
                    Ok(Eq)
                } else {
                    Ok(Assign)
                }
            }
            '-' => {
                if Self::peek(input, ptr) == Some('>') {
                    Self::read_char(input, ptr);
                    Ok(ArrowType)
                } else {
                    Ok(Minus)
                }
            }
            '*' => Ok(Mult),
            '<' => Ok(LT),
            '>' => Ok(GT),
            '!' => {
                if Self::peek(input, ptr) == Some('=') {
                    Self::read_char(input, ptr);
                    Ok(Neq)
                } else {
                    Ok(Bang)
                }
            },
            '/' => Ok(Slash),
            // TODO: Don't accept line break inside single or double quote
            '\'' => {
                // TODO: `c` must be escaped if necessary
                let c = Self::read_char(input, ptr).ok_or(UnclosedQuote)?;
                let q = Self::read_char(input, ptr).ok_or(UnclosedQuote)?;
                if q != '\'' {
                    Err(SingleQuoteString)
                } else {
                    Ok(CharLit(c))
                }
            }
            '\"' => {
                let s = Self::read_while(input, ptr, |b: u8| { b != b'\"' });
                let _ = Self::read_char(input, ptr);
                Ok(StrLit(s))
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let rest = Self::read_while(input, ptr, |b: u8| { b.is_ascii_alphanumeric() || b == b'_' });
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
                    _        => Ok(Identifier(word))
                }
            }
            '0'..='9' => {
                let rest = Self::read_while(input, ptr, |b| { (b as char).is_numeric() });
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
        Self::get_next_aux(&self.input, &mut self.ptr)
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
