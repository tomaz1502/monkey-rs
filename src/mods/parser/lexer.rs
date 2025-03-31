use std::hash::Hasher;

#[derive(Debug)]
#[repr(u8)]
pub enum Token {
    // Identifiers and literals
    Id(String),
    Integer(i64),

    // Delimeters
    LPar,
    RPar,
    LBrack,
    RBrack,
    Semicolon,
    Comma,

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

    // EOF
    Eof
}

impl Token {
    fn discriminant(&self) -> u8
    {
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }
}

// we can't derive because we need Id(_) = Id(_) and Integer(_) = Integer(_) for the hashmap
impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool
    {
        self.discriminant() == other.discriminant()
    }
}

impl std::cmp::Eq for Token {}

impl std::hash::Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H)
    {
        state.write_u8(self.discriminant());
    }
}

#[derive(PartialEq, Debug)]
pub enum LexError { UnrecognizedToken }

use Token::*;
use LexError::*;

pub struct Lexer {
    input: Vec<u8>,
    ptr: usize
}

impl Lexer {
    pub fn new(input: String) -> Self
    {
        Lexer { input: input.as_bytes().to_vec(), ptr: 0 }
    }

    fn is_space(byt: u8) -> bool
    {
        return byt == b' ' || byt == b'\n' || byt == b'\t';
    }

    fn peek(input: &Vec<u8>, ptr: &usize) -> Option<char> {
        if *ptr == input.len() {
            None
        } else {
            Some(input[*ptr] as char)
        }
    }
    fn read_char(input: &Vec<u8>, ptr: &mut usize) -> Option<char> {
        if *ptr == input.len() {
            None
        } else {
            let ch = input[*ptr] as char;
            *ptr += 1;
            Some(ch)
        }
    }

    fn read_while(input: &Vec<u8>, ptr: &mut usize, pred: fn(u8) -> bool) -> String
    {
        let mut tok = String::from("");
        while *ptr < input.len() && pred(input[*ptr]) {
            tok.push(input[*ptr] as char);
            *ptr += 1;
        }
        tok
    }

    fn get_next_aux(input: &Vec<u8>, ptr: &mut usize) -> Result<Token, LexError>
    {
        while *ptr < input.len() && Self::is_space(input[*ptr]) {
            *ptr += 1;
        }
        if *ptr == input.len() {
            return Ok(Eof);
        }

        let ch = Self::read_char(input, ptr).unwrap();
        match ch {
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
            '-' => Ok(Minus),
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
                    _        => Ok(Id(word))
                }
            }
            '1'..='9' => {
                let rest = Self::read_while(input, ptr, |b| { (b as char).is_numeric() });
                let num_str = String::from(ch) + &rest;
                match num_str.parse::<i64>() {
                    Ok(num) => Ok(Integer(num)),
                    Err(_) => Err(UnrecognizedToken) // TODO: Create a token error for this
                }
            }
            _ => Err(UnrecognizedToken),
        }
    }

    pub fn get_next_token(&mut self) -> Result<Token, LexError>
    {
        Self::get_next_aux(&self.input, &mut self.ptr)
    }

    pub fn peek_token(&self) -> Result<Token, LexError>
    {
        let mut p = self.ptr;
        Self::get_next_aux(&self.input, &mut p)
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use super::Token::*;
    use super::LexError::*;

    #[test]
    fn tokenize_simple_program()
    {
        let program = "
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
                Err(UnrecognizedToken) => { panic!("Unexpected error in tokenizer"); }
            }
        }
        println!("{:?}", tokens);
        assert!(tokens ==
          [Let, Id("five".to_string()), Assign, Integer(5), Semicolon, Let,
           Id("ten".to_string()), Assign, Integer(10), Semicolon, Let,
           Id("add".to_string()), Assign, Fn, LPar, Id("x".to_string()),
           Comma, Id("y".to_string()), RPar, LBrack, Id("x".to_string()), Plus,
           Id("y".to_string()), Mult, Integer(3), Semicolon, RBrack, Semicolon, Let,
           Id("result".to_string()), Assign, Id("add".to_string()),
           LPar, Id("five".to_string()), Comma, Id("ten".to_string()),
           RPar, Semicolon
          ]);
    }
}
