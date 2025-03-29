use std::ops::Fn;

#[derive(PartialEq, Debug)]
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
}

#[derive(PartialEq, Debug)]
pub enum TknError { UnrecognizedToken, Eof }

use Token::*;
use TknError::*;

pub struct Tokenizer {
    input: Vec<u8>,
    ptr: usize
}

impl Tokenizer {
    pub fn new(input: String) -> Self
    {
        Tokenizer { input: input.as_bytes().to_vec(), ptr: 0 }
    }

    fn is_space(byt: u8) -> bool
    {
        return byt == b' ' || byt == b'\n' || byt == b'\t';
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

    fn read_while<F>(&mut self, pred: F) -> String
        where F: Fn(u8) -> bool
    {
        let mut tok = String::from("");
        while self.ptr < self.input.len() && pred(self.input[self.ptr]) {
            tok.push(self.input[self.ptr] as char);
            self.ptr += 1;
        }
        tok
    }

    pub fn get_next(&mut self) -> Result<Token, TknError>
    {
        while self.ptr < self.input.len() && Self::is_space(self.input[self.ptr]) {
            self.ptr += 1;
        }
        if self.ptr == self.input.len() {
            return Err(Eof);
        }

        let ch = self.read_char().unwrap();
        match ch {
            ';' => Ok(Semicolon),
            ',' => Ok(Comma),
            '(' => Ok(LPar),
            ')' => Ok(RPar),
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
            '-' => Ok(Minus),
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
            'a'..='z' | 'A'..='Z' | '_' => {
                let rest = self.read_while(|b| { b.is_ascii_alphanumeric() || b == b'_' });
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
                let rest = self.read_while(|b| { (b as char).is_numeric() });
                let num_str = String::from(ch) + &rest;
                match num_str.parse::<i64>() {
                    Ok(num) => Ok(Integer(num)),
                    Err(_) => Err(UnrecognizedToken) // TODO: Create a token error for this
                }
            }
            _ => Err(UnrecognizedToken),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Tokenizer;
    use crate::lexer::Token::*;
    use crate::lexer::TknError::*;

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
        let mut tkn = Tokenizer::new(program.to_string());
        let mut tokens = vec![];
        loop {
            match tkn.get_next() {
                Ok(tk) => { tokens.push(tk); }
                Err(Eof) => { break; }
                Err(_) => { panic!("Unexpected error in tokenizer"); }
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
