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

    // Keywords
    Let,
    Fn,

    // Operators
    Plus,
    Equals,
}

#[derive(PartialEq)]
pub enum TknError { UnrecognizedToken, Eof }

use Token::*;
use TknError::*;

pub struct Tokenizer {
    input: Vec<u8>,
    ptr: usize
}

impl Tokenizer {
    pub fn new(input: String) -> Self {
        Tokenizer { input: input.as_bytes().to_vec(), ptr: 0 }
    }

    fn is_space(byt: u8) -> bool {
        return byt == b' ' || byt == b'\n' || byt == b'\t';
    }

    fn get_delimiter(byt: u8) -> Option<Token> {
        match byt {
            b';' => Some(Semicolon),
            b',' => Some(Comma),
            b'(' => Some(LPar),
            b')' => Some(RPar),
            b'{' => Some(LBrack),
            b'}' => Some(RBrack),
            b'+' => Some(Plus),
            b'=' => Some(Equals),
            _ => None
        }
    }

    fn should_stop_lexing(byt: u8) -> bool {
        Option::is_some(&Self::get_delimiter(byt)) || Self::is_space(byt)
    }

    fn is_valid_id(token: &String) -> bool {
        let bytes = token.as_bytes();
        let first_char = bytes[0] as char;
        first_char.is_alphabetic() || first_char == '_'
    }

    fn is_integer(token: &String) -> bool {
        let mut it = token.chars().into_iter();
        let first_char = it.next().unwrap();
        let b = it.all(|c| { c.is_numeric() });
        b && (first_char.is_numeric() || first_char == '-')
    }

    pub fn get_next(&mut self) -> Result<Token, TknError> {
        while self.ptr < self.input.len() && Self::is_space(self.input[self.ptr]) {
            self.ptr += 1;
        }
        if self.ptr == self.input.len() {
            return Err(Eof);
        }
        if let Some(tk) = Self::get_delimiter(self.input[self.ptr]) {
            self.ptr += 1;
            return Ok(tk);
        }
        let mut token: String = String::from(self.input[self.ptr] as char);
        self.ptr += 1;
        while self.ptr < self.input.len() && !Self::should_stop_lexing(self.input[self.ptr]) {
            token.push(self.input[self.ptr] as char);
            self.ptr += 1;
        }
        match token.as_str() {
            "let" => Ok(Let),
            "fn"  => Ok(Fn),
            _     => {
                if Self::is_valid_id(&token) {
                    Ok(Id(token))
                } else if Self::is_integer(&token) {
                    match token.parse::<i64>() {
                        Ok(num) => Ok(Integer(num)),
                        _ => { unreachable!() } // It was already checked that every char in token
                                                // is a number
                    }
                } else {
                    Err(UnrecognizedToken)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Tokenizer;
    use crate::lexer::Token::*;
    use crate::lexer::TknError::*;
    #[test]
    fn tokenize_simple_program() {
        let program = "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
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
        assert!(tokens ==
          [Let, Id("five".to_string()), Equals, Integer(5), Semicolon, Let,
           Id("ten".to_string()), Equals, Integer(10), Semicolon, Let,
           Id("add".to_string()), Equals, Fn, LPar, Id("x".to_string()),
           Comma, Id("y".to_string()), RPar, LBrack, Id("x".to_string()), Plus,
           Id("y".to_string()), Semicolon, RBrack, Semicolon, Let,
           Id("result".to_string()), Equals, Id("add".to_string()),
           LPar, Id("five".to_string()), Comma, Id("ten".to_string()),
           RPar, Semicolon
          ]);
    }
}
