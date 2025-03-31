use std::collections::HashMap;

use crate::mods::parser::lexer;

use lexer::Token::*;
use lexer::LexError;

#[derive(PartialEq, Eq)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

impl Precedence {
    fn token_precedence(tk: lexer::Token) -> Self
    {
        match tk {
            Eq    => Precedence::EQUALS,
            Neq   => Precedence::EQUALS,
            LT    => Precedence::LESSGREATER,
            GT    => Precedence::LESSGREATER,
            Plus  => Precedence::SUM,
            Minus => Precedence::SUM,
            Mult  => Precedence::PRODUCT,
            Slash => Precedence::PRODUCT,
            _     => Precedence::LOWEST
        }
    }
}

type Id = String;

#[derive(PartialEq)]
enum Expr {
    Ident(Id),
    Integer(i64),
    PrefixExpr(Box<Expr>),
    InfixExpr(Box<Expr>, Box<Expr>)
}

#[derive(PartialEq)]
enum Stmt {
    LetStmt(Id, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr)
}

struct Program {
    stmts: Vec<Stmt>
}

enum ParseError { UnrecognizedToken, UnexpectedToken }

struct Parser {
    curr_token: lexer::Token,
    lexer: lexer::Lexer,

    prefix_parse_fn: HashMap<lexer::Token, for<'a> fn(&'a Parser) -> Result<Expr, ParseError>>,
    infix_parse_fn: HashMap<lexer::Token, for<'a> fn(&'a Parser, Expr) -> Result<Expr, ParseError>>,
}

impl Parser {
    fn new(source_code: String) -> Result<Self, ParseError> {
        let mut lexer = lexer::Lexer::new(source_code);
        match lexer.get_next_token() {
            Ok(curr_token) => {
                Ok(Parser { curr_token, lexer, prefix_parse_fn: HashMap::new(), infix_parse_fn: HashMap::new() })
            }
            Err(LexError::UnrecognizedToken) => Err(ParseError::UnrecognizedToken)
        }
    }

    fn initialize(source_code: String) -> Result<Self, ParseError>
    {
        let mut parser = Self::new(source_code)?;
        parser.register_prefix_fn(lexer::Token::Id("".to_string()), Self::parse_id_to_expr);
        parser.register_prefix_fn(lexer::Token::Integer(42), Self::parse_integer_to_expr);
        Ok(parser)
    }

    fn register_prefix_fn(&mut self, t: lexer::Token, f: for<'a> fn(&'a Parser) -> Result<Expr, ParseError>) -> ()
    {
        self.prefix_parse_fn.insert(t, f);
    }

    fn register_infix_fn(&mut self, t: lexer::Token, f: for<'a> fn(&'a Parser, Expr) -> Result<Expr, ParseError>) -> ()
    {
        self.infix_parse_fn.insert(t, f);
    }

    fn parse_integer_to_expr(&self) -> Result<Expr, ParseError>
    {
        match self.curr_token {
            Integer(num) => Ok(Expr::Integer(num)),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_id_to_expr(&self) -> Result<Expr, ParseError>
    {
        match &self.curr_token {
            Id(name) => Ok(Expr::Ident(name.clone())),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn advance_token(&mut self) -> Result<(), ParseError>
    {
        match self.lexer.get_next_token() {
            Ok(tkn) => { self.curr_token = tkn; Ok(()) }
            Err(LexError::UnrecognizedToken) => Err(ParseError::UnrecognizedToken)
        }
    }

    fn parse_id(&self) -> Result<String, ParseError>
    {
        match &self.curr_token {
            Id(name) => Ok(name.clone()),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError>
    {
        todo!()
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError>
    {
        let expr = self.parse_expr()?;
        Ok(Stmt::ExprStmt(expr))
    }

    // Should update the lexer until the first token after the let expression (after the semicolon)
    fn parse_let(&mut self) -> Result<Stmt, ParseError>
    {
        self.advance_token()?;

        let id = self.parse_id()?;

        self.advance_token()?; // Equal sign
        if self.curr_token != Assign {
            return Err(ParseError::UnexpectedToken);
        }

        self.advance_token()?;

        let expr = self.parse_expr()?;

        self.advance_token()?; // semicolon
        if self.curr_token != Semicolon {
            return Err(ParseError::UnexpectedToken);
        }

        self.advance_token()?;

        Ok(Stmt::LetStmt(id, expr))
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseError>
    {
        self.advance_token()?;

        let expr = self.parse_expr()?;

        self.advance_token()?;
        if self.curr_token != Semicolon {
            return Err(ParseError::UnexpectedToken);
        }

        self.advance_token()?;

        Ok(Stmt::ReturnStmt(expr))
    }

    // I don't like the way this is structured. Parser should be something that we pass a string
    // and it returns the AST. Instead here its an object that holds a string and eventually in the
    // future we ask it to "parse" that string. It's weird.
    // TODO: This way we stop after the first error. What if we want to continue?
    fn parse_program(&mut self) -> Result<Program, ParseError>
    {
        let mut prog = Program { stmts: vec![] };
        loop {
            match self.curr_token {
                Let => { let stmt = self.parse_let()?; prog.stmts.push(stmt); },
                Return => { let stmt = self.parse_return()?; prog.stmts.push(stmt); },
                Eof => break,
                _ => { let stmt = self.parse_expr_stmt()?; prog.stmts.push(stmt); }
            }
        }
        Ok(prog)
    }

    // My solution will be to just expose this
    pub fn parse(source_code: String) -> Result<Program, ParseError>
    {
        let mut parser = Self::initialize(source_code)?;
        parser.parse_program()
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use super::Stmt;
    use super::Expr;

    #[test]
    fn parse_simple_program()
    {
        let source_code = "
            let x = 5;
            let y = 4;
            return 42;";
        if let Ok(prog) = Parser::parse(source_code.to_string()) {
            let expected =
                vec![ Stmt::LetStmt("x".to_string(), Expr::Integer(5))
                    , Stmt::LetStmt("y".to_string(), Expr::Integer(4))
                    , Stmt::ReturnStmt(Expr::Integer(42))
                    ];
            assert!(prog.stmts == expected);
        } else {
            panic!("Unrecognized token");
        }
    }
}
