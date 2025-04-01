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
    fn token_precedence(tk: &lexer::Token) -> Self
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

enum ParseError { UnrecognizedToken, UnexpectedToken, TokenWithoutPrefixFn, TokenWithoutInfixFn }

#[derive(PartialEq, Debug)]
enum PrefixOperator { Minus, Bang }

impl PrefixOperator {
    fn from_tok(tok: &lexer::Token) -> Result<Self, ParseError>
    {
        match tok {
            lexer::Token::Bang => Ok(PrefixOperator::Bang),
            lexer::Token::Minus => Ok(PrefixOperator::Minus),
            _ => Err(ParseError::UnexpectedToken) // TODO: Should we have another parse error here?
        }
    }
}

#[derive(PartialEq, Debug)]
enum InfixOperator { Plus, Minus, Mult, Slash, Eq, Neq, LT, GT }

impl InfixOperator {
    fn from_tok(tok: &lexer::Token) -> Result<Self, ParseError>
    {
        match tok {
            lexer::Token::Plus => Ok(InfixOperator::Plus),
            lexer::Token::Minus => Ok(InfixOperator::Minus),
            lexer::Token::Mult => Ok(InfixOperator::Mult),
            lexer::Token::Slash => Ok(InfixOperator::Slash),
            lexer::Token::LT => Ok(InfixOperator::LT),
            lexer::Token::GT => Ok(InfixOperator::GT),
            lexer::Token::Eq => Ok(InfixOperator::Eq),
            lexer::Token::Neq => Ok(InfixOperator::Neq),
            _ => Err(ParseError::UnexpectedToken) // TODO: Should we have another parse error here?
        }
    }
}

#[derive(PartialEq, Debug)]
enum Expr {
    Ident(Id),
    Integer(i64),
    PrefixExpr(PrefixOperator, Box<Expr>),
    InfixExpr(InfixOperator, Box<Expr>, Box<Expr>)
}

#[derive(PartialEq, Debug)]
enum Stmt {
    LetStmt(Id, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr)
}

struct Program {
    stmts: Vec<Stmt>
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self
    {
        match err {
            LexError::UnrecognizedToken => ParseError::UnrecognizedToken
        }
    }
}

type PrefixCont = for<'a> fn (&'a mut Parser) -> Result<Expr, ParseError>;
type InfixCont = for<'a> fn (&'a mut Parser, Expr) -> Result<Expr, ParseError>;

struct Parser {
    curr_token: lexer::Token,
    lexer: lexer::Lexer,

    prefix_parse_fn: HashMap<lexer::Token, PrefixCont>,
    infix_parse_fn: HashMap<lexer::Token, InfixCont>,
}

impl Parser {
    fn new(source_code: String) -> Result<Self, ParseError> {
        let mut lexer = lexer::Lexer::new(source_code);
        let curr_token = lexer.get_next_token()?;
        let mut parser =
            Parser { curr_token, lexer, prefix_parse_fn: HashMap::new(), infix_parse_fn: HashMap::new() };
        parser.register_prefix_fn(lexer::Token::Id("".to_string()), Self::parse_id_to_expr);
        parser.register_prefix_fn(lexer::Token::Integer(42), Self::parse_integer_to_expr);
        parser.register_prefix_fn(lexer::Token::Bang, Self::parse_prefix_expr);
        parser.register_prefix_fn(lexer::Token::Minus, Self::parse_prefix_expr);
        parser.register_infix_fn(lexer::Token::Plus, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Minus, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Mult, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Slash, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::LT, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::GT, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Eq, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Neq, Self::parse_infix_expr);
        Ok(parser)
    }

    fn register_prefix_fn(&mut self, t: lexer::Token, f: PrefixCont) -> ()
    {
        self.prefix_parse_fn.insert(t, f);
    }

    fn get_prefix_fn(&self) -> Result<PrefixCont, ParseError>
    {
        self.prefix_parse_fn.get(&self.curr_token).ok_or(ParseError::TokenWithoutPrefixFn).map(|x| { *x })
    }

    fn register_infix_fn(&mut self, t: lexer::Token, f: InfixCont) -> ()
    {
        self.infix_parse_fn.insert(t, f);
    }

    fn get_infix_fn(&self) -> Result<InfixCont, ParseError>
    {
        self.infix_parse_fn.get(&self.curr_token).ok_or(ParseError::TokenWithoutInfixFn).map(|x| { *x })
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, ParseError>
    {
        let op = PrefixOperator::from_tok(&self.curr_token)?;
        self.advance_token()?;
        let rhs = self.parse_expr()?;
        Ok(Expr::PrefixExpr(op, Box::new(rhs)))
    }

    fn parse_infix_expr(&mut self, lhs: Expr) -> Result<Expr, ParseError>
    {
        let op = InfixOperator::from_tok(&self.curr_token)?;
        let prec = Precedence::token_precedence(&self.curr_token);
        self.advance_token()?;
        let rhs = self.parse_expr_prec(prec)?;
        Ok(Expr::InfixExpr(op, Box::new(lhs), Box::new(rhs)))
    }

    fn parse_integer_to_expr(&mut self) -> Result<Expr, ParseError>
    {
        match self.curr_token {
            Integer(num) => Ok(Expr::Integer(num)),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_id_to_expr(&mut self) -> Result<Expr, ParseError>
    {
        match &self.curr_token {
            Id(name) => Ok(Expr::Ident(name.clone())),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn advance_token(&mut self) -> Result<(), ParseError>
    {
        self.curr_token = self.lexer.get_next_token()?;
        Ok(())
    }

    fn parse_id(&self) -> Result<String, ParseError>
    {
        match &self.curr_token {
            Id(name) => Ok(name.clone()),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_expr_prec(&mut self, _prec: Precedence) -> Result<Expr, ParseError>
    {
        let prefix_fn = self.get_prefix_fn()?;
        let left_expr = prefix_fn(self)?;

        // loop {
            
        // }

        Ok(left_expr)
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError>
    {
        self.parse_expr_prec(Precedence::LOWEST)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError>
    {
        let expr = self.parse_expr()?;

        self.advance_token()?;
        match self.curr_token {
            Semicolon => self.advance_token()?,
            _ => ()
        }

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
        let mut parser = Self::new(source_code)?;
        parser.parse_program()
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use super::Stmt;
    use super::Expr;
    use super::PrefixOperator;
    use super::InfixOperator;

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
            panic!("Parser failed");
        }
    }

    #[test]
    fn parse_simple_id()
    {
        let source_code = "foobar;";
        if let Ok(prog) = Parser::parse(source_code.to_string()) {
            let expected = vec![Stmt::ExprStmt(Expr::Ident("foobar".to_string()))];
            assert!(prog.stmts == expected);
        } else {
            panic!("Parser failed");
        }
    }

    #[test]
    fn parse_prefix_expr_test()
    {
        struct TestCase { input: &'static str, op: PrefixOperator, arg: Expr }
        let cases = [
            TestCase {input: "!5;", op: PrefixOperator::Bang, arg: Expr::Integer(5)},
            TestCase {input: "-15;", op: PrefixOperator::Minus, arg: Expr::Integer(15)}
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                match &prog.stmts[0] {
                    Stmt::ExprStmt(Expr::PrefixExpr(op, rhs)) =>
                        assert!(*op == case.op && **rhs == case.arg),
                    _ => panic!("Failed: wrong parsing")
                }
            } else {
                panic!("Parser failed");
            }
        }
    }

    #[test]
    fn parse_infix_expr_test()
    {
        struct TestCase { input: &'static str, op: InfixOperator, lhs: Expr, rhs: Expr }
        let cases = [
            TestCase {input: "5 + 5;", op: InfixOperator::Plus, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 - 5;", op: InfixOperator::Minus, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 * 5;", op: InfixOperator::Mult, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 / 5;", op: InfixOperator::Slash, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 < 5;", op: InfixOperator::LT, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 > 5;", op: InfixOperator::GT, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 == 5;", op: InfixOperator::Eq, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 != 5;", op: InfixOperator::Neq, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                match &prog.stmts[0] {
                    Stmt::ExprStmt(Expr::InfixExpr(op, lhs, rhs)) =>
                        assert!(*op == case.op && **lhs == case.lhs && **rhs == case.rhs),
                    _ => panic!("Failed: wrong parsing")
                }
            } else {
                panic!("Parser failed");
            }
        }
    }
}
