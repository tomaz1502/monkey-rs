use std::collections::HashMap;

use crate::mods::lib::lexer;
use crate::mods::lib::expr::*;

use lexer::Token::*;
use lexer::LexError;

impl PrefixOperator {
    fn from_tok(tok: &lexer::Token) -> Result<Self, ParseError> {
        match tok {
            lexer::Token::Bang => Ok(PrefixOperator::Bang),
            lexer::Token::Minus => Ok(PrefixOperator::Minus),
            _ => Err(ParseError::UnexpectedToken) // TODO: Specify expected and what you got in the error
        }
    }
}

impl InfixOperator {
    fn from_tok(tok: &lexer::Token) -> Result<Self, ParseError> {
        match tok {
            lexer::Token::Plus => Ok(InfixOperator::Plus),
            lexer::Token::Minus => Ok(InfixOperator::Minus),
            lexer::Token::Mult => Ok(InfixOperator::Mult),
            lexer::Token::Slash => Ok(InfixOperator::Div),
            lexer::Token::LT => Ok(InfixOperator::LT),
            lexer::Token::GT => Ok(InfixOperator::GT),
            lexer::Token::Eq => Ok(InfixOperator::Eq),
            lexer::Token::Neq => Ok(InfixOperator::Neq),
            _ => Err(ParseError::UnexpectedToken) // TODO: Should we have another parse error here?
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn token_precedence(tk: &lexer::Token) -> Self {
        match tk {
            Eq    => Precedence::Equals,
            Neq   => Precedence::Equals,
            LT    => Precedence::LessGreater,
            GT    => Precedence::LessGreater,
            Plus  => Precedence::Sum,
            Minus => Precedence::Sum,
            Mult  => Precedence::Product,
            Slash => Precedence::Product,
            LPar  => Precedence::Call,
            _     => Precedence::Lowest
        }
    }
}

#[derive(Debug)]
pub enum ParseError { UnrecognizedToken, UnexpectedToken }

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        match err {
            LexError::UnrecognizedToken => ParseError::UnrecognizedToken
        }
    }
}

type PrefixCont = for<'a> fn (&'a mut Parser) -> Result<Expr, ParseError>;
type InfixCont = for<'a> fn (&'a mut Parser, Expr) -> Result<Expr, ParseError>;

pub struct Parser {
    curr_token: lexer::Token,
    lexer: lexer::Lexer,

    prefix_fn_map: HashMap<lexer::Token, PrefixCont>,
    infix_fn_map: HashMap<lexer::Token, InfixCont>,
}

// IMPORTANT NOTE: All prefix and infix parsing functions always leave the current token as the last token within the
// rule being parsed, they do not advance past it.
impl Parser {
    fn new(source_code: String) -> Result<Self, ParseError> {
        let mut lexer = lexer::Lexer::new(source_code);
        let curr_token = lexer.get_next_token()?;
        let mut parser =
            Parser { curr_token, lexer, prefix_fn_map: HashMap::new(), infix_fn_map: HashMap::new() };
        parser.register_prefix_fn(lexer::Token::Id("".to_string()), Self::parse_id_to_expr);
        parser.register_prefix_fn(lexer::Token::Integer(0), Self::convert_token_to_int);
        parser.register_prefix_fn(lexer::Token::Bang, Self::parse_prefix_expr);
        parser.register_prefix_fn(lexer::Token::Minus, Self::parse_prefix_expr);
        parser.register_prefix_fn(lexer::Token::True, Self::convert_token_to_bool);
        parser.register_prefix_fn(lexer::Token::False, Self::convert_token_to_bool);
        parser.register_prefix_fn(lexer::Token::LPar, Self::parse_group);
        parser.register_prefix_fn(lexer::Token::If, Self::parse_ite);
        parser.register_prefix_fn(lexer::Token::Fn, Self::parse_lambda);
        parser.register_infix_fn(lexer::Token::Plus, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Minus, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Mult, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Slash, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::LT, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::GT, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Eq, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::Neq, Self::parse_infix_expr);
        parser.register_infix_fn(lexer::Token::LPar, Self::parse_call);
        Ok(parser)
    }

    fn register_prefix_fn(&mut self, t: lexer::Token, f: PrefixCont) {
        self.prefix_fn_map.insert(t, f);
    }

    fn register_infix_fn(&mut self, t: lexer::Token, f: InfixCont) {
        self.infix_fn_map.insert(t, f);
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, ParseError> {
        let op = PrefixOperator::from_tok(&self.curr_token)?;
        self.advance_token()?;
        let rhs = self.parse_expr_prec(Precedence::Prefix)?;
        Ok(Expr::PrefixOp(op, Box::new(rhs)))
    }

    fn parse_infix_expr(&mut self, lhs: Expr) -> Result<Expr, ParseError> {
        let op = InfixOperator::from_tok(&self.curr_token)?;
        let prec = Precedence::token_precedence(&self.curr_token);
        self.advance_token()?;
        let rhs = self.parse_expr_prec(prec)?;
        Ok(Expr::InfixOp(op, Box::new(lhs), Box::new(rhs)))
    }

    fn expect_token(&self, tok: lexer::Token) -> Result<(), ParseError> {
        if self.curr_token == tok {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_group(&mut self) -> Result<Expr, ParseError> {
        self.advance_token()?;
        let expr = self.parse_expr()?;
        self.advance_token()?;
        self.expect_token(RPar)?;
        Ok(expr)
    }

    fn parse_ite(&mut self) -> Result<Expr, ParseError> {
        self.advance_token()?;
        self.expect_token(LPar)?;
        self.advance_token()?;
        let cond = self.parse_expr()?;
        self.advance_token()?;
        self.expect_token(RPar)?;
        self.advance_token()?;
        self.expect_token(LBrack)?;
        self.advance_token()?;
        let then_block = self.parse_block_stmt()?;
        if self.lexer.peek_token()? == Else {
            self.advance_token()?; // RBrack
            self.advance_token()?; // Else
            self.expect_token(LBrack)?;
            self.advance_token()?;
            let else_block = self.parse_block_stmt()?;
            Ok(Expr::Ite(Box::new(cond), then_block, Some(else_block)))
        } else {
            Ok(Expr::Ite(Box::new(cond), then_block, None))
        }
    }

    fn convert_token_to_basic_type(&mut self) -> Result<Type, ParseError> {
        match self.curr_token {
            Int => Ok(Type::Integer),
            Bool => Ok(Type::Boolean),
            Unit => Ok(Type::Unit),
            _ =>  Err(ParseError::UnexpectedToken)
        }
    }

    // No higher order functions for now
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let first_type = self.convert_token_to_basic_type()?;
        if self.lexer.peek_token()? == Arrow {
            self.advance_token()?;
            self.advance_token()?;
            let arrow_tail = self.parse_type()?;
            Ok(Type::Arrow(Box::new(first_type), Box::new(arrow_tail)))
        } else {
            Ok(first_type)
        }
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        self.advance_token()?; // fn
        self.expect_token(LPar)?;
        self.advance_token()?;
        let mut params = vec![];
        while let Id(id) = self.curr_token.clone() {
            self.advance_token()?;
            self.expect_token(Colon)?;
            self.advance_token()?;
            let typ = self.parse_type()?;
            params.push((id, typ));
            self.advance_token()?;
            if self.curr_token == Comma {
                self.advance_token()?;
            } else {
                break;
            }
        }
        self.expect_token(RPar)?;
        self.advance_token()?;
        self.expect_token(Arrow)?;
        self.advance_token()?;
        let ret = self.parse_type()?;
        self.advance_token()?;

        self.expect_token(LBrack)?;
        self.advance_token()?;
        let body = self.parse_block_stmt()?;
        Ok(Expr::Lambda(params, ret, body))
    }

    fn parse_call(&mut self, f_id: Expr) -> Result<Expr, ParseError> {
        let f_id_str = match f_id {
            Expr::Ident(id) => id,
            _ => unreachable!()
        };
        self.advance_token()?;
        let mut args = vec![];
        while self.curr_token != RPar {
            let arg = self.parse_expr()?;
            args.push(arg);
            self.advance_token()?;
            if self.curr_token == Comma {
                self.advance_token()?;
            }
        }
        self.expect_token(RPar)?;
        Ok(Expr::Call(f_id_str, args))
    }

    fn convert_token_to_int(&mut self) -> Result<Expr, ParseError> {
        // This check looks redundant
        match self.curr_token {
            Integer(num) => Ok(Expr::Integer(num)),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    /* We should have a single function parse literal for integer and bool and string */
    fn convert_token_to_bool(&mut self) -> Result<Expr, ParseError> {
        match self.curr_token {
            True => Ok(Expr::Boolean(true)),
            False => Ok(Expr::Boolean(false)),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_id_to_expr(&mut self) -> Result<Expr, ParseError> {
        match &self.curr_token {
            Id(name) => Ok(Expr::Ident(name.clone())),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn advance_token(&mut self) -> Result<(), ParseError> {
        self.curr_token = self.lexer.get_next_token()?;
        Ok(())
    }

    fn parse_id(&self) -> Result<String, ParseError> {
        match &self.curr_token {
            Id(name) => Ok(name.clone()),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_expr_prec(&mut self, prec: Precedence) -> Result<Expr, ParseError> {
        let prefix_fn = self.prefix_fn_map.get(&self.curr_token).ok_or(ParseError::UnexpectedToken)?;
        let mut left_expr = prefix_fn(self)?;

        loop {
            let tk = self.lexer.peek_token()?;
            if tk == lexer::Token::Semicolon {
                 break;
            }
            if Precedence::token_precedence(&tk) <= prec {
                 break;
            }
            match self.infix_fn_map.get(&tk) {
                 None => return Ok(left_expr),
                 Some(infix_fn) => {
                     let infix_fn_cln = *infix_fn;
                     self.advance_token()?;
                     left_expr = infix_fn_cln(self, left_expr)?;
                 }
            }
        }

        Ok(left_expr)
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_prec(Precedence::Lowest)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;

        self.advance_token()?;
        if let Semicolon = self.curr_token {
            self.advance_token()?;
        }

        Ok(Stmt::Expr(expr))
    }

    // Should update the lexer until the first token after the let expression (after the semicolon)
    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        self.advance_token()?;

        let id = self.parse_id()?;

        self.advance_token()?; // assign token
        self.expect_token(Assign)?;

        self.advance_token()?;

        let expr = self.parse_expr()?;

        self.advance_token()?;
        self.expect_token(Semicolon)?;

        self.advance_token()?;

        Ok(Stmt::Let(id, expr))
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        self.advance_token()?;

        let expr = self.parse_expr()?;

        self.advance_token()?;
        self.expect_token(Semicolon)?;

        self.advance_token()?;

        Ok(Stmt::Return(expr))
    }

    fn parse_block_stmt(&mut self) -> Result<Block, ParseError> {
        let mut block = Block { stmts: vec![] };
        loop {
            match self.curr_token {
                Let => { let stmt = self.parse_let()?; block.stmts.push(stmt); },
                Return => { let stmt = self.parse_return()?; block.stmts.push(stmt); },
                RBrack => break,
                _ => { let stmt = self.parse_expr_stmt()?; block.stmts.push(stmt); }
            }
        }
        Ok(block)
    }

    // TODO: This way we stop after the first error. What if we want to continue?
    fn parse_prog(&mut self) -> Result<Block, ParseError> {
        let mut prog = Block { stmts: vec![] };
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

    pub fn parse(source_code: String) -> Result<Block, ParseError> {
        let mut parser = Self::new(source_code)?;
        parser.parse_prog()
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
    fn parse_simple_program_test() {
        let source_code = "
            let x = 5;
            let y = 4;
            return 42;";
        if let Ok(prog) = Parser::parse(source_code.to_string()) {
            let expected =
                vec![ Stmt::Let("x".to_string(), Expr::Integer(5))
                    , Stmt::Let("y".to_string(), Expr::Integer(4))
                    , Stmt::Return(Expr::Integer(42))
                    ];
            assert!(prog.stmts == expected);
        } else {
            panic!("Parser failed");
        }
    }

    #[test]
    fn parse_simple_id_test() {
        let source_code = "foobar;";
        if let Ok(prog) = Parser::parse(source_code.to_string()) {
            let expected = vec![Stmt::Expr(Expr::Ident("foobar".to_string()))];
            assert!(prog.stmts == expected);
        } else {
            panic!("Parser failed");
        }
    }

    #[test]
    fn parse_prefix_expr_test() {
        struct TestCase { input: &'static str, op: PrefixOperator, arg: Expr }
        let cases = [
            TestCase {input: "!5;", op: PrefixOperator::Bang, arg: Expr::Integer(5)},
            TestCase {input: "-15;", op: PrefixOperator::Minus, arg: Expr::Integer(15)}
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                match &prog.stmts[0] {
                    Stmt::Expr(Expr::PrefixOp(op, rhs)) =>
                        assert!(*op == case.op && **rhs == case.arg),
                    _ => panic!("Failed: wrong parsing")
                }
            } else {
                panic!("Parser failed");
            }
        }
    }

    #[test]
    fn parse_infix_expr_test() {
        struct TestCase { input: &'static str, op: InfixOperator, lhs: Expr, rhs: Expr }
        let cases = [
            TestCase {input: "5 + 5;", op: InfixOperator::Plus, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 - 5;", op: InfixOperator::Minus, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 * 5;", op: InfixOperator::Mult, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 / 5;", op: InfixOperator::Div, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 < 5;", op: InfixOperator::LT, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 > 5;", op: InfixOperator::GT, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 == 5", op: InfixOperator::Eq, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 != 5;", op: InfixOperator::Neq, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                assert_eq!(prog.stmts.len(), 1);
                match &prog.stmts[0] {
                    Stmt::Expr(Expr::InfixOp(op, lhs, rhs)) =>
                        assert!(*op == case.op && **lhs == case.lhs && **rhs == case.rhs),
                    _ => panic!("Failed: wrong parsing")
                }
            } else {
                panic!("Parser failed");
            }
        }
    }

    #[test]
    fn parse_composed_expr_test() {
        struct TestCase { input: &'static str, parsed_pp: &'static str }
        let cases = [
            TestCase { input: "a + b * c", parsed_pp: "(a + (b * c))" },
            TestCase { input: "-a * b", parsed_pp: "((-a) * b)" },
            TestCase { input: "!-a", parsed_pp: "(!(-a))" },
            TestCase { input: "a + b + c", parsed_pp: "((a + b) + c)" },
            TestCase { input: "a + b - c", parsed_pp: "((a + b) - c)" },
            TestCase { input: "a * b * c", parsed_pp: "((a * b) * c)" },
            TestCase { input: "a * b / c", parsed_pp: "((a * b) / c)" },
            TestCase { input: "a + b / c", parsed_pp: "(a + (b / c))" },
            TestCase { input: "a + b * c + d / e - f", parsed_pp: "(((a + (b * c)) + (d / e)) - f)" },
            TestCase { input: "5 > 4 == 3 < 4", parsed_pp: "((5 > 4) == (3 < 4))" },
            TestCase { input: "5 < 4 != 3 > 4", parsed_pp: "((5 < 4) != (3 > 4))" },
            TestCase { input: "3 + 4 * 5 == 3 * 1 + 4 * 5", parsed_pp: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", },
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                assert_eq!(prog.stmts.len(), 1);
                assert_eq!(prog.stmts[0].to_string(), case.parsed_pp);
            } else {
                panic!("Parser failed");
            }
        }
    }
}
