use std::collections::HashMap;

use crate::mods::lib::lexer;

use lexer::Token::*;
use lexer::LexError;

#[derive(PartialEq, Eq, PartialOrd)]
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
            LPar  => Precedence::CALL,
            _     => Precedence::LOWEST
        }
    }
}

type Id = String;

#[derive(Debug)]
pub enum ParseError { UnrecognizedToken, UnexpectedToken }

#[derive(PartialEq, Debug)]
enum PrefixOperator { Minus, Bang }

impl PrefixOperator {
    fn from_tok(tok: &lexer::Token) -> Result<Self, ParseError>
    {
        match tok {
            lexer::Token::Bang => Ok(PrefixOperator::Bang),
            lexer::Token::Minus => Ok(PrefixOperator::Minus),
            _ => Err(ParseError::UnexpectedToken) // TODO: Specify expected and what you got in the error
        }
    }
}

impl ToString for PrefixOperator {
    fn to_string(&self) -> String
    {
        use PrefixOperator::*;
        match self {
            Minus => "-".to_string(),
            Bang => "!".to_string(),
        }
    }
}

#[derive(PartialEq, Debug)]
enum InfixOperator { Plus, Minus, Mult, Slash, Eq, Neq, LT, GT }

impl ToString for InfixOperator {
    fn to_string(&self) -> String
    {
        use InfixOperator::*;
        match self {
            Plus => "+".to_string(),
            Minus => "-".to_string(),
            Mult => "*".to_string(),
            Slash => "/".to_string(),
            Eq => "==".to_string(),
            Neq => "!=".to_string(),
            LT => "<".to_string(),
            GT => ">".to_string(),
        }
    }
}

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
    Boolean(bool),
    Ite(Box<Expr>, BlockStmt, Option<BlockStmt>),
    Lambda(Vec<Id>, BlockStmt),
    Call(Id, Vec<Expr>),
    PrefixExpr(PrefixOperator, Box<Expr>),
    InfixOp(InfixOperator, Box<Expr>, Box<Expr>)
}

impl ToString for Expr {

    fn to_string(&self) -> String
    {
        use Expr::*;
        match self {
            Ident(name) => name.to_string(),
            Integer(num) => num.to_string(),
            Boolean(b) => b.to_string(),
            Ite(cond, t, Some(e)) => format!("if ({}) {{ {} }} else {{ {} }}", (*cond).to_string(), t.to_string(), e.to_string()),
            Ite(cond, t, None) => format!("if ({}) {{ {} }}", (*cond).to_string(), t.to_string()),
            Lambda(params, body) => format!("fn ({}) {{ {} }}", params.join(", "), body.to_string()),
            Call(name, args) => format!("{}({})", name, args.iter().map(Expr::to_string).collect::<Vec<_>>().join(", ")),
            PrefixExpr(op, arg) => format!("({}{})", op.to_string(), arg.to_string()),
            InfixOp(op, lhs, rhs) => format!("({} {} {})", lhs.to_string(), op.to_string(), rhs.to_string())
        }
    }
}

#[derive(PartialEq, Debug)]
enum Stmt {
    LetStmt(Id, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr)
}

impl ToString for Stmt {
    fn to_string(&self) -> String
    {
        use Stmt::*;
        match self {
            LetStmt(id, e) => format!("let {} = {};", id, e.to_string()),
            ReturnStmt(e) => format!("return {};", e.to_string()),
            ExprStmt(e) => e.to_string()
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct BlockStmt {
    stmts: Vec<Stmt>
}

impl ToString for BlockStmt {
    fn to_string(&self) -> String
    {
        self.stmts.iter().map(Stmt::to_string).collect::<Vec<_>>().concat()
    }
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
        parser.register_prefix_fn(lexer::Token::Integer(0), Self::parse_integer_to_expr);
        parser.register_prefix_fn(lexer::Token::Bang, Self::parse_prefix_expr);
        parser.register_prefix_fn(lexer::Token::Minus, Self::parse_prefix_expr);
        parser.register_prefix_fn(lexer::Token::True, Self::parse_boolean_to_expr);
        parser.register_prefix_fn(lexer::Token::False, Self::parse_boolean_to_expr);
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

    fn register_prefix_fn(&mut self, t: lexer::Token, f: PrefixCont) -> ()
    {
        self.prefix_fn_map.insert(t, f);
    }

    fn register_infix_fn(&mut self, t: lexer::Token, f: InfixCont) -> ()
    {
        self.infix_fn_map.insert(t, f);
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, ParseError>
    {
        let op = PrefixOperator::from_tok(&self.curr_token)?;
        self.advance_token()?;
        let rhs = self.parse_expr_prec(Precedence::PREFIX)?;
        Ok(Expr::PrefixExpr(op, Box::new(rhs)))
    }

    fn parse_infix_expr(&mut self, lhs: Expr) -> Result<Expr, ParseError>
    {
        let op = InfixOperator::from_tok(&self.curr_token)?;
        let prec = Precedence::token_precedence(&self.curr_token);
        self.advance_token()?;
        let rhs = self.parse_expr_prec(prec)?;
        Ok(Expr::InfixOp(op, Box::new(lhs), Box::new(rhs)))
    }

    fn expect_token(&self, tok: lexer::Token) -> Result<(), ParseError>
    {
        if self.curr_token == tok {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_group(&mut self) -> Result<Expr, ParseError>
    {
        self.advance_token()?;
        let expr = self.parse_expr()?;
        self.advance_token()?;
        self.expect_token(RPar)?;
        Ok(expr)
    }

    fn parse_ite(&mut self) -> Result<Expr, ParseError>
    {
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

    fn parse_lambda(&mut self) -> Result<Expr, ParseError>
    {
        self.advance_token()?; // fn
        self.expect_token(LPar)?;
        self.advance_token()?;
        let mut params = vec![];
        while let Id(param) = &self.curr_token {
            params.push(param.clone());
            self.advance_token()?;
            if self.curr_token == Comma {
                self.advance_token()?;
            }
        }
        self.expect_token(RPar)?;
        self.advance_token()?;
        self.expect_token(LBrack)?;
        self.advance_token()?;
        let body = self.parse_block_stmt()?;
        Ok(Expr::Lambda(params, body))
    }

    fn parse_call(&mut self, f_id: Expr) -> Result<Expr, ParseError>
    {
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

    fn parse_integer_to_expr(&mut self) -> Result<Expr, ParseError>
    {
        // This check looks redundant
        match self.curr_token {
            Integer(num) => Ok(Expr::Integer(num)),
            _ => Err(ParseError::UnexpectedToken)
        }
    }

    /* We should have a single function parse literal for integer and bool and string */
    fn parse_boolean_to_expr(&mut self) -> Result<Expr, ParseError>
    {
        match self.curr_token {
            True => Ok(Expr::Boolean(true)),
            False => Ok(Expr::Boolean(false)),
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

    fn parse_expr_prec(&mut self, prec: Precedence) -> Result<Expr, ParseError>
    {
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
                    // what does it mean to clone a function? Is it expensive?
                    // but anyway modifying `infix_fn_cln` does not modify infix_fn
                    let infix_fn_cln = infix_fn.clone();
                    self.advance_token()?;
                    left_expr = infix_fn_cln(self, left_expr)?;
                }
           }
        }

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

        self.advance_token()?; // assign token
        self.expect_token(Assign)?;

        self.advance_token()?;

        let expr = self.parse_expr()?;

        self.advance_token()?;
        self.expect_token(Semicolon)?;

        self.advance_token()?;

        Ok(Stmt::LetStmt(id, expr))
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseError>
    {
        self.advance_token()?;

        let expr = self.parse_expr()?;

        self.advance_token()?;
        self.expect_token(Semicolon)?;

        self.advance_token()?;

        Ok(Stmt::ReturnStmt(expr))
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParseError>
    {
        let mut block = BlockStmt { stmts: vec![] };
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

    // I don't like the way this is structured. Parser should be something that we pass a string
    // and it returns the AST. Instead here its an object that holds a string and eventually in the
    // future we ask it to "parse" that string. It's weird.
    // TODO: This way we stop after the first error. What if we want to continue?
    fn parse_prog(&mut self) -> Result<BlockStmt, ParseError>
    {
        let mut prog = BlockStmt { stmts: vec![] };
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
    pub fn parse(source_code: String) -> Result<BlockStmt, ParseError>
    {
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
    fn parse_simple_program_test()
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
    fn parse_simple_id_test()
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
            TestCase {input: "5 == 5", op: InfixOperator::Eq, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
            TestCase {input: "5 != 5;", op: InfixOperator::Neq, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                assert_eq!(prog.stmts.len(), 1);
                match &prog.stmts[0] {
                    Stmt::ExprStmt(Expr::InfixOp(op, lhs, rhs)) =>
                        assert!(*op == case.op && **lhs == case.lhs && **rhs == case.rhs),
                    _ => panic!("Failed: wrong parsing")
                }
            } else {
                panic!("Parser failed");
            }
        }
    }

    #[test]
    fn parse_composed_expr_test()
    {
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
