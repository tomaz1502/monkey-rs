/*
Grammar:

block ::=
      "{" (<stmt>)* "}"

stmt ::= <stmt_content>;

stmt_content ::=
      <expr>
    | "let" <name> = <expr>
    | "return" <expr>
    | <block>

expr ::=
      <name>
    | <integer>
    | <boolean>
    | <char>
    | <string>
    | "uu"
    | "if" "(" <expr> ")" <block> ("else" "if" <block>)* ("else" <block>)?
    | "fn" "(" <comma_separated_typed_names> ")" "->" <type> <block>
    | <expr> "(" <comma_separated_exprs> ")"
    | <prefix_op> <expr>
    | <expr> <infix_op> <expr>

type ::=
      "unit"
    | "int"
    | "bool"
    | "char"
    | "string"
    | <type> "->" <type>

typed_name ::= <name> ":" <type>

comma_separated_exprs ::=
      ""
    | <expr>
    | <expr> "," <comma_separated_exprs>

comma_separated_typed_names ::=
      ""
    | <typed_name>
    | <typed_name> "," <comma_separated_typed_names>

prefix_op ::=
    "-" | "!"

infix_op ::=
    "+" | "-" | "*" | "/" | "%" | "=" | "!=" | "<" | ">"
*/


use std::collections::HashMap;

use crate::mods::lib::lexer;
use crate::mods::lib::expr::*;
use crate::mods::lib::utils::RESERVED_WORDS;

use lexer::Token::*;
use lexer::{ LexError, LexErrorKind };

impl PrefixOperator {
    fn from_curr_tok(parser: &Parser) -> Result<Self, ParseError> {
        match parser.curr_token {
            lexer::Token::Bang  => Ok(PrefixOperator::Bang),
            lexer::Token::Minus => Ok(PrefixOperator::Minus),
            _                   => Err(parser.mk_error(ParseErrorKind::UnexpectedToken)) // TODO: Specify expected and what you got in the error
        }
    }
}

impl InfixOperator {
    fn from_curr_tok(parser: &Parser) -> Result<Self, ParseError> {
        match parser.curr_token {
            lexer::Token::Plus    => Ok(InfixOperator::Plus),
            lexer::Token::Minus   => Ok(InfixOperator::Minus),
            lexer::Token::Mult    => Ok(InfixOperator::Mult),
            lexer::Token::Slash   => Ok(InfixOperator::Div),
            lexer::Token::Modulus => Ok(InfixOperator::Mod),
            lexer::Token::LT      => Ok(InfixOperator::LT),
            lexer::Token::GT      => Ok(InfixOperator::GT),
            lexer::Token::Eq      => Ok(InfixOperator::Eq),
            lexer::Token::Neq     => Ok(InfixOperator::Neq),
            _                     => Err(parser.mk_error(ParseErrorKind::UnexpectedToken)) // TODO: Should we have another parse error here?
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

impl lexer::Token {
    fn prec(&self) -> Precedence {
        match self {
            Eq       => Precedence::Equals,
            Neq      => Precedence::Equals,
            LT       => Precedence::LessGreater,
            GT       => Precedence::LessGreater,
            Plus     => Precedence::Sum,
            Minus    => Precedence::Sum,
            Mult     => Precedence::Product,
            Slash    => Precedence::Product,
            Modulus  => Precedence::Product,
            LPar     => Precedence::Call,
            LSqBrack => Precedence::Call,
            _        => Precedence::Lowest
        }
    }
}

// TODO: Make LexError a subtype of this
#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnrecognizedToken,
    UnexpectedToken,
    SingleQuoteString,
    UnclosedQuote,
    UnclosedDoubleQuote,
    ExpectedSemicolon,
    ReservedIdentifier(String),
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg: String = match self {
            Self::UnrecognizedToken      => String::from("unrecognized token."),
            Self::UnexpectedToken        => String::from("unexpected token."),
            Self::SingleQuoteString      => String::from("String enclosed with a single quote."),
            Self::UnclosedQuote          => String::from("Single quote was not closed."),
            Self::UnclosedDoubleQuote    => String::from("Double quote was not closed."),
            Self::ExpectedSemicolon      => String::from("Expected semicolon."),
            Self::ReservedIdentifier(id) => std::format!("let statement with reserved identifier: {}", id),
        };
        write!(f, "{}", msg)
    }
}

pub struct ParseError {
    kind: ParseErrorKind,
    line: u32,
    col: u32,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = std::format!("Parsing error at line {}, column {}:", self.line, self.col);
        let msg = self.kind().to_string();
        write!(f, "{} {}", prefix, &msg)
    }
}

impl From<ParseError> for String {
    fn from(pe: ParseError) -> String {
        pe.to_string()
    }
}

impl ParseError {
    pub fn kind(&self) -> ParseErrorKind {
        self.kind.clone()
    }
}

impl From<LexErrorKind> for ParseErrorKind {
    fn from(err: LexErrorKind) -> Self {
        match err {
            LexErrorKind::UnrecognizedToken   => ParseErrorKind::UnrecognizedToken,
            LexErrorKind::SingleQuoteString   => ParseErrorKind::SingleQuoteString,
            LexErrorKind::UnclosedQuote       => ParseErrorKind::UnclosedQuote,
            LexErrorKind::UnclosedDoubleQuote => ParseErrorKind::UnclosedDoubleQuote,
        }
    }
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        let kind: ParseErrorKind = From::from(err.kind());
        ParseError { kind: kind, line: err.line(), col: err.col() }
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

// Invariant: All functions prefixed with `parse` always leave the curr_token as the first
// one after the expression they are parsing.
impl Parser {
    fn new(source_code: String) -> Result<Self, ParseError> {
        let mut lexer = lexer::Lexer::new(source_code);
        let curr_token = lexer.get_next_token()?;
        let mut parser =
            Parser { curr_token, lexer, prefix_fn_map: HashMap::new(), infix_fn_map: HashMap::new() };
        parser.register_prefix_fn(lexer::Token::Identifier("".to_string()), Self::parse_id_expr);
        parser.register_prefix_fn(lexer::Token::IntLit(0), Self::parse_int);
        parser.register_prefix_fn(lexer::Token::CharLit('a'), Self::parse_char);
        parser.register_prefix_fn(lexer::Token::StrLit("".to_string()), Self::parse_string);
        parser.register_prefix_fn(lexer::Token::Unit, Self::parse_unit);
        parser.register_prefix_fn(lexer::Token::Bang, Self::parse_prefix_op);
        parser.register_prefix_fn(lexer::Token::Minus, Self::parse_prefix_op);
        parser.register_prefix_fn(lexer::Token::True, Self::parse_bool);
        parser.register_prefix_fn(lexer::Token::False, Self::parse_bool);
        parser.register_prefix_fn(lexer::Token::LPar, Self::parse_group);
        parser.register_prefix_fn(lexer::Token::If, Self::parse_ite);
        parser.register_prefix_fn(lexer::Token::Fn, Self::parse_lambda);
        parser.register_infix_fn(lexer::Token::Plus, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::Minus, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::Mult, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::Slash, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::Modulus, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::LT, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::GT, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::Eq, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::Neq, Self::parse_infix_op);
        parser.register_infix_fn(lexer::Token::LPar, Self::parse_call);
        parser.register_infix_fn(lexer::Token::LSqBrack, Self::parse_indexed_access);
        Ok(parser)
    }

    fn mk_error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError { kind: kind, line: self.lexer.line(), col: self.lexer.col() }
    }

    fn register_prefix_fn(&mut self, t: lexer::Token, f: PrefixCont) {
        self.prefix_fn_map.insert(t, f);
    }

    fn register_infix_fn(&mut self, t: lexer::Token, f: InfixCont) {
        self.infix_fn_map.insert(t, f);
    }

    fn parse_prefix_op(&mut self) -> Result<Expr, ParseError> {
        let op = PrefixOperator::from_curr_tok(&self)?;
        self.advance_token()?;
        let rhs = self.parse_expr_prec(Precedence::Prefix)?;
        Ok(Expr::PrefixOp(op, Box::new(rhs)))
    }

    fn parse_infix_op(&mut self, lhs: Expr) -> Result<Expr, ParseError> {
        let op = InfixOperator::from_curr_tok(&self)?;
        let prec = self.curr_token.prec();
        self.advance_token()?;
        let rhs = self.parse_expr_prec(prec)?;
        Ok(Expr::InfixOp(op, Box::new(lhs), Box::new(rhs)))
    }

    fn advance_token(&mut self) -> Result<(), ParseError> {
        self.curr_token = self.lexer.get_next_token()?;
        Ok(())
    }

    fn expect_token(&self, tok: lexer::Token, err: Option<ParseErrorKind>) -> Result<(), ParseError> {
        if self.curr_token == tok {
            Ok(())
        } else {
            let err = err.unwrap_or(ParseErrorKind::UnexpectedToken);
            Err(self.mk_error(err))
        }
    }

    fn parse_group(&mut self) -> Result<Expr, ParseError> {
        self.advance_token()?;
        let expr = self.parse_expr()?;
        self.expect_token(RPar, None)?;
        self.advance_token()?;
        Ok(expr)
    }

    fn parse_ite(&mut self) -> Result<Expr, ParseError> {
        self.advance_token()?; // If
        let cond = self.parse_group()?;
        let then_block = self.parse_block()?;
        if self.curr_token == Else {
            self.advance_token()?;
            if self.curr_token == If {
                let r = self.parse_ite()?;
                let s = Stmt::Expr(r);
                let b = Block { stmts: vec![s] };
                Ok(Expr::Ite(Box::new(cond), then_block, Some(b)))
            } else {
                let else_block = self.parse_block()?;
                Ok(Expr::Ite(Box::new(cond), then_block, Some(else_block)))
            }
        } else {
            Ok(Expr::Ite(Box::new(cond), then_block, None))
        }
    }

    fn token_to_basic_type(&self, tk: &lexer::Token) -> Result<Type, ParseError> {
        match tk {
            IntType  => Ok(Type::Integer),
            CharType => Ok(Type::Char),
            BoolType => Ok(Type::Boolean),
            UnitType => Ok(Type::Unit),
            StrType  => Ok(Type::Str),
            _    => Err(self.mk_error(ParseErrorKind::UnexpectedToken))
        }
    }

    // No higher order functions for now
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let first_type = self.token_to_basic_type(&self.curr_token)?;
        self.advance_token()?;
        if self.curr_token == ArrowType {
            self.advance_token()?;
            let arrow_tail = self.parse_type()?;
            Ok(Type::Arrow(Box::new(first_type), Box::new(arrow_tail)))
        } else {
            Ok(first_type)
        }
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        self.advance_token()?; // fn
        self.expect_token(LPar, None)?;
        self.advance_token()?;
        let mut params = vec![];
        while let Identifier(id) = self.curr_token.clone() {
            self.advance_token()?;
            self.expect_token(Colon, None)?;
            self.advance_token()?;
            let typ = self.parse_type()?;
            params.push((id, typ));
            if self.curr_token == Comma {
                self.advance_token()?;
            } else {
                break;
            }
        }
        self.expect_token(RPar, None)?;
        self.advance_token()?;
        self.expect_token(ArrowType, None)?;
        self.advance_token()?;
        let ret = self.parse_type()?;
        let body = self.parse_block()?;
        Ok(Expr::Lambda(params, ret, body))
    }

    fn parse_call(&mut self, f: Expr) -> Result<Expr, ParseError> {
        self.advance_token()?;
        let mut args = vec![];
        while self.curr_token != RPar {
            let arg = self.parse_expr()?;
            args.push(arg);
            if self.curr_token == Comma {
                self.advance_token()?;
            } else {
                break;
            }
        }
        if args.is_empty() {
            args.push(Expr::Unit);
        }
        self.expect_token(RPar, None)?;
        self.advance_token()?;
        Ok(Expr::Call(Box::new(f), args))
    }

    fn parse_indexed_access(&mut self, arr: Expr) -> Result<Expr, ParseError> {
        self.advance_token()?;
        let idx = self.parse_expr()?;
        self.expect_token(RSqBrack, None)?;
        self.advance_token()?;
        Ok(Expr::IndexedAccess(Box::new(arr), Box::new(idx)))
    }

    fn parse_int(&mut self) -> Result<Expr, ParseError> {
        // This check looks redundant
        match self.curr_token {
            IntLit(num) => {
                self.advance_token()?;
                Ok(Expr::Integer(num))
            }
            _ => unreachable!(),
        }
    }

    fn parse_char(&mut self) -> Result<Expr, ParseError> {
        // This check looks redundant
        match self.curr_token {
            CharLit(c) => {
                self.advance_token()?;
                Ok(Expr::Char(c))
            }
            _ => unreachable!(),
        }
    }

    fn parse_unit(&mut self) -> Result<Expr, ParseError> {
        // This check looks redundant
        match self.curr_token {
            Unit => {
                self.advance_token()?;
                Ok(Expr::Unit)
            }
            _ => unreachable!(),
        }
    }

    fn parse_string(&mut self) -> Result<Expr, ParseError> {
        let mut answer = Err(self.mk_error(ParseErrorKind::UnexpectedToken));
        match self.curr_token {
            StrLit(ref s) => {
                answer = Ok(Expr::Str(s.clone()));
            }
            _ => {},
        }
        self.advance_token()?;
        return answer;
    }

    /* We should have a single function parse literal for integer and bool and string */
    fn parse_bool(&mut self) -> Result<Expr, ParseError> {
        match self.curr_token {
            True => {
                self.advance_token()?;
                Ok(Expr::Boolean(true))
            }
            False => {
                self.advance_token()?;
                Ok(Expr::Boolean(false))
            }
            _ => unreachable!(),
        }
    }

    fn parse_id_expr(&mut self) -> Result<Expr, ParseError> {
        match &self.curr_token {
            Identifier(name) => {
                let name_cln = name.clone();
                self.advance_token()?;
                Ok(Expr::Ident(name_cln))
            }
            _ => Err(self.mk_error(ParseErrorKind::UnexpectedToken))
        }
    }

    fn parse_id(&mut self) -> Result<String, ParseError> {
        match &self.curr_token {
            Identifier(name) => {
                let name_cln = name.clone();
                self.advance_token()?;
                Ok(name_cln)
            }
            _ => Err(self.mk_error(ParseErrorKind::UnexpectedToken))
        }
    }

    fn parse_expr_prec(&mut self, prec: Precedence) -> Result<Expr, ParseError> {
        let prefix_fn =
            self.prefix_fn_map
                .get(&self.curr_token)
                // A token without a prefix function - maybe a different kind of parse error
                .ok_or(self.mk_error(ParseErrorKind::UnexpectedToken))?;
        let mut left_expr = prefix_fn(self)?;

        loop {
            let tk = &self.curr_token;
            if *tk == lexer::Token::Semicolon {
                 break;
            }
            if tk.prec() <= prec {
                 break;
            }
            match self.infix_fn_map.get(&tk) {
                 None => return Ok(left_expr),
                 Some(infix_fn) => {
                     let infix_fn_cln = *infix_fn;
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
        Ok(Stmt::Expr(expr))
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        self.advance_token()?;
        let id = self.parse_id()?;

        if RESERVED_WORDS.contains(&id.as_str()) {
            return Err(self.mk_error(ParseErrorKind::ReservedIdentifier(id)));
        }

        self.expect_token(Assign, None)?;
        self.advance_token()?;
        let expr = self.parse_expr()?;
        Ok(Stmt::Let(id, expr))
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        self.advance_token()?;
        let expr = self.parse_expr()?;
        Ok(Stmt::Return(expr))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self.curr_token {
            Let => self.parse_let(),
            Return => self.parse_return(),
            LBrack => {
                let block = self.parse_block()?;
                Ok(Stmt::Block(block))
            },
            _ => self.parse_expr_stmt(),

        };
        self.expect_token(Semicolon, Some(ParseErrorKind::ExpectedSemicolon))?;
        self.advance_token()?;
        stmt
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut block = Block { stmts: vec![] };
        self.advance_token()?;
        loop {
            if let RBrack = self.curr_token {
                self.advance_token()?;
                break;
            } else {
                let stmt = self.parse_stmt()?;
                block.stmts.push(stmt);
            }
        }
        Ok(block)
    }

    // TODO: This way we stop after the first error. What if we want to continue?
    fn parse_prog(&mut self) -> Result<Block, ParseError> {
        let mut prog = Block { stmts: vec![] };
        loop {
            if let Eof = self.curr_token {
                break;
            } else {
                let stmt = self.parse_stmt()?;
                prog.stmts.push(stmt);
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
    use super::*;

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
            TestCase {input: "5 == 5;", op: InfixOperator::Eq, lhs: Expr::Integer(5), rhs: Expr::Integer(5)},
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
            TestCase { input: "a + b * c;", parsed_pp: "(a + (b * c))" },
            TestCase { input: "-a * b;", parsed_pp: "((-a) * b)" },
            TestCase { input: "!-a;", parsed_pp: "(!(-a))" },
            TestCase { input: "a + b + c;", parsed_pp: "((a + b) + c)" },
            TestCase { input: "a + b - c;", parsed_pp: "((a + b) - c)" },
            TestCase { input: "a * b * c;", parsed_pp: "((a * b) * c)" },
            TestCase { input: "a * b / c;", parsed_pp: "((a * b) / c)" },
            TestCase { input: "a + b / c;", parsed_pp: "(a + (b / c))" },
            TestCase { input: "a + b * c + d / e - f;", parsed_pp: "(((a + (b * c)) + (d / e)) - f)" },
            TestCase { input: "5 > 4 == 3 < 4;", parsed_pp: "((5 > 4) == (3 < 4))" },
            TestCase { input: "5 < 4 != 3 > 4;", parsed_pp: "((5 < 4) != (3 > 4))" },
            TestCase { input: "3 + 4 * 5 == 3 * 1 + 4 * 5;", parsed_pp: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", },
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

    #[test]
    fn parse_let_char() {
        struct TestCase { input: &'static str, parsed_pp: &'static str }
        let cases = [
            TestCase { input: "let foo = 'a';", parsed_pp: "let foo = 'a';"},
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                assert_eq!(prog.stmts.len(), 1);
                assert_eq!(prog.stmts[0].to_string(), case.parsed_pp);
            } else {
                panic!("Parser failed!");
            }
        }
    }

    #[test]
    fn parse_let_string() {
        struct TestCase { input: &'static str, parsed_pp: &'static str }
        let cases = [
            TestCase { input: "let foo = \"foobar\";", parsed_pp: "let foo = \"foobar\";"},
        ];
        for case in cases {
            if let Ok(prog) = Parser::parse(case.input.to_string()) {
                assert_eq!(prog.stmts.len(), 1);
                assert_eq!(prog.stmts[0].to_string(), case.parsed_pp);
            } else {
                panic!("Parser failed!");
            }
        }
    }

    #[test]
    fn parse_two_exprs_error() {
        let input = "\"foo\" \"bar\"";
        let output = Parser::parse(input.to_string());
        match output {
            Err(e) => {
                match e.kind() {
                    super::ParseErrorKind::ExpectedSemicolon => {},
                    _ => panic!("Expected `ExpectedSemicolon` error, got {}", e.kind()),
                }
            }
            Ok(_) => panic!("Expected `ExpectedSemicolon` error, but parsing succeded."),
        }
    }
}
