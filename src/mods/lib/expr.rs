use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Integer,
    Boolean,
    Unit,
    Char,
    Str,
    Arrow(Box<Type>, Box<Type>)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
        match self {
            Type::Integer       => write!(f, "int"),
            Type::Boolean       => write!(f, "bool"),
            Type::Unit          => write!(f, "unit"),
            Type::Char          => write!(f, "char"),
            Type::Str           => write!(f, "string"),
            Type::Arrow(t1, t2) => write!(f, "{} -> {}", t1, t2),
        }
    }
}

pub type Id = String;

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    Let(Id, Expr),
    Return(Expr),
    Expr(Expr),
    Block(Block)
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
        use Stmt::*;
        match self {
            Let(id, e) => write!(f, "let {} = {};", id, e),
            Return(e)  => write!(f, "return {};", e),
            Expr(e)    => write!(f, "{}", e),
            Block(b)   => write!(f, "{}", b),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
        for stmt in self.stmts.iter() {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum PrefixOperator { Minus, Bang }

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
        use PrefixOperator::*;
        match self {
            Minus => write!(f, "-"),
            Bang  => write!(f, "!"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum InfixOperator { Plus, Minus, Mult, Div, Eq, Neq, LT, GT }

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
        use InfixOperator::*;
        match self {
            Plus  => write!(f, "+"),
            Minus => write!(f, "-"),
            Mult  => write!(f, "*"),
            Div   => write!(f, "/"),
            Eq    => write!(f, "=="),
            Neq   => write!(f, "!="),
            LT    => write!(f, "<"),
            GT    => write!(f, ">"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ident(Id),
    Integer(i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Ite(Box<Expr>, Block, Option<Block>),
    Lambda(Vec<(Id, Type)>, Type, Block),
    Call(Id, Vec<Expr>), // TODO: should accept any expression as the caller
    PrefixOp(PrefixOperator, Box<Expr>),
    InfixOp(InfixOperator, Box<Expr>, Box<Expr>)
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
        use Expr::*;
        match self {
            Ident(name)               => write!(f, "{}", name),
            Integer(num)              => write!(f, "{}", num),
            Boolean(b)                => write!(f, "{}", b),
            Char(c)                   => write!(f, "'{}'", c),
            Str(s)                    => write!(f, "\"{}\"", s),
            Ite(cond, t, Some(e))     => write!(f, "if ({}) {{ {} }} else {{ {} }}", (*cond), t, e),
            Ite(cond, t, None)        => write!(f, "if ({}) {{ {} }}", (*cond), t),
            Lambda(params, ret, body) => {
                let typed_ids =
                    params.iter().map(|(id, typ)| id.to_owned() + ": " + &typ.to_string()).collect::<Vec<_>>();
                write!(f, "fn ({}) -> {} {{ {} }}", typed_ids.join(", "), ret, body)
            }
            Call(name, args)          => write!(f, "{}({})", name, args.iter().map(Expr::to_string).collect::<Vec<_>>().join(", ")),
            PrefixOp(op, arg)         => write!(f, "({}{})", op, arg),
            InfixOp(op, lhs, rhs)     => write!(f, "({} {} {})", lhs, op, rhs)
        }
    }
}
