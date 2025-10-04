
#[derive(PartialEq, Debug, Clone)]
pub enum Type
{
    Integer,
    Boolean,
    Unit,
    Arrow(Box<Type>, Box<Type>)
}

impl ToString for Type
{
    fn to_string(&self) -> String
    {
        match self {
            Type::Integer => "int".to_string(),
            Type::Boolean => "bool".to_string(),
            Type::Unit    => "unit".to_string(),
            Type::Arrow(t1, t2) => t1.to_string() + " -> " + &t2.to_string()
        }
    }
}

pub type Id = String;

#[derive(PartialEq, Debug)]
pub enum Stmt
{
    LetStmt(Id, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr),
    BlockStmt(Block)
}

impl ToString for Stmt
{
    fn to_string(&self) -> String
    {
        use Stmt::*;
        match self {
            LetStmt(id, e) => format!("let {} = {};", id, e.to_string()),
            ReturnStmt(e) => format!("return {};", e.to_string()),
            ExprStmt(e) => e.to_string(),
            BlockStmt(b) => b.to_string()
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

impl ToString for Block {
    fn to_string(&self) -> String
    {
        self.stmts.iter().map(Stmt::to_string).collect::<Vec<_>>().concat()
    }
}

#[derive(PartialEq, Debug)]
pub enum PrefixOperator { Minus, Bang }

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
pub enum InfixOperator { Plus, Minus, Mult, Div, Eq, Neq, LT, GT }

impl ToString for InfixOperator {
    fn to_string(&self) -> String
    {
        use InfixOperator::*;
        match self {
            Plus => "+".to_string(),
            Minus => "-".to_string(),
            Mult => "*".to_string(),
            Div => "/".to_string(),
            Eq => "==".to_string(),
            Neq => "!=".to_string(),
            LT => "<".to_string(),
            GT => ">".to_string(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Ident(Id),
    Integer(i64),
    Boolean(bool),
    Ite(Box<Expr>, Block, Option<Block>),
    Lambda(Vec<(Id, Type)>, Block),
    Call(Id, Vec<Expr>),
    PrefixOp(PrefixOperator, Box<Expr>),
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
            Lambda(params, body) => {
                let typed_ids = params.into_iter().map(|(id, typ)| id.to_owned() + ": " + &typ.to_string() ).collect::<Vec<_>>();
                format!("fn ({}) {{ {} }}", typed_ids.join(", "), body.to_string())
            }
            Call(name, args) => format!("{}({})", name, args.iter().map(Expr::to_string).collect::<Vec<_>>().join(", ")),
            PrefixOp(op, arg) => format!("({}{})", op.to_string(), arg.to_string()),
            InfixOp(op, lhs, rhs) => format!("({} {} {})", lhs.to_string(), op.to_string(), rhs.to_string())
        }
    }
}
