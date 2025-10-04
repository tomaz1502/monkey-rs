use crate::mods::lib::expr::*;

use std::collections::HashMap;

#[derive(Clone)]
enum EvalResult {
    Integer(i64),
    Boolean(bool),
}

impl Stmt
{
    fn eval(&self, ctx: &mut HashMap<Id, EvalResult>) -> Option<EvalResult>
    {
        match self {
            Stmt::LetStmt(id, expr) => {
                let res = Expr::eval(expr, ctx);
                ctx.insert(id.clone(), res);
                None
            }
            Stmt::ReturnStmt(expr) => Some(Expr::eval(expr, ctx)),
            Stmt::ExprStmt(expr) => Some(Expr::eval(expr, ctx)),
            Stmt::BlockStmt(block) => {
                let res = Block::eval(block, ctx)?;
                Some(res)
            }
        }
    }
}

impl Block
{
    fn eval(&self, ctx: &mut HashMap<Id, EvalResult>) -> Option<EvalResult>
    {
        let results = self.stmts.iter().map(|stmt| Stmt::eval(stmt, ctx));
        match results.last()? {
            None => panic!("Last statement in a block should be a return or an expression."),
            Some(res) => Some(res)
        }
    }
}

impl Expr
{
    fn eval(&self, ctx: &mut HashMap<Id, EvalResult>) -> EvalResult
    {
        match &self {
            Expr::Integer(i) => EvalResult::Integer(*i),
            Expr::Boolean(b) => EvalResult::Boolean(*b),
            Expr::Ident(id) => ctx[id].clone(),
            Expr::Ite(cond, t, e) =>
                match cond.eval(ctx) {
                    EvalResult::Boolean(true) => {
                        let res = Block::eval(&t, ctx);
                        match res {
                            None => panic!("Last statement in a block should be a return or an expression."),
                            Some(res) => res
                        }
                    }
                    EvalResult::Boolean(false) => {
                        // match e {
                        //     None => 
                        // }
                        let res = Block::eval(&t, ctx);
                        match res {
                            None => panic!("Last statement in a block should be a return or an expression."),
                            Some(res) => res
                        }
                    }
                    _ => unreachable!() // TODO: type checker
                }
            _ => unreachable!()
        }
    }
}
