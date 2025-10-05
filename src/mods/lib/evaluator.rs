use crate::mods::lib::expr::*;

use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum EvalResult {
    Integer(i64),
    Boolean(bool),
    Unit,
    #[allow(dead_code)] // We keep the types for pretty printing
    Lambda(Vec<(Id, Type)>, Type, Block),
}

impl Stmt
{
    fn eval(&self, ctx: &mut HashMap<Id, EvalResult>) -> EvalResult
    {
        match self {
            Stmt::LetStmt(id, expr) => {
                let res = Expr::eval(expr, ctx);
                ctx.insert(id.clone(), res);
                EvalResult::Unit
            }
            Stmt::ReturnStmt(expr) => Expr::eval(expr, ctx),
            Stmt::ExprStmt(expr) => Expr::eval(expr, ctx),
            Stmt::BlockStmt(block) => {
                Block::eval(block, ctx)
            }
        }
    }
}

impl Block
{
    pub fn eval(&self, ctx: &mut HashMap<Id, EvalResult>) -> EvalResult
    {
        let mut res = EvalResult::Unit;
        for stmt in self.stmts.iter() {
            res = stmt.eval(ctx);
        }
        res
    }
}

impl Expr
{
    fn eval(&self, ctx: &mut HashMap<Id, EvalResult>) -> EvalResult
    {
        match self {
            Expr::Integer(i) => EvalResult::Integer(*i),
            Expr::Boolean(b) => EvalResult::Boolean(*b),
            Expr::Ident(id) => {
                match ctx.get(id) {
                    None => unreachable!("[evaluator]: identifier not found"),
                    Some(v) => v.clone(),
                }
            }
            Expr::Ite(cond, t, opt_e) =>
                match cond.eval(ctx) {
                    EvalResult::Boolean(true) => Block::eval(t, ctx),
                    EvalResult::Boolean(false) => {
                        match opt_e {
                            None => EvalResult::Unit,
                            Some(e) => Block::eval(e, ctx)
                        }
                    }
                    _ => unreachable!("[evaluator]: ITE without ground boolean condition")
                },
            Expr::Lambda(params, ret, body) => EvalResult::Lambda(params.clone(), ret.clone(), body.clone()),
            Expr::Call(caller, args) => {
                match ctx.get(caller) {
                    None => unreachable!("[evaluator]: unknown symbol {caller}"),
                    Some(EvalResult::Lambda(params, _, body)) => {
                        // Call by value
                        let evaluated_args =
                            args.into_iter().map(|arg| { arg.eval(&mut ctx.clone()) }).collect::<Vec<_>>();
                        let mut new_ctx = ctx.clone();
                        let mut i = 0;
                        for arg in evaluated_args.into_iter() {
                            let (id, _) = &params[i];
                            new_ctx.insert(id.clone(), arg);
                            i += 1;
                        }
                        body.eval(&mut new_ctx)
                    },
                    Some(_) => unreachable!("[evaluator]: {caller} is not a function")
                }
            },
            Expr::PrefixOp(PrefixOperator::Bang, b_) => {
                match b_.eval(ctx) {
                    EvalResult::Boolean(b) => EvalResult::Boolean(!b),
                    _ => unreachable!("[evaluator]: !b, where b is not bool"),
                }
            },
            Expr::PrefixOp(PrefixOperator::Minus, n_) => {
                match n_.eval(ctx) {
                    EvalResult::Integer(n) => EvalResult::Integer(-n),
                    _ => unreachable!("[evaluator]: -n, where n is not int"),
                }
            },
            Expr::InfixOp(InfixOperator::Plus, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n + m),
                    _ => unreachable!("[evaluator]: n + m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Minus, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n - m),
                    _ => unreachable!("[evaluator]: n - m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Mult, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n * m),
                    _ => unreachable!("[evaluator]: n * m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Div, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n / m),
                    _ => unreachable!("[evaluator]: n / m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::LT, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n < m),
                    _ => unreachable!("[evaluator]: n < m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::GT, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n > m),
                    _ => unreachable!("[evaluator]: n > m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Eq, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n == m),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => EvalResult::Boolean(n == m),
                    _ => unreachable!("[evaluator]: n == m, where n and m are not both int or bool"),
                }
            },
            Expr::InfixOp(InfixOperator::Neq, n_, m_) => {
                match (n_.eval(ctx), m_.eval(ctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n != m),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => EvalResult::Boolean(n != m),
                    _ => unreachable!("[evaluator]: n != m, where n and m are not both int or bool"),
                }
            },
        }
    }
}
