use crate::mods::lib::expr::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Debug)]
pub enum EvalResult
{
    Integer(i64),
    Boolean(bool),
    Unit,
    #[allow(dead_code)] // We keep the types for pretty printing
    Lambda(Vec<(Id, Type)>, Type, Block),
}

#[derive(Clone)]
pub struct LocalContext
{
    pub curr: HashMap<Id, EvalResult>,
    pub parent: Option<Rc<RefCell<LocalContext>>>,
}

impl LocalContext
{
    fn search(&self, id: &Id) -> Option<EvalResult>
    {
        if let Some(er) = self.curr.get(id) {
            Some(er.clone())
        } else if let Some(par) = &self.parent {
            par.borrow().search(id)
        } else {
            None
        }
    }

    pub fn stack(s: Rc<RefCell<LocalContext>>) -> Self
    {
        LocalContext { curr: HashMap::new(), parent: Some(s) }
    }
}

impl Stmt
{
    fn eval(&self, lctx: &mut LocalContext) -> EvalResult
    {
        match self {
            Stmt::LetStmt(id, expr) => {
                let res = expr.eval(lctx);
                lctx.curr.insert(id.clone(), res);
                EvalResult::Unit
            }
            Stmt::ReturnStmt(expr) => Expr::eval(expr, lctx),
            Stmt::ExprStmt(expr) => Expr::eval(expr, lctx),
            Stmt::BlockStmt(block) => {
                Block::eval(block, lctx)
            }
        }
    }
}

impl Block
{
    pub fn eval(&self, lctx: &mut LocalContext) -> EvalResult
    {
        let mut res = EvalResult::Unit;
        for stmt in self.stmts.iter() {
            res = stmt.eval(lctx);
        }
        res
    }
}

impl Expr
{
    fn eval(&self, lctx: &mut LocalContext) -> EvalResult
    {
        match self {
            Expr::Integer(i) => EvalResult::Integer(*i),
            Expr::Boolean(b) => EvalResult::Boolean(*b),
            Expr::Ident(id) => {
                match lctx.search(id) {
                    None => unreachable!("[evaluator]: identifier not found"),
                    Some(v) => v.clone(),
                }
            }
            Expr::Ite(cond, t, opt_e) =>
                match cond.eval(lctx) {
                    EvalResult::Boolean(true) => Block::eval(t, lctx),
                    EvalResult::Boolean(false) => {
                        match opt_e {
                            None => EvalResult::Unit,
                            Some(e) => Block::eval(e, lctx)
                        }
                    }
                    _ => unreachable!("[evaluator]: ITE without ground boolean condition")
                },
            Expr::Lambda(params, ret, body) => EvalResult::Lambda(params.clone(), ret.clone(), body.clone()),
            Expr::Call(caller, args) => {
                match lctx.search(caller) {
                    None => unreachable!("[evaluator]: unknown symbol {caller}"),
                    Some(EvalResult::Lambda(params, typ, body)) => {
                        let mut evaluated_args = vec![];
                        for arg in args.into_iter() {
                            let lctx_cln = lctx.clone().into();
                            let mut new_ctx = LocalContext::stack(Rc::new(lctx_cln));
                            let evaluated_arg = arg.eval(&mut new_ctx);
                            evaluated_args.push(evaluated_arg);
                        }
                        let mut new_map : HashMap<Id, EvalResult> = HashMap::new();
                        for (i, arg) in evaluated_args.into_iter().enumerate() {
                            let id = params[i].0.clone();
                            new_map.insert(id, arg);
                        }
                        let f = EvalResult::Lambda(params, typ, body.clone());
                        new_map.insert(caller.to_string(), f);
                        let mut new_ctx = LocalContext { curr: new_map, parent: None };
                        body.eval(&mut new_ctx)
                    },
                    Some(_) => unreachable!("[evaluator]: {caller} is not a function")
                }
            },
            Expr::PrefixOp(PrefixOperator::Bang, b_) => {
                match b_.eval(lctx) {
                    EvalResult::Boolean(b) => EvalResult::Boolean(!b),
                    _ => unreachable!("[evaluator]: !b, where b is not bool"),
                }
            },
            Expr::PrefixOp(PrefixOperator::Minus, n_) => {
                match n_.eval(lctx) {
                    EvalResult::Integer(n) => EvalResult::Integer(-n),
                    _ => unreachable!("[evaluator]: -n, where n is not int"),
                }
            },
            Expr::InfixOp(InfixOperator::Plus, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n + m),
                    _ => unreachable!("[evaluator]: n + m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Minus, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n - m),
                    _ => unreachable!("[evaluator]: n - m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Mult, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n * m),
                    _ => unreachable!("[evaluator]: n * m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Div, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n / m),
                    _ => unreachable!("[evaluator]: n / m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::LT, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n < m),
                    _ => unreachable!("[evaluator]: n < m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::GT, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n > m),
                    _ => unreachable!("[evaluator]: n > m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Eq, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n == m),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => EvalResult::Boolean(n == m),
                    _ => unreachable!("[evaluator]: n == m, where n and m are not both int or bool"),
                }
            },
            Expr::InfixOp(InfixOperator::Neq, n_, m_) => {
                match (n_.eval(lctx), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n != m),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => EvalResult::Boolean(n != m),
                    _ => unreachable!("[evaluator]: n != m, where n and m are not both int or bool"),
                }
            },
        }
    }
}
