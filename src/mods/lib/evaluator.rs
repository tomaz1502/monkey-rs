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

pub struct LocalContext
{
    pub counter: RefCell<i32>,
    pub curr: HashMap<Id, EvalResult>,
    pub parent: Option<Rc<RefCell<LocalContext>>>,
}

impl LocalContext
{
    fn search(&self, id: &Id, depth: i32) -> Option<EvalResult>
    {
        println!("searching with counter: {}", self.get_counter());
        println!("depth: {depth}");
        if let Some(er) = self.curr.get(id) {
            Some(er.clone())
        } else if let Some(par) = &self.parent {
            par.borrow().search(id, depth + 1)
        } else {
            None
        }
    }

    fn get_counter(&self) -> i32 {
        *self.counter.borrow()
    }

    pub fn create_on_top(s: Rc<RefCell<Self>>, mp : HashMap<Id, EvalResult>) -> Self
    {
        let c = s.borrow().get_counter();
        LocalContext { counter: (c + 1).into(), curr: mp, parent: Some(s) }
    }

    pub fn create_empty_on_top(s: Rc<RefCell<Self>>) -> Self
    {
        Self::create_on_top(s, HashMap::new())
    }
}

impl Stmt
{
    fn eval(&self, lctx: Rc<RefCell<LocalContext>>) -> EvalResult
    {
        match self {
            Stmt::LetStmt(id, expr) => {
                let res = Expr::eval(expr, lctx.clone());
                lctx.borrow_mut().curr.insert(id.clone(), res);
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
    pub fn eval(&self, lctx: Rc<RefCell<LocalContext>>) -> EvalResult
    {
        let mut res = EvalResult::Unit;
        for stmt in self.stmts.iter() {
            res = stmt.eval(Rc::clone(&lctx));
        }
        res
    }
}

impl Expr
{
    fn eval(&self, lctx: Rc<RefCell<LocalContext>>) -> EvalResult
    {
        match self {
            Expr::Integer(i) => EvalResult::Integer(*i),
            Expr::Boolean(b) => EvalResult::Boolean(*b),
            Expr::Ident(id) => {
                match lctx.borrow().search(id, 0) {
                    None => unreachable!("[evaluator]: identifier not found"),
                    Some(v) => v.clone(),
                }
            }
            Expr::Ite(cond, t, opt_e) =>
                match cond.eval(Rc::clone(&lctx)) {
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
                match lctx.borrow().search(caller, 0) {
                    None => unreachable!("[evaluator]: unknown symbol {caller}"),
                    Some(EvalResult::Lambda(params, typ, body)) => {
                        // Call by value
                        let mut evaluated_args = vec![];
                        for arg in args.into_iter() {
                            let new_ctx = LocalContext::create_empty_on_top(Rc::clone(&lctx));
                            let evaluated_arg = arg.eval(Rc::new(new_ctx.into()));
                            evaluated_args.push(evaluated_arg);
                        }
                        let mut new_map : HashMap<Id, EvalResult> = HashMap::new();
                        let mut i = 0;
                        for arg in evaluated_args.into_iter() {
                            let id = params[i].0.clone();
                            new_map.insert(id, arg);
                            i += 1;
                        }
                        let f = EvalResult::Lambda(params, typ, body.clone());
                        new_map.insert(caller.to_string(), f);
                        let new_ctx = LocalContext { counter: 0.into(), curr: new_map, parent: None };
                        body.eval(Rc::new(new_ctx.into()))
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
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n + m),
                    _ => unreachable!("[evaluator]: n + m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Minus, n_, m_) => {
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n - m),
                    _ => unreachable!("[evaluator]: n - m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Mult, n_, m_) => {
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n * m),
                    _ => unreachable!("[evaluator]: n * m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Div, n_, m_) => {
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Integer(n / m),
                    _ => unreachable!("[evaluator]: n / m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::LT, n_, m_) => {
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n < m),
                    _ => unreachable!("[evaluator]: n < m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::GT, n_, m_) => {
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n > m),
                    _ => unreachable!("[evaluator]: n > m where n and m are not both int"),
                }
            },
            Expr::InfixOp(InfixOperator::Eq, n_, m_) => {
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n == m),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => EvalResult::Boolean(n == m),
                    _ => unreachable!("[evaluator]: n == m, where n and m are not both int or bool"),
                }
            },
            Expr::InfixOp(InfixOperator::Neq, n_, m_) => {
                match (n_.eval(Rc::clone(&lctx)), m_.eval(lctx)) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => EvalResult::Boolean(n != m),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => EvalResult::Boolean(n != m),
                    _ => unreachable!("[evaluator]: n != m, where n and m are not both int or bool"),
                }
            },
        }
    }
}
