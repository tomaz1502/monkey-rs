use crate::mods::lib::expr::*;

use std::collections::HashMap;

// Normalized exprs
#[derive(Clone, Debug)]
pub enum EvalResult {
    Integer(i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Unit,
    #[allow(dead_code)] // We keep the types for pretty printing
    Lambda(Vec<(Id, Type)>, Type, Block),
}

#[derive(Clone)]
pub struct Context {
    pub bindings_stack: Vec<HashMap<Id, EvalResult>>,
}

impl Context {
    pub fn new() -> Self {
        Context { bindings_stack: vec![] }
    }

    fn lookup(&self, id: &Id) -> Option<EvalResult> {
        for i in (0..self.bindings_stack.len()).rev() {
            if let Some(t) = self.bindings_stack[i].get(id) {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn scope_eval(&mut self, bindings: Vec<(Id, EvalResult)>, block: &Block) -> Option<EvalResult> {
        let mut cur_bindings = HashMap::new();
        for (id, typ) in bindings {
            cur_bindings.insert(id, typ);
        }
        self.bindings_stack.push(cur_bindings);
        let mut res = Some(EvalResult::Unit);
        for stmt in &block.stmts {
            res = self.eval(stmt);
        }
        self.bindings_stack.pop();
        res
    }
}

pub trait Evaluate<T> {
    // TODO: Should be result
    fn eval(&mut self, t: &T) -> Option<EvalResult>;
}

impl Evaluate<Stmt> for Context {
    fn eval(&mut self, stmt: &Stmt) -> Option<EvalResult> {
        match stmt {
            Stmt::Let(id, expr) => {
                let res = self.eval(expr)?;
                self
                  .bindings_stack
                  .last_mut()
                  .expect("let without a context")
                  .insert(id.clone(), res);
                Some(EvalResult::Unit)
            }
            Stmt::Return(expr)  => self.eval(expr),
            Stmt::Expr(expr)    => self.eval(expr),
            Stmt::Block(block)  => {
                self.scope_eval(vec![], block)
            }
        }
    }
}

impl Evaluate<Expr> for Context {
    fn eval(&mut self, expr: &Expr) -> Option<EvalResult> {
        match expr {
            Expr::Integer(i) => Some(EvalResult::Integer(*i)),
            Expr::Boolean(b) => Some(EvalResult::Boolean(*b)),
            Expr::Char(c)    => Some(EvalResult::Char(*c)),
            Expr::Str(s)     => Some(EvalResult::Str(s.clone())),
            Expr::Unit       => Some(EvalResult::Unit),
            Expr::Ident(id) => self.lookup(id),
            Expr::Ite(cond, t, opt_e) =>
                match self.eval(&**cond)? {
                    EvalResult::Boolean(true) => self.scope_eval(vec![], t),
                    EvalResult::Boolean(false) => {
                        match opt_e {
                            None => Some(EvalResult::Unit),
                            Some(e) => self.scope_eval(vec![], e),
                        }
                    }
                    _ => None,
                },
            Expr::Lambda(params, ret, body) => Some(EvalResult::Lambda(params.clone(), ret.clone(), body.clone())),
            Expr::Call(caller, args) => {
                match &**caller {
                    Expr::Lambda(params, _, body) => {
                        // TODO: abstract this code
                        let evaluated_args = args.iter().map(|arg| self.eval(arg)).collect::<Option<Vec<_>>>()?;
                        // NOTE: `caller` is already there
                        // NOTE: No need to check arity since we already type checked
                        let mut cur_bindings = vec![];
                        for (i, arg) in evaluated_args.into_iter().enumerate() {
                            let id = params[i].0.clone();
                            cur_bindings.push((id, arg));
                        }
                        self.scope_eval(cur_bindings, &body)
                    }
                    Expr::Ident(caller) => {
                        match &caller[..] {
                            "print" => {
                                let evaluated_args = args.iter().map(|arg| self.eval(arg)).collect::<Option<Vec<_>>>()?;
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s)] => {
                                        print!("{}", s);
                                        Some(EvalResult::Unit)
                                    },
                                    _ => unreachable!("impossible (TC)")
                                }
                            },
                            "read" => {
                                let mut s = String::new();
                                std::io::stdin().read_line(&mut s).unwrap();
                                Some(EvalResult::Str(s))
                            }
                            "len" => {
                                let evaluated_args = args.iter().map(|arg| self.eval(arg)).collect::<Option<Vec<_>>>()?;
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s)] => {
                                        Some(EvalResult::Integer(s.len() as i64))
                                    },
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            _ => {
                                match self.lookup(caller) {
                                    Some(EvalResult::Lambda(params, _, body)) => {
                                        let evaluated_args = args.iter().map(|arg| self.eval(arg)).collect::<Option<Vec<_>>>()?;
                                        let mut cur_bindings = vec![];
                                        for (i, arg) in evaluated_args.into_iter().enumerate() {
                                            let id = params[i].0.clone();
                                            cur_bindings.push((id, arg));
                                        }
                                        self.scope_eval(cur_bindings, &body)
                                    },
                                    _ => None
                                }
                            }
                        }
                    }
                    _ => None
                }
            },
            Expr::IndexedAccess(arr, idx) => {
                match self.eval(&**arr)? {
                    EvalResult::Str(s) => {
                        match self.eval(&**idx)? {
                            EvalResult::Integer(i) => {
                                if i < 0 || i >= (s.len() as i64) {
                                    None
                                } else {
                                    Some(EvalResult::Char(s.chars().nth(i as usize)?))
                                }
                            }
                            _ => None,
                        }
                    }
                    _ => None
                }
            }
            Expr::PrefixOp(PrefixOperator::Bang, b_) => {
                match self.eval(&**b_)? {
                    EvalResult::Boolean(b) => Some(EvalResult::Boolean(!b)),
                    _ => None,
                }
            },
            Expr::PrefixOp(PrefixOperator::Minus, n_) => {
                match self.eval(&**n_)? {
                    EvalResult::Integer(n) => Some(EvalResult::Integer(-n)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Plus, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Integer(n + m)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Minus, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Integer(n - m)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Mult, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Integer(n * m)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Div, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Integer(n / m)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::LT, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Boolean(n < m)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::GT, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Boolean(n > m)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Eq, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Boolean(n == m)),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => Some(EvalResult::Boolean(n == m)),
                    (EvalResult::Char(n), EvalResult::Char(m))       => Some(EvalResult::Boolean(n == m)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Neq, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Some(EvalResult::Boolean(n != m)),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => Some(EvalResult::Boolean(n != m)),
                    (EvalResult::Char(n), EvalResult::Char(m))       => Some(EvalResult::Boolean(n != m)),
                    _ => None,
                }
            },
        }
    }
}

impl Evaluate<Block> for Context {
    fn eval(&mut self, b: &Block) -> Option<EvalResult> {
        self.scope_eval(vec![], b)
    }
}

#[cfg(test)]
mod tests {
    // TODO
}
