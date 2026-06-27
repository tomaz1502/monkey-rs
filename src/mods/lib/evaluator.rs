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
        //                             Global context
        Context { bindings_stack: vec![HashMap::new()] }
    }

    fn lookup(&self, id: &Id) -> Option<EvalResult> {
        for i in (0..self.bindings_stack.len()).rev() {
            if let Some(t) = self.bindings_stack[i].get(id) {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn scope_eval(&mut self, bindings: Vec<(Id, EvalResult)>, block: &Block) -> Option<(EvalResult, bool)> {
        let mut cur_bindings = HashMap::new();
        for (id, typ) in bindings {
            cur_bindings.insert(id, typ);
        }
        self.bindings_stack.push(cur_bindings);
        let mut res = (EvalResult::Unit, false);
        for stmt in &block.stmts {
            res = self.eval(stmt)?;
            if res.1 {
                self.bindings_stack.pop();
                return Some(res);
            }
        }
        self.bindings_stack.pop();
        Some(res)
    }
}

pub trait Evaluate<T> {
    // TODO: Should be result
    fn eval(&mut self, t: &T) -> Option<(EvalResult, bool)>;
}

impl Evaluate<Stmt> for Context {
    fn eval(&mut self, stmt: &Stmt) -> Option<(EvalResult, bool)> {
        match stmt {
            Stmt::Let(id, expr) => {
                let res = self.eval(expr)?;
                self
                  .bindings_stack
                  .last_mut()
                  .expect("let without a context")
                  .insert(id.clone(), res.0);
                Some((EvalResult::Unit, false))
            }
            Stmt::Return(expr)  => {
                let (val, _) = self.eval(expr)?;
                return Some((val, true));
            }
            Stmt::Expr(expr)    => self.eval(expr),
            Stmt::Block(block)  => {
                self.scope_eval(vec![], block)
            }
        }
    }
}

// The only reason we have to return the early return flag here is because if is an expression
impl Evaluate<Expr> for Context {
    fn eval(&mut self, expr: &Expr) -> Option<(EvalResult, bool)> {
        match expr {
            Expr::Integer(i) => Some((EvalResult::Integer(*i), false)),
            Expr::Boolean(b) => Some((EvalResult::Boolean(*b), false)),
            Expr::Char(c)    => Some((EvalResult::Char(*c), false)),
            Expr::Str(s)     => Some((EvalResult::Str(s.clone()), false)),
            Expr::Unit       => Some((EvalResult::Unit, false)),
            Expr::Ident(id) => {
                let val = self.lookup(id)?;
                return Some((val, false));
            }
            Expr::Ite(cond, t, opt_e) =>
                match self.eval(&**cond)? {
                    // TODO: The typech
                    (val, true) => return Some((val, true)),
                    (EvalResult::Boolean(true), false) => self.scope_eval(vec![], t),
                    (EvalResult::Boolean(false), false) => {
                        match opt_e {
                            None => Some((EvalResult::Unit, false)),
                            Some(e) => self.scope_eval(vec![], e),
                        }
                    }
                    _ => None,
                },
            Expr::Lambda(params, ret, body) => Some((EvalResult::Lambda(params.clone(), ret.clone(), body.clone()), false)),
            Expr::Call(caller, args) => {
                let mut evaluated_args: Vec<EvalResult> = Vec::new();
                for arg in args.iter() {
                    match self.eval(arg)? {
                        (val, true) => return Some((val, true)),
                        (val, false) => evaluated_args.push(val),
                    };
                }
                match &**caller {
                    Expr::Lambda(params, _, body) => {
                        // TODO: abstract this code
                        // NOTE: `caller` is already there
                        // NOTE: No need to check arity since we already type checked
                        let mut cur_bindings = vec![];
                        for (i, arg) in evaluated_args.into_iter().enumerate() {
                            let id = params[i].0.clone();
                            cur_bindings.push((id, arg));
                        }
                        // Here is the limit of propagation for the return flag
                        let (val, _) = self.scope_eval(cur_bindings, body)?;
                        Some((val, false))
                    }
                    Expr::Ident(caller) => {
                        match &caller[..] {
                            // NOTE: the correctness of the compiler relies on the fact that
                            // this internal implementation returns the correct type; this
                            // is not guaranteed by the type checker.
                            "print" => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s)] => {
                                        print!("{}", s);
                                        Some((EvalResult::Unit, false))
                                    },
                                    _ => unreachable!("impossible (TC)")
                                }
                            },
                            "read" => {
                                let mut s = String::new();
                                std::io::stdin().read_line(&mut s).unwrap();
                                Some((EvalResult::Str(s), false))
                            }
                            "len" => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s)] => Some((EvalResult::Integer(s.len() as i64), false)),
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            "getSlice" => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s), EvalResult::Integer(i), EvalResult::Integer(j)] => {
                                        let t = String::from(&s[(*i as usize)..(*j as usize)]);
                                        Some((EvalResult::Str(t), false))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            "getElem" => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s), EvalResult::Integer(i)] => {
                                        Some((EvalResult::Char(s.chars().nth(*i as usize)?), false))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            "concat" => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s1), EvalResult::Str(s2)] => {
                                        let t = format!("{}{}", s1, s2);
                                        Some((EvalResult::Str(t), false))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            "strOfChar" => {
                                match &evaluated_args[..] {
                                    [EvalResult::Char(c)] => {
                                        Some((EvalResult::Str(String::from(*c)), false))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            _ => {
                                match self.lookup(caller) {
                                    Some(EvalResult::Lambda(params, _, body)) => {
                                        let mut cur_bindings = vec![];
                                        for (i, arg) in evaluated_args.into_iter().enumerate() {
                                            let id = params[i].0.clone();
                                            cur_bindings.push((id, arg));
                                        }
                                        let (val, _) = self.scope_eval(cur_bindings, &body)?;
                                        // Here is the limit of propagation for the return flag
                                        Some((val, false))
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
                    (val, true) => Some((val, true)),
                    (EvalResult::Str(s), false) => {
                        match self.eval(&**idx)? {
                            (EvalResult::Integer(i), false) => {
                                if i < 0 || i >= (s.len() as i64) {
                                    None
                                } else {
                                    Some((EvalResult::Char(s.chars().nth(i as usize)?), false))
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
                    (val, true)                     => return Some((val, true)),
                    (EvalResult::Boolean(b), false) => Some((EvalResult::Boolean(!b), false)),
                    _ => None,
                }
            },
            Expr::PrefixOp(PrefixOperator::Minus, n_) => {
                match self.eval(&**n_)? {
                    (val, true)                     => return Some((val, true)),
                    (EvalResult::Integer(n), false) => Some((EvalResult::Integer(-n), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Plus, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Integer(n + m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Minus, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Integer(n - m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Mult, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Integer(n * m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Div, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Integer(n / m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Mod, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Integer(n % m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::LT, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Boolean(n < m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::GT, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Boolean(n > m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Eq, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Boolean(n == m), false)),
                    ((EvalResult::Boolean(n), _), (EvalResult::Boolean(m), _)) => Some((EvalResult::Boolean(n == m), false)),
                    ((EvalResult::Char(n), _),    (EvalResult::Char(m), _))    => Some((EvalResult::Boolean(n == m), false)),
                    _ => None,
                }
            },
            Expr::InfixOp(InfixOperator::Neq, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    ((val, true), _) => Some((val, true)),
                    (_, (val, true)) => Some((val, true)),
                    ((EvalResult::Integer(n), _), (EvalResult::Integer(m), _)) => Some((EvalResult::Boolean(n != m), false)),
                    ((EvalResult::Boolean(n), _), (EvalResult::Boolean(m), _)) => Some((EvalResult::Boolean(n != m), false)),
                    ((EvalResult::Char(n), _),    (EvalResult::Char(m), _))    => Some((EvalResult::Boolean(n != m), false)),
                    _ => None,
                }
            },
        }
    }
}

impl Evaluate<Block> for Context {
    fn eval(&mut self, b: &Block) -> Option<(EvalResult, bool)> {
        self.scope_eval(vec![], b)
    }
}

#[cfg(test)]
mod tests {
    // TODO
}
