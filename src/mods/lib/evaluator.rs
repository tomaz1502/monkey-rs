use crate::mods::lib::expr::*;
use crate::mods::lib::utils::BuiltinSymbol;

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
    Builtin(BuiltinSymbol),
}

pub enum EvalSignal {
    EarlyReturn(EvalResult),
    RuntimeError,
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

    pub fn scope_eval(&mut self, bindings: Vec<(Id, EvalResult)>, block: &Block) -> Result<EvalResult, EvalSignal> {
        let mut cur_bindings = HashMap::new();
        for (id, typ) in bindings {
            cur_bindings.insert(id, typ);
        }
        self.bindings_stack.push(cur_bindings);
        let mut res = Ok(EvalResult::Unit);
        for stmt in &block.stmts {
            match self.eval(stmt) {
                Err(err) => {
                    res = Err(err);
                    break;
                }
                cur_res => { res = cur_res; }
            };
        }
        self.bindings_stack.pop();
        res
    }
}

pub trait Evaluate<T> {
    fn eval(&mut self, t: &T) -> Result<EvalResult, EvalSignal>;
}

impl Evaluate<Stmt> for Context {
    fn eval(&mut self, stmt: &Stmt) -> Result<EvalResult, EvalSignal> {
        match stmt {
            Stmt::Let(id, expr) => {
                let res = self.eval(expr)?;
                self
                  .bindings_stack
                  .last_mut()
                  .expect("let without a context")
                  .insert(id.clone(), res);
                Ok(EvalResult::Unit)
            }
            Stmt::Return(expr)  => {
                let val = self.eval(expr)?;
                return Err(EvalSignal::EarlyReturn(val));
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
    fn eval(&mut self, expr: &Expr) -> Result<EvalResult, EvalSignal> {
        match expr {
            Expr::Integer(i) => Ok(EvalResult::Integer(*i)),
            Expr::Boolean(b) => Ok(EvalResult::Boolean(*b)),
            Expr::Char(c)    => Ok(EvalResult::Char(*c)),
            Expr::Str(s)     => Ok(EvalResult::Str(s.clone())),
            Expr::Unit       => Ok(EvalResult::Unit),
            Expr::Ident(id) => {
                if let Some(b) = expr.to_builtin() {
                    return Ok(EvalResult::Builtin(b))
                }
                let val = self.lookup(id).ok_or(EvalSignal::RuntimeError)?;
                return Ok(val);
            }
            Expr::Ite(cond, t, opt_e) =>
                match self.eval(&**cond)? {
                    EvalResult::Boolean(true) => self.scope_eval(vec![], t),
                    EvalResult::Boolean(false) => {
                        match opt_e {
                            None => Ok(EvalResult::Unit),
                            Some(e) => self.scope_eval(vec![], e),
                        }
                    }
                    _ => Err(EvalSignal::RuntimeError), // unreachable (type checker)
                },
            Expr::Lambda(params, ret, body) => Ok(EvalResult::Lambda(params.clone(), ret.clone(), body.clone())),
            Expr::Call(caller, args) => {
                let evaluated_args = args.iter().map(|arg| self.eval(arg)).collect::<Result<Vec<_>, _>>()?;
                let evaluated_caller = self.eval(&**caller)?;
                match evaluated_caller {
                    EvalResult::Lambda(params, _, body) => {
                        // TODO: abstract this code
                        // NOTE: `caller` is already there
                        // NOTE: No need to check arity since we already type checked
                        let mut cur_bindings = vec![];
                        for (i, arg) in evaluated_args.into_iter().enumerate() {
                            let id = params[i].0.clone();
                            cur_bindings.push((id, arg));
                        }
                        // Here is the limit of propagation for the return flag
                        match self.scope_eval(cur_bindings, &body) {
                            Err(EvalSignal::EarlyReturn(val)) => Ok(val),
                            result => result,
                        }
                    }
                    EvalResult::Builtin(builtin) => {
                        match builtin {
                            // NOTE: the correctness of the compiler relies on the fact that
                            // this internal implementation returns the correct type; this
                            // is not guaranteed by the type checker.
                            BuiltinSymbol::Print => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s)] => {
                                        print!("{}", s);
                                        Ok(EvalResult::Unit)
                                    },
                                    _ => unreachable!("impossible (TC)")
                                }
                            },
                            BuiltinSymbol::Read => {
                                let mut s = String::new();
                                std::io::stdin().read_line(&mut s).unwrap();
                                Ok(EvalResult::Str(s))
                            }
                            BuiltinSymbol::Len => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s)] => Ok(EvalResult::Integer(s.len() as i64)),
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            BuiltinSymbol::GetSlice => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s), EvalResult::Integer(i), EvalResult::Integer(j)] => {
                                        let t = String::from(&s[(*i as usize)..(*j as usize)]);
                                        Ok(EvalResult::Str(t))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            BuiltinSymbol::GetElem => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s), EvalResult::Integer(i)] => {
                                        Ok(EvalResult::Char(s.chars().nth(*i as usize).ok_or(EvalSignal::RuntimeError)?))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            BuiltinSymbol::Concat => {
                                match &evaluated_args[..] {
                                    [EvalResult::Str(s1), EvalResult::Str(s2)] => {
                                        let t = format!("{}{}", s1, s2);
                                        Ok(EvalResult::Str(t))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                            BuiltinSymbol::StrOfChar => {
                                match &evaluated_args[..] {
                                    [EvalResult::Char(c)] => {
                                        Ok(EvalResult::Str(String::from(*c)))
                                    }
                                    _ => unreachable!("impossible (TC)")
                                }
                            }
                        }
                    }
                    _ => Err(EvalSignal::RuntimeError)
                }
            },
            Expr::IndexedAccess(arr, idx) => {
                match self.eval(&**arr)? {
                    EvalResult::Str(s) => {
                        match self.eval(&**idx)? {
                            EvalResult::Integer(i) => {
                                if i < 0 || i >= (s.len() as i64) {
                                    Err(EvalSignal::RuntimeError)
                                } else {
                                    Ok(EvalResult::Char(s.chars().nth(i as usize).ok_or(EvalSignal::RuntimeError)?))
                                }
                            }
                            _ => Err(EvalSignal::RuntimeError),
                        }
                    }
                    _ => Err(EvalSignal::RuntimeError)
                }
            }
            Expr::PrefixOp(PrefixOperator::Bang, b_) => {
                match self.eval(&**b_)? {
                    EvalResult::Boolean(b) => Ok(EvalResult::Boolean(!b)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::PrefixOp(PrefixOperator::Minus, n_) => {
                match self.eval(&**n_)? {
                    EvalResult::Integer(n) => Ok(EvalResult::Integer(-n)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::Plus, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Integer(n + m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::Minus, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Integer(n - m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::Mult, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Integer(n * m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::Div, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Integer(n / m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::Mod, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Integer(n % m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::LT, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Boolean(n < m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::GT, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Boolean(n > m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::Eq, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Boolean(n == m)),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => Ok(EvalResult::Boolean(n == m)),
                    (EvalResult::Char(n),    EvalResult::Char(m))    => Ok(EvalResult::Boolean(n == m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
            Expr::InfixOp(InfixOperator::Neq, n_, m_) => {
                match (self.eval(&**n_)?, self.eval(&**m_)?) {
                    (EvalResult::Integer(n), EvalResult::Integer(m)) => Ok(EvalResult::Boolean(n != m)),
                    (EvalResult::Boolean(n), EvalResult::Boolean(m)) => Ok(EvalResult::Boolean(n != m)),
                    (EvalResult::Char(n),    EvalResult::Char(m))    => Ok(EvalResult::Boolean(n != m)),
                    _ => Err(EvalSignal::RuntimeError),
                }
            },
        }
    }
}

impl Evaluate<Block> for Context {
    fn eval(&mut self, b: &Block) -> Result<EvalResult, EvalSignal> {
        self.scope_eval(vec![], b)
    }
}

#[cfg(test)]
mod tests {
    // TODO
}
