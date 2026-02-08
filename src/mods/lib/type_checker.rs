// TODO: implement tests, specially testing the scopes
use crate::mods::lib::expr::*;

use std::collections::HashMap;

static EQUALITY_TYPES: [Type; 3] = [Type::Integer, Type::Boolean, Type::Char];

pub struct Context {
    bindings_stack: Vec<HashMap<Id, Type>>,
    // NOTE: I wish this was static and shared with the evaluator, but
    // types are recursive and can't have their size known at compile time
    builtins: HashMap<String, Type>,
    // This represents the current name that is being defined via a `let`.
    // It wasn't introduced in the bindings yet since we still don't know its type,
    // but we still need it, for instance, for recursive definitions.
    curr_let_def: Option<String>,
}

impl Context {
    pub fn new() -> Self {
        let builtins = HashMap::from([
            ("print".to_string(), Type::Arrow(Box::new(Type::Str), Box::new(Type::Unit))),
            ("read".to_string(), Type::Arrow(Box::new(Type::Unit), Box::new(Type::Str))),
        ]);
        Context { builtins, bindings_stack: vec![], curr_let_def: None }
    }

    fn lookup(&self, id: &Id) -> Option<Type> {
        for i in (0..self.bindings_stack.len()).rev() {
            if let Some(t) = self.bindings_stack[i].get(id) {
                return Some(t.clone());
            }
        }
        None
    }

    fn lookup_builtin(&self, id: &Id) -> Option<Type> {
        self.builtins.get(id).cloned()
    }

    fn lookup_full(&self, id: &Id) -> Option<Type> {
        self.lookup_builtin(id).or(self.lookup(id))
    }

    pub fn scope_tc(&mut self, bindings: Vec<(Id, Type)>, block: &Block) -> Option<Type> {
        let mut cur_bindings = HashMap::new();
        for (id, typ) in bindings {
            cur_bindings.insert(id, typ);
        }
        self.bindings_stack.push(cur_bindings);
        let mut typ = Some(Type::Unit);
        for stmt in &block.stmts {
            typ = self.tc(stmt);
        }
        self.bindings_stack.pop();
        typ
    }
}

pub trait TypeCheck<T> {
    fn tc(&mut self, t: &T) -> Option<Type>;
}

impl TypeCheck<Stmt> for Context {
    fn tc(&mut self, stmt: &Stmt) -> Option<Type> {
        match stmt {
            Stmt::Let(id, expr) => {
                self.curr_let_def = Some(id.to_string());
                let t_expr = self.tc(expr)?;
                self.curr_let_def = None;
                self
                  .bindings_stack
                  .last_mut()
                  .expect("let without a context")
                  .insert(id.clone(), t_expr);
                Some(Type::Unit)
            }
            Stmt::Expr(expr) => self.tc(expr),
            Stmt::Return(expr) => self.tc(expr),
            Stmt::Block(block) => self.scope_tc(vec![], block),
        }
    }
}

impl Expr {
    fn build_arrow_aux(params: &[(Id, Type)], codom : &Type) -> Type {
        match params {
            [] => codom.clone(),
            [prv_types @ .., last_type] => {
                Self::build_arrow_aux(prv_types, &Type::Arrow(Box::new(last_type.1.clone()), Box::new(codom.clone())))
            }
        }
    }

    fn build_arrow(params: &[(Id, Type)], codom : &Type) -> Type {
        if params.is_empty() { // create a thunk
            Type::Arrow(Box::new(Type::Unit), Box::new(codom.clone()))
        } else {
            Self::build_arrow_aux(params, codom)
        }
    }

    fn check_call(caller_type: Type, arg_types : &[Type]) -> Option<Type> {
        match (caller_type, arg_types) {
            (Type::Arrow(t1, t2), [hd, tl @ ..]) => {
                if *t1 == *hd {
                    Self::check_call(*t2, tl)
                } else {
                    None
                }
            }
            (t, []) => Some(t),
            _ => None
        }
    }
}

impl TypeCheck<Expr> for Context {
    fn tc(&mut self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Ident(id)  => {
                self.lookup_full(id)
            }
            Expr::Integer(_) => Some(Type::Integer),
            Expr::Boolean(_) => Some(Type::Boolean),
            Expr::Char(_)    => Some(Type::Char),
            Expr::Str(_)    => Some(Type::Str),
            Expr::Unit      => Some(Type::Unit),
            Expr::Lambda(params, ret, body) => {
                let expected_type = Expr::build_arrow(params, ret);
                let mut scoped_bindings = params.clone();
                if let Some(let_name) = &self.curr_let_def {
                    scoped_bindings.push((let_name.clone(), expected_type.clone()));
                }
                let t_block = self.scope_tc(scoped_bindings, body)?;
                if t_block != *ret {
                    None
                } else {
                    Some(expected_type)
                }
            }
            Expr::Ite(cond, t, opt_e) => {
                if let Type::Boolean = self.tc(&**cond)? {
                    let tt = self.scope_tc(vec![], t)?;
                    let te =
                        match opt_e {
                            Some(e) => self.scope_tc(vec![], e)?,
                            None => Type::Unit
                        };
                    if tt == te {
                        Some(tt)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expr::Call(caller, args) => {
                let caller_type = self.lookup_full(caller)?;
                let mut arg_types = vec![];
                for arg in args {
                    arg_types.push(self.tc(arg)?);
                }
                Expr::check_call(caller_type, &arg_types)
            },
            Expr::PrefixOp(PrefixOperator::Minus, e) =>
                if self.tc(&**e) == Some(Type::Integer) { Some(Type::Integer) } else { None },
            Expr::PrefixOp(PrefixOperator::Bang, e) =>
                if self.tc(&**e) == Some(Type::Boolean) { Some(Type::Boolean) } else { None },
            Expr::InfixOp(InfixOperator::Eq, e1, e2) | Expr::InfixOp(InfixOperator::Neq, e1, e2) => {
                let te1 = self.tc(&**e1)?;
                let te2 = self.tc(&**e2)?;
                if te1 == te2 {
                    if EQUALITY_TYPES.contains(&te1) {
                        Some(Type::Boolean)
                    } else {
                        None // NOTE: Give a different typing error here then in the other branch
                    }
                } else {
                    None
                }
            }
            Expr::InfixOp(InfixOperator::Plus, e1, e2)  |
            Expr::InfixOp(InfixOperator::Minus, e1, e2) |
            Expr::InfixOp(InfixOperator::Mult, e1, e2)  |
            Expr::InfixOp(InfixOperator::Div, e1, e2) => {
                let te1 = self.tc(&**e1)?;
                let te2 = self.tc(&**e2)?;
                if te1 == te2 && te1 == Type::Integer {
                    Some(Type::Integer)
                } else {
                    None
                }
            }
            Expr::InfixOp(InfixOperator::GT, e1, e2) |
            Expr::InfixOp(InfixOperator::LT, e1, e2) => {
                let te1 = self.tc(&**e1)?;
                let te2 = self.tc(&**e2)?;
                if te1 == te2 && te1 == Type::Integer {
                    Some(Type::Boolean)
                } else {
                    None
                }
            }
        }
    }
}

impl TypeCheck<Block> for Context {
    fn tc(&mut self, b: &Block) -> Option<Type> {
        self.scope_tc(vec![], b)
    }
}

#[cfg(test)]
mod tests {
    // TODO
}
