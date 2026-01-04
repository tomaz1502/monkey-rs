use crate::mods::lib::expr::*;

use std::collections::HashMap;

impl Stmt {
    fn tc(&self, ctx: &mut HashMap<Id, Type>) -> Option<Type> {
        match self {
            Stmt::Let(id, expr) => {
                let t_expr = expr.tc(ctx, &Some(id.to_string()))?;
                // NOTE: Scopes are broken; we should have a stack of contexts
                ctx.insert(id.clone(), t_expr);
                Some(Type::Unit)
            }
            Stmt::Expr(expr) => expr.tc(ctx, &None),
            Stmt::Return(expr) => expr.tc(ctx, &None),
            Stmt::Block(block) => block.tc(ctx),
        }
    }
}

impl Block {
    pub fn tc(&self, ctx: &mut HashMap<Id, Type>) -> Option<Type> {
        let mut typ = Some(Type::Unit);
        for stmt in &self.stmts {
            typ = Some(stmt.tc(ctx)?);
        }
        typ
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

    fn tc(&self, ctx: &mut HashMap<Id, Type>, opt_let_name : &Option<String>) -> Option<Type> {
        match self {
            Expr::Ident(id) => ctx.get(id).cloned(),
            Expr::Integer(_) => Some(Type::Integer),
            Expr::Boolean(_) => Some(Type::Boolean),
            Expr::Ite(cond, t, opt_e) => {
                let t_cond = cond.tc(ctx, opt_let_name)?;
                if t_cond == Type::Boolean {
                    let tt = t.tc(ctx)?;
                    let te =
                        match opt_e {
                            Some(e) => e.tc(ctx)?,
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
            Expr::Lambda(params, ret, body) => {
                let expected_type = Self::build_arrow(params, ret);
                let mut new_ctx = ctx.clone();
                if let Some(let_name) = opt_let_name {
                    new_ctx.insert(let_name.clone(), expected_type.clone());
                }
                for (id, typ) in params {
                    new_ctx.insert(id.clone(), typ.clone());
                }
                let t_block = body.tc(&mut new_ctx)?;
                if t_block != *ret {
                    None
                } else {
                    Some(expected_type)
                }
            }
            Expr::Call(caller, args) => {
                let caller_type = ctx.get(caller)?.clone();
                let mut arg_types = vec![];
                for arg in args {
                    arg_types.push(arg.tc(ctx, opt_let_name)?);
                }
                Self::check_call(caller_type, &arg_types)
            },
            Expr::PrefixOp(PrefixOperator::Minus, e) =>
                if e.tc(ctx, opt_let_name) == Some(Type::Integer) { Some(Type::Integer) } else { None },
            Expr::PrefixOp(PrefixOperator::Bang, e) =>
                if e.tc(ctx, opt_let_name) == Some(Type::Boolean) { Some(Type::Boolean) } else { None },
            Expr::InfixOp(InfixOperator::Eq, e1, e2) | Expr::InfixOp(InfixOperator::Neq, e1, e2) => {
                let te1 = e1.tc(ctx, opt_let_name)?;
                let te2 = e2.tc(ctx, opt_let_name)?;
                if te1 == te2 {
                    Some(Type::Boolean)
                } else {
                    None
                }
            }
            Expr::InfixOp(InfixOperator::Plus, e1, e2) |
            Expr::InfixOp(InfixOperator::Minus, e1, e2) |
            Expr::InfixOp(InfixOperator::Mult, e1, e2) |
            Expr::InfixOp(InfixOperator::Div, e1, e2) => {
                let te1 = e1.tc(ctx, opt_let_name)?;
                let te2 = e2.tc(ctx, opt_let_name)?;
                if te1 == te2 && te1 == Type::Integer {
                    Some(Type::Integer)
                } else {
                    None
                }
            }
            Expr::InfixOp(InfixOperator::GT, e1, e2) |
            Expr::InfixOp(InfixOperator::LT, e1, e2) => {
                let te1 = e1.tc(ctx, opt_let_name)?;
                let te2 = e2.tc(ctx, opt_let_name)?;
                if te1 == te2 && te1 == Type::Integer {
                    Some(Type::Boolean)
                } else {
                    None
                }
            }
        }
    }
}
