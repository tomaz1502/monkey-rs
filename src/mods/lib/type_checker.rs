use crate::mods::lib::expr::*;

use std::collections::HashMap;

impl Block
{
    fn tc(&self, ctx: &mut HashMap<Id, Type>) -> Option<Type>
    {
        None
    }
}

impl Expr
{
    fn build_arrow(params: &[(Id, Type)], codom : Type) -> Type
    {
        match &params[..] {
            [] => codom.clone(),
            [prv_types @ .., last_type] => {
                Self::build_arrow(prv_types, Type::Arrow(Box::new(last_type.1.clone()), Box::new(codom)))
            }
        }
    }

    fn check_call(caller_type: Type, arg_types : &[Type]) -> Option<Type>
    {
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

    fn tc(&self, ctx: &mut HashMap<Id, Type>) -> Option<Type>
    {
        match self {
            Expr::Ident(id) => ctx.get(id).map(|typ| { typ.clone() }),
            Expr::Integer(_) => Some(Type::Integer),
            Expr::Boolean(_) => Some(Type::Boolean),
            Expr::Ite(cond, t, opt_e) => {
                let t_cond = cond.tc(ctx)?;
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
            Expr::Lambda(params, body) => {
                for (id, typ) in params {
                    ctx.insert(id.clone(), typ.clone());
                }
                let t_block = body.tc(ctx)?;
                Some(Self::build_arrow(&params, t_block))
            }
            Expr::Call(caller, args) => {
                let caller_type = ctx.get(caller)?.clone();
                let mut arg_types = vec![];
                for arg in args {
                    arg_types.push(arg.tc(ctx)?);
                }
                Self::check_call(caller_type, &arg_types)
            },
            Expr::PrefixOp(PrefixOperator::Minus, e) =>
                if e.tc(ctx) == Some(Type::Integer) { Some(Type::Integer) } else { None },
            Expr::PrefixOp(PrefixOperator::Bang, e) =>
                if e.tc(ctx) == Some(Type::Boolean) { Some(Type::Boolean) } else { None },
            Expr::InfixOp(InfixOperator::Eq, e1, e2) | Expr::InfixOp(InfixOperator::Neq, e1, e2) => {
                let te1 = e1.tc(ctx)?;
                let te2 = e2.tc(ctx)?;
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
                let te1 = e1.tc(ctx)?;
                let te2 = e2.tc(ctx)?;
                if te1 == te2 && te1 == Type::Integer {
                    Some(Type::Integer)
                } else {
                    None
                }
            }
            Expr::InfixOp(InfixOperator::GT, e1, e2) |
            Expr::InfixOp(InfixOperator::LT, e1, e2) => {
                let te1 = e1.tc(ctx)?;
                let te2 = e2.tc(ctx)?;
                if te1 == te2 && te1 == Type::Integer {
                    Some(Type::Boolean)
                } else {
                    None
                }
            }
        }
    }
}
