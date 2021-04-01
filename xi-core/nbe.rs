/* We introduce semantical judgments for xi. We introduce the
syntax_to_semantics and semantics_to_syntax function needed for
normalization by evaluation. This is attempt two after mu. */

use crate::judgment::Primitive;
use crate::judgment::{Judgment, JudgmentKind, Metadata};
use std::rc::Rc;
use xi_uuid::VarUuid;

#[derive(Clone)]
pub enum SJudgment<T, S> {
    FreeVar(VarUuid, Box<SJudgment<T, S>>),
    BoundVar(u32, Box<SJudgment<T, S>>),
    Syn(Judgment<T, S>),
    Lam(
        Box<SJudgment<T, S>>,
        Rc<dyn Fn(SJudgment<T, S>) -> SJudgment<T, S>>,
    ),
    Pi(
        Box<SJudgment<T, S>>,
        Rc<dyn Fn(SJudgment<T, S>) -> SJudgment<T, S>>,
    ),
}

impl<T: Primitive, S: Metadata> SJudgment<T, S> {
    #[allow(non_snake_case)]
    pub fn semantics_to_syntax(sem: SJudgment<T, S>) -> Judgment<T, S> {
        fn up<T: Primitive, S: Metadata>(syn: Judgment<T, S>) -> SJudgment<T, S> {
            let syn_clone = syn.clone();
            match syn.type_of() {
                Some(type_of_syn) => match type_of_syn.tree {
                    JudgmentKind::Type => SJudgment::Syn(syn),
                    JudgmentKind::Pi(var_type, _expr) => SJudgment::Lam(
                        Box::new(SJudgment::Syn(*var_type)),
                        Rc::new(move |S| {
                            up(Judgment::app_unchecked(syn_clone.clone(), down(S), None))
                        }),
                    ),

                    JudgmentKind::Lam(_, _) => panic!("shoudn't see Lambda on type of syn"),
                    JudgmentKind::BoundVar(_, _) => SJudgment::Syn(syn),
                    JudgmentKind::Application(_, _) => SJudgment::Syn(syn),
                    JudgmentKind::Prim(_) => SJudgment::Syn(syn),
                    JudgmentKind::FreeVar(_, _) => SJudgment::Syn(syn),
                },
                None => SJudgment::Syn(syn),
            }
        }

        fn down<T: Primitive, S: Metadata>(sem: SJudgment<T, S>) -> Judgment<T, S> {
            match sem {
                SJudgment::Syn(judgment) => judgment,
                SJudgment::Lam(svar_type, func) => {
                    let free_var = VarUuid::new();
                    let expr = down(func(up(Judgment::free(
                        free_var,
                        down(*svar_type.clone()),
                        None,
                    ))));
                    let expr_rebound = Judgment::rebind(expr, free_var);

                    Judgment::lam(down(*svar_type.clone()), expr_rebound, None)
                }
                SJudgment::Pi(svar_type, func) => {
                    let free_var = VarUuid::new();
                    let expr = down(func(up(Judgment::free(
                        free_var,
                        down(*svar_type.clone()),
                        None,
                    ))));
                    let expr_rebound = Judgment::rebind(expr, free_var);

                    Judgment::pi(down(*svar_type.clone()), expr_rebound, None)
                }
                SJudgment::FreeVar(free_var, var_type) => {
                    Judgment::free(free_var, down(*var_type), None)
                }
                SJudgment::BoundVar(var_index, var_type) => {
                    Judgment::bound_var(var_index, down(*var_type), None)
                }
            }
        }

        down(sem)
    }

    #[allow(non_snake_case)]
    pub fn syntax_to_semantics<U: Semantics<T> + Primitive>(
        syn: Judgment<T, S>,
        ctx: Vec<SJudgment<U, S>>,
    ) -> SJudgment<U, S> {
        #[allow(non_snake_case)]
        fn add_to_ctx<U: Clone>(v: Vec<U>, x: &U) -> Vec<U> {
            let mut v = v;
            v.push(x.clone());
            v
        }

        let ctx_clone = ctx.clone();
        let ctx_clone2 = ctx.clone();
        match syn.tree {
            JudgmentKind::Type => SJudgment::Syn(Judgment::u(None)),
            JudgmentKind::Pi(var_type, expr) => SJudgment::Pi(
                Box::new(SJudgment::syntax_to_semantics(
                    *var_type,
                    // HACK: variable indicies are off by one in both var_type and body.
                    // So we add something to context that should never be read.
                    add_to_ctx(ctx, &SJudgment::Syn(Judgment::u(None))),
                )),
                Rc::new(move |S| {
                    SJudgment::syntax_to_semantics(*expr.clone(), add_to_ctx(ctx_clone.clone(), &S))
                }),
            ),
            JudgmentKind::Lam(var_type, expr) => SJudgment::Lam(
                Box::new(SJudgment::syntax_to_semantics(
                    *var_type,
                    // HACK
                    add_to_ctx(ctx, &SJudgment::Syn(Judgment::u(None))),
                )),
                Rc::new(move |S| {
                    SJudgment::syntax_to_semantics(
                        *expr.clone(),
                        add_to_ctx(ctx_clone2.clone(), &S),
                    )
                }),
            ),
            JudgmentKind::BoundVar(i, var_type) => {
                if ctx.len() < i as usize {
                    SJudgment::BoundVar(i, Box::new(SJudgment::syntax_to_semantics(*var_type, ctx)))
                } else {
                    ctx[ctx.len() - 1 - i as usize].clone()
                }
            }
            JudgmentKind::Application(func, elem) => {
                match SJudgment::syntax_to_semantics(*func, ctx.clone()) {
                    SJudgment::Syn(a) => {
                        panic!("syntax_to_semantics(func) should match to Lam {:?}", a)
                    }
                    SJudgment::Lam(_, sfunc) => sfunc(SJudgment::syntax_to_semantics(*elem, ctx)),
                    SJudgment::Pi(_, _) => panic!("syntax_to_semantics(func) should match to Lam"),
                    SJudgment::FreeVar(_, _) => {
                        panic!("syntax_to_semantics(func) should match to Lam")
                    }
                    SJudgment::BoundVar(_, _) => {
                        panic!("syntax_to_semantics(func) should match to Lam")
                    }
                }
            }
            JudgmentKind::Prim(prim) => U::meaning(prim),
            JudgmentKind::FreeVar(free_var, var_type) => SJudgment::FreeVar(
                free_var,
                Box::new(SJudgment::syntax_to_semantics(*var_type, ctx)),
            ),
        }
    }
}

pub trait Semantics<T>
where
    Self: Sized,
{
    fn meaning<S: Metadata>(prim: T) -> SJudgment<Self, S>;
}

impl<T: Primitive> Semantics<T> for T {
    fn meaning<S: Metadata>(prim: T) -> SJudgment<Self, S> {
        fn add_to_ctx<U: Clone>(v: Vec<U>, x: &U) -> Vec<U> {
            let mut v = v;
            v.push(x.clone());
            v
        }
        fn meaning_rec<T: Primitive, S: Metadata>(
            prim: T,
            type_of: Judgment<T, S>,
            ctx: Vec<SJudgment<T, S>>,
        ) -> SJudgment<T, S> {
            let ctx_clone = ctx.clone();
            match type_of.tree {
                JudgmentKind::Pi(var_type, expr) => SJudgment::Lam(
                    Box::new(SJudgment::syntax_to_semantics(
                        *var_type,
                        // HACK
                        add_to_ctx(ctx, &SJudgment::Syn(Judgment::u(None))),
                    )),
                    Rc::new(move |s| {
                        meaning_rec(
                            prim.clone(),
                            *expr.clone(),
                            add_to_ctx(ctx_clone.clone(), &s),
                        )
                    }),
                ),
                _ => SJudgment::Syn(appn(prim, ctx)),
            }
        }

        fn appn<T: Primitive, S: Metadata>(prim: T, ctx: Vec<SJudgment<T, S>>) -> Judgment<T, S> {
            let mut ctx = ctx;
            if ctx.len() == 0 {
                Judgment::prim(prim, None)
            } else {
                // TODO: simplify?
                let var = SJudgment::semantics_to_syntax(ctx.pop().unwrap());
                if ctx.len() == 0 {
                    Judgment::app_unchecked(Judgment::prim(prim.clone(), None), var, None)
                } else {
                    Judgment::app_unchecked(appn(prim.clone(), ctx), var, None)
                }
            }
        }
        meaning_rec(prim.clone(), prim.type_of(), vec![])
    }
}

#[rustfmt::skip::macros(term)]
#[allow(non_snake_case)]
mod test {

    #[test]
    fn test_nbe() {
        use super::*;
        use crate::judgment::NatPrim;
        use xi_proc_macro::term;
        use NatPrim::{Add, NatType};

        let id: Judgment<NatPrim, ()> = term!(Lam |T : U| T);
        assert_eq!(id.clone().nbe(), id);

        let id_on_term: Judgment<NatPrim, ()> = term!(Lam |T : U, t : T| t);
        assert_eq!(id_on_term.clone().nbe(), id_on_term);

        let unit: Judgment<NatPrim, ()> = term!(Pi |T : U| T -> T);
        assert_eq!(unit.clone().nbe(), unit);

        let unit_test = term!((Lam |T : U| T) {unit.clone()});
        assert_eq!(unit, unit_test.nbe());

        let add: Judgment<NatPrim, ()> = term!([NatPrim::Add]).nbe();
        assert_eq!(add, term!(Lam |x : [NatType], y : [NatType]| [Add] x y));
    }
    #[test]
    fn test_app() {
        use super::*;
        use crate::judgment::NatPrim;
        use xi_proc_macro::term;
        use NatPrim::{Add, Nat};

        let add3: Judgment<NatPrim, ()> = term!([Add] [Nat(3)]);
        let err = std::panic::catch_unwind(|| term!({add3} U));
        assert!(err.is_err());
    }
}
