/* We introduce semantical judgments for xi. We introduce the
syntax_to_semantics and semantics_to_syntax function needed for
normalization by evaluation. This is attempt two after mu. */

use crate::judgment::Judgment;
use crate::judgment::Primitive;
use free_var::FreeVar;
use std::rc::Rc;

#[derive(Clone)]
pub enum SJudgment<T> {
    FreeVar(FreeVar, Box<SJudgment<T>>),
    Syn(Judgment<T>),
    Lam(Box<SJudgment<T>>, Rc<dyn Fn(SJudgment<T>) -> SJudgment<T>>),
    Pi(Box<SJudgment<T>>, Rc<dyn Fn(SJudgment<T>) -> SJudgment<T>>),
}

impl<T: Primitive> SJudgment<T> {
    #[allow(non_snake_case)]
    pub fn semantics_to_syntax(sem: SJudgment<T>) -> Judgment<T> {
        fn up<T: Primitive>(syn: Judgment<T>) -> SJudgment<T> {
            let syn_clone = syn.clone();
            match syn.type_of() {
                Some(type_of_syn) => match type_of_syn {
                    Judgment::UInNone => SJudgment::Syn(syn),
                    Judgment::Pi(var_type, _expr) => SJudgment::Lam(
                        Box::new(SJudgment::Syn(*var_type)),
                        Rc::new(move |S| up(Judgment::app_unchecked(syn_clone.clone(), down(S)))),
                    ),

                    Judgment::Lam(_, _) => panic!("shoudn't see Lambda on type of syn"),
                    Judgment::BoundVar(_, _) => SJudgment::Syn(syn),
                    Judgment::Application(_, _) => SJudgment::Syn(syn),
                    Judgment::Prim(_) => SJudgment::Syn(syn),
                    Judgment::FreeVar(_, _) => SJudgment::Syn(syn),
                },
                None => SJudgment::Syn(syn),
            }
        }

        fn down<T: Primitive>(sem: SJudgment<T>) -> Judgment<T> {
            match sem {
                SJudgment::Syn(judgment) => judgment,
                SJudgment::Lam(svar_type, func) => {
                    let free_var = FreeVar::new();
                    let expr = down(func(up(Judgment::free(free_var, down(*svar_type.clone())))));
                    let expr_rebound = Judgment::rebind(expr, free_var);

                    Judgment::lam(down(*svar_type.clone()), expr_rebound)
                }
                SJudgment::Pi(svar_type, func) => {
                    let free_var = FreeVar::new();
                    let expr = down(func(up(Judgment::free(free_var, down(*svar_type.clone())))));
                    let expr_rebound = Judgment::rebind(expr, free_var);

                    Judgment::pi(down(*svar_type.clone()), expr_rebound)
                }
                SJudgment::FreeVar(free_var, var_type) => {
                    Judgment::FreeVar(free_var, Box::new(down(*var_type)))
                }
            }
        }

        down(sem)
    }

    #[allow(non_snake_case)]
    pub fn syntax_to_semantics<U: Semantics<T> + Primitive>(
        syn: Judgment<T>,
        ctx: Vec<SJudgment<U>>,
    ) -> SJudgment<U> {
        #[allow(non_snake_case)]
        fn add_to_ctx<U: Clone>(v: Vec<U>, x: &U) -> Vec<U> {
            let mut v = v;
            v.push(x.clone());
            v
        }

        let ctx_clone = ctx.clone();
        let ctx_clone2 = ctx.clone();
        match syn {
            Judgment::UInNone => SJudgment::Syn(Judgment::UInNone),
            Judgment::Pi(var_type, expr) => SJudgment::Pi(
                Box::new(SJudgment::syntax_to_semantics(
                    *var_type,
                    add_to_ctx(ctx, &SJudgment::Syn(Judgment::UInNone)),
                )),
                Rc::new(move |S| {
                    SJudgment::syntax_to_semantics(*expr.clone(), add_to_ctx(ctx_clone.clone(), &S))
                }),
            ),
            Judgment::Lam(var_type, expr) => SJudgment::Lam(
                Box::new(SJudgment::syntax_to_semantics(
                    *var_type,
                    add_to_ctx(ctx, &SJudgment::Syn(Judgment::UInNone)),
                )),
                Rc::new(move |S| {
                    SJudgment::syntax_to_semantics(
                        *expr.clone(),
                        add_to_ctx(ctx_clone2.clone(), &S),
                    )
                }),
            ),
            Judgment::BoundVar(i, _var_type) => ctx[ctx.len() - 1 - i as usize].clone(),
            Judgment::Application(func, elem) => {
                match SJudgment::syntax_to_semantics(*func, ctx.clone()) {
                    SJudgment::Syn(a) => {
                        panic!("syntax_to_semantics(func) should match to Lam {:?}", a)
                    }
                    SJudgment::Lam(_, sfunc) => sfunc(SJudgment::syntax_to_semantics(*elem, ctx)),
                    SJudgment::Pi(_, _) => panic!("syntax_to_semantics(func) should match to Lam"),
                    SJudgment::FreeVar(_, _) => {
                        panic!("syntax_to_semantics(func) should match to Lam")
                    }
                }
            }
            Judgment::Prim(prim) => U::meaning(prim),
            Judgment::FreeVar(free_var, var_type) => SJudgment::FreeVar(
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
    fn meaning(prim: T) -> SJudgment<Self>;
}

impl<T: Primitive> Semantics<T> for T {
    fn meaning(prim: T) -> SJudgment<Self> {
        fn add_to_ctx<U: Clone>(v: Vec<U>, x: &U) -> Vec<U> {
            let mut v = v;
            v.push(x.clone());
            v
        }
        fn meaning_rec<T: Primitive>(
            prim: T,
            type_of: Judgment<T>,
            ctx: Vec<SJudgment<T>>,
        ) -> SJudgment<T> {
            // match type_of {
            //     Judgment::Pi(var_type, expr) => SJudgment::Lam(
            //         Box::new(SJudgment::syntax_to_semantics(
            //             *var_type,
            //             add_to_ctx(ctx.clone(), &SJudgment::Syn(Judgment::UInNone)),
            //         )),
            //         Rc::new(move |s| {
            //             SJudgment::Syn(Judgment::app_unchecked(
            //                 SJudgment::semantics_to_syntax(meaning_rec(
            //                     prim.clone(),
            //                     *expr.clone(),
            //                     add_to_ctx(ctx.clone(), &s.clone()),
            //                 )),
            //                 SJudgment::semantics_to_syntax(s),
            //             ))
            //         }),
            //     ),
            //     _ => SJudgment::Syn(Judgment::prim(prim)),
            // }
            let ctx_clone = ctx.clone();
            match type_of {
                Judgment::Pi(var_type, expr) => SJudgment::Lam(
                    Box::new(SJudgment::syntax_to_semantics(
                        *var_type,
                        add_to_ctx(ctx, &SJudgment::Syn(Judgment::UInNone)),
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

        fn appn<T: Primitive>(prim: T, ctx: Vec<SJudgment<T>>) -> Judgment<T> {
            let mut ctx = ctx;
            if ctx.len() == 0 {
                Judgment::prim(prim)
            } else {
                let var = SJudgment::semantics_to_syntax(ctx.pop().unwrap());
                if ctx.len() == 0 {
                    Judgment::app_unchecked(Judgment::prim(prim.clone()), var)
                } else {
                    Judgment::app_unchecked(appn(prim.clone(), ctx), var)
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
        use term_macro::term;
        use NatPrim::{Add, NatType};

        let id: Judgment<NatPrim> = term!(Lam |T : U| T);
        assert_eq!(id.clone().nbe(), id);

        let id_on_term: Judgment<NatPrim> = term!(Lam |T : U, t : T| t);
        assert_eq!(id_on_term.clone().nbe(), id_on_term);

        let unit: Judgment<NatPrim> = term!(Pi |T : U| T -> T);
        assert_eq!(unit.clone().nbe(), unit);

        let unit_test = term!((Lam |T : U| T) {unit.clone()});
        assert_eq!(unit, unit_test.nbe());

        assert_eq!(
            term!([NatPrim::Add]).nbe(),
            term!(Lam |x : [NatType], y : [NatType]| [Add] x y),
        );
    }
    #[test]
    fn test_app() {
        use super::*;
        use crate::judgment::NatPrim;
        use term_macro::term;
        use NatPrim::{Add, Nat};

        let add3 = term!([Add] [Nat(3)]);
        let err = std::panic::catch_unwind(|| term!({add3} U));
        assert!(err.is_err());
    }
}
