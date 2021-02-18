/* We introduce semantical judgments for xi. We introduce the
syntax_to_semantics and semantics_to_syntax function needed for
normalization by evaluation. This is attempt two after mu. */

use crate::xi_syntax::Judgment;
use crate::xi_syntax::Primitive;
use free_var::FreeVar;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Clone)]
pub enum SJudgment<T> {
    FreeVar(FreeVar, Box<SJudgment<T>>),
    Syn(Judgment<T>),
    Lam(Box<SJudgment<T>>, Rc<dyn Fn(SJudgment<T>) -> SJudgment<T>>),
    Pi(Box<SJudgment<T>>, Rc<dyn Fn(SJudgment<T>) -> SJudgment<T>>),
}

impl<T: Primitive + Clone + PartialEq + Eq + 'static + Debug> SJudgment<T> {
    #[allow(non_snake_case)]
    pub fn semantics_to_syntax(sem: SJudgment<T>) -> Judgment<T> {
        fn up<T: Primitive + Clone + PartialEq + Eq + 'static + Debug>(
            syn: Judgment<T>,
        ) -> SJudgment<T> {
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

        fn down<T: Primitive + Clone + PartialEq + Eq + 'static + Debug>(
            sem: SJudgment<T>,
        ) -> Judgment<T> {
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
    pub fn syntax_to_semantics<
        U: Semantics<T> + Primitive + Clone + PartialEq + Eq + 'static + Debug,
    >(
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
                Box::new(SJudgment::syntax_to_semantics(
                    Judgment::FreeVar(free_var, Box::new(*var_type)),
                    ctx,
                )),
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

impl<T: Primitive + Clone + PartialEq + Eq + Debug + 'static> Semantics<T> for T {
    fn meaning(prim: T) -> SJudgment<Self> {
        fn add_to_ctx<U: Clone>(v: Vec<U>, x: &U) -> Vec<U> {
            let mut v = v;
            v.push(x.clone());
            v
        }
        fn meaning_rec<T: Primitive + Clone + PartialEq + Eq + Debug + 'static>(
            prim: T,
            type_of: Judgment<T>,
            var_list: Vec<Judgment<T>>,
            ctx: Vec<SJudgment<T>>,
        ) -> SJudgment<T> {
            let var_list_clone = var_list.clone();
            let ctx_clone = ctx.clone();
            match type_of {
                Judgment::Pi(var_type, expr) => SJudgment::Lam(
                    Box::new(SJudgment::syntax_to_semantics(
                        *var_type,
                        add_to_ctx(ctx, &SJudgment::Syn(Judgment::UInNone)),
                    )),
                    Rc::new(move |s| match s {
                        SJudgment::Syn(s_) => meaning_rec(
                            prim.clone(),
                            *expr.clone(),
                            add_to_ctx(var_list_clone.clone(), &s_),
                            add_to_ctx(ctx_clone.clone(), &SJudgment::Syn(s_)),
                        ),
                        _ => panic!("only can be syn"),
                    }),
                ),
                _ => SJudgment::Syn(appn(prim, var_list)),
            }
        }

        fn appn<T: Primitive + Clone + PartialEq + Eq + Debug + 'static>(
            prim: T,
            var_list: Vec<Judgment<T>>,
        ) -> Judgment<T> {
            let mut var_list = var_list;
            if var_list.len() == 0 {
                Judgment::prim(prim)
            } else {
                let var = var_list.pop();
                if var_list.len() == 0 {
                    Judgment::app_unchecked(Judgment::prim(prim.clone()), var.unwrap())
                } else {
                    Judgment::app_unchecked(appn(prim.clone(), var_list), var.unwrap())
                }
            }
        }
        meaning_rec(prim.clone(), prim.type_of(), vec![], vec![])
    }
}

#[rustfmt::skip::macros(term)]
#[allow(non_snake_case)]
mod test {

    #[test]
    fn test_nbe() {
        use super::*;
        use crate::xi_syntax::NatPrim;
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
    #[should_panic]
    fn test_app() {
        use super::*;
        use crate::xi_syntax::NatPrim;
        use term_macro::term;
        use NatPrim::{Add, Nat};

        let add3 = term!([Add] [Nat(3)]);
        term!({add3} U);
    }
}
