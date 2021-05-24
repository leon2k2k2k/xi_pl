/* We introduce semantical judgments for xi. We introduce the
syntax_to_semantics and semantics_to_syntax function needed for
normalization by evaluation. This is attempt two after mu. */

use crate::judgment::Primitive;
use crate::judgment::{Judgment, JudgmentKind};
use std::collections::BTreeMap;
use std::rc::Rc;
use xi_uuid::VarUuid;

#[derive(Clone)]
pub enum SJudgment<T> {
    FreeVar(VarUuid, Box<SJudgment<T>>),
    Syn(Judgment<T>),
    Lam(Box<SJudgment<T>>, Rc<dyn Fn(SJudgment<T>) -> SJudgment<T>>),
    Pi(Box<SJudgment<T>>, Rc<dyn Fn(SJudgment<T>) -> SJudgment<T>>),
}

impl<T: Primitive> SJudgment<T> {
    #[allow(non_snake_case)]
    pub fn semantics_to_syntax(sem: SJudgment<T>) -> Judgment<T> {
        fn up<T: Primitive>(syn: Judgment<T>) -> SJudgment<T> {
            let syn_clone = syn.clone();
            match *syn.type_of().tree {
                JudgmentKind::Type => SJudgment::Syn(syn),
                JudgmentKind::Pi(var_type, _expr) => SJudgment::Lam(
                    Box::new(SJudgment::Syn(var_type)),
                    Rc::new(move |S| up(Judgment::app_unchecked(syn_clone.clone(), down(S), None))),
                ),

                JudgmentKind::Lam(_, _) => panic!("shoudn't see Lambda on type of syn"),
                JudgmentKind::BoundVar(_, _) => SJudgment::Syn(syn),
                JudgmentKind::App(_, _) => SJudgment::Syn(syn),
                JudgmentKind::Prim(_, _) => SJudgment::Syn(syn),
                JudgmentKind::FreeVar(_, _) => SJudgment::Syn(syn),
            }
        }

        fn down<T: Primitive>(sem: SJudgment<T>) -> Judgment<T> {
            match sem {
                SJudgment::Syn(judgment) => judgment,
                SJudgment::Lam(svar_type, func) => {
                    let free_var = VarUuid::new();
                    let expr = down(func(up(Judgment::free(
                        free_var,
                        down(*svar_type.clone()),
                        None,
                    ))));
                    let expr_rebound = expr.bind(free_var);

                    Judgment::lam(down(*svar_type.clone()), expr_rebound, None)
                }
                SJudgment::Pi(svar_type, func) => {
                    let free_var = VarUuid::new();
                    let expr = down(func(up(Judgment::free(
                        free_var,
                        down(*svar_type.clone()),
                        None,
                    ))));
                    let expr_rebound = expr.bind(free_var);

                    Judgment::pi(down(*svar_type.clone()), expr_rebound, None)
                }
                SJudgment::FreeVar(free_var, var_type) => {
                    Judgment::free(free_var, down(*var_type), None)
                }
            }
        }

        down(sem)
    }

    #[allow(non_snake_case)]
    pub fn syntax_to_semantics<U: Semantics<T> + Primitive>(
        syn: Judgment<T>,
        ctx: BTreeMap<VarUuid, SJudgment<U>>,
    ) -> SJudgment<U> {
        #[allow(non_snake_case)]

        fn add_to_ctx<T: Ord, U: Clone>(v: BTreeMap<T, U>, index: T, x: &U) -> BTreeMap<T, U> {
            let mut v = v;
            v.insert(index, x.clone());
            v
        }

        let ctx_clone = ctx.clone();
        match *syn.tree {
            JudgmentKind::Type => SJudgment::Syn(Judgment::u(None)),
            JudgmentKind::Pi(var_type, sexpr) => {
                let (index, expr) = sexpr.unbind();
                SJudgment::Pi(
                    Box::new(SJudgment::syntax_to_semantics(var_type, ctx)),
                    Rc::new(move |S| {
                        SJudgment::syntax_to_semantics(
                            expr.clone(),
                            add_to_ctx(ctx_clone.clone(), index, &S),
                        )
                    }),
                )
            }
            JudgmentKind::Lam(var_type, sexpr) => {
                let (index, expr) = sexpr.unbind();
                SJudgment::Lam(
                    Box::new(SJudgment::syntax_to_semantics(var_type, ctx)),
                    Rc::new(move |S| {
                        SJudgment::syntax_to_semantics(
                            expr.clone(),
                            add_to_ctx(ctx_clone.clone(), index, &S),
                        )
                    }),
                )
            }
            JudgmentKind::BoundVar(_i, _var_type) => {
                unreachable!()
            }
            JudgmentKind::App(func, elem) => {
                match SJudgment::syntax_to_semantics(func.clone(), ctx.clone()) {
                    SJudgment::Lam(_, sfunc) => sfunc(SJudgment::syntax_to_semantics(elem, ctx)),
                    _ => {
                        panic!(
                            "syntax_to_semantics(func) should match to Lam, but func is {:?}",
                            func
                        );
                    }
                }
            }
            JudgmentKind::Prim(prim, prim_type) => U::meaning(prim, prim_type),
            JudgmentKind::FreeVar(index, var_type) => match ctx.get(&index) {
                Some(svar_type) => svar_type.clone(),
                None => eta_expand(
                    Judgment::free(
                        index,
                        SJudgment::semantics_to_syntax(SJudgment::syntax_to_semantics(
                            var_type,
                            ctx.clone(),
                        )),
                        None,
                    ),
                    Context {
                        btreemap: ctx,
                        vec: vec![],
                    },
                ),
            },
        }
    }
}

pub trait Semantics<T>
where
    Self: Sized,
{
    fn meaning(prim: T, prim_type: Judgment<T>) -> SJudgment<Self>;
}
#[derive(Clone)]
pub struct Context<T> {
    pub btreemap: BTreeMap<VarUuid, SJudgment<T>>,
    pub vec: Vec<VarUuid>,
}

impl<T: Primitive> Context<T> {
    pub fn add_to_ctx<U: Clone>(ctx: Context<U>, index: VarUuid, x: &SJudgment<U>) -> Context<U> {
        let mut ctx = ctx;
        ctx.btreemap.insert(index.clone(), x.clone());
        ctx.vec.push(index);
        ctx
    }

    pub fn new<U: Clone>() -> Context<U> {
        Context {
            btreemap: BTreeMap::new(),
            vec: vec![],
        }
    }
}

fn eta_expand<T: Primitive>(expr: Judgment<T>, ctx: Context<T>) -> SJudgment<T> {
    fn eta_expand_rec<T: Primitive>(
        expr: Judgment<T>,
        effective_type: Judgment<T>,
        ctx: Context<T>,
    ) -> SJudgment<T> {
        let ctx_clone = ctx.clone();
        match *effective_type.tree {
            JudgmentKind::Pi(var_type, sexpr) => {
                let (index, new_type) = sexpr.unbind();
                SJudgment::Lam(
                    Box::new(SJudgment::syntax_to_semantics(var_type, ctx.btreemap)),
                    Rc::new(move |s| {
                        eta_expand_rec(
                            expr.clone(),
                            new_type.clone(),
                            Context::<T>::add_to_ctx(ctx_clone.clone(), index, &s),
                        )
                    }),
                )
            }
            _ => SJudgment::Syn(appn(expr, ctx)),
        }
    }

    fn appn<T: Primitive>(expr: Judgment<T>, ctx: Context<T>) -> Judgment<T> {
        let mut ctx = ctx;
        if ctx.vec.len() == 0 {
            expr
        } else {
            let index = ctx.vec.pop().unwrap();
            let var = SJudgment::semantics_to_syntax(ctx.btreemap.get(&index).unwrap().clone());

            Judgment::app_unchecked(appn(expr, ctx), var, None)
        }
    }
    eta_expand_rec(expr.clone(), expr.type_of(), ctx)
}

impl<T: Primitive> Semantics<T> for T {
    fn meaning(prim: T, prim_type: Judgment<T>) -> SJudgment<Self> {
        eta_expand(Judgment::prim(prim, prim_type, None), Context::<T>::new())
    }
}

#[allow(non_snake_case)]
mod test {

    #[test]
    fn test_nbe() {
        use super::*;
        use crate::judgment::NatPrim;
        use xi_proc_macro::term;
        use NatPrim::{Add, NatType};

        let id: Judgment<NatPrim> = term!(Lam | T: U | T);
        assert_eq!(id.clone().nbe(), id);

        let id_on_term: Judgment<NatPrim> = term!(Lam | T: U, t: T | t);
        assert_eq!(id_on_term.clone().nbe(), id_on_term);

        let unit: Judgment<NatPrim> = term!(Pi |T : U| T -> T);
        assert_eq!(unit.clone().nbe(), unit);

        let unit_test = term!((Lam |T : U| T) {unit.clone()});
        assert_eq!(unit, unit_test.nbe());

        let add: Judgment<NatPrim> = term!([NatPrim::Add]).nbe();
        assert_eq!(add, term!(Lam |x : [NatType], y : [NatType]| [Add] x y));
    }
    // #[test]
    // fn test_app() {
    //     use super::*;
    //     use crate::judgment::NatPrim;
    //     use xi_proc_macro::term;
    //     use NatPrim::{Add, Nat};

    //     let add3: Judgment<NatPrim> = term!([Add][Nat(3)]);

    //     let err = std::panic::catch_unwind(|| term!({add3} U));
    //     assert!(err.is_err());
    // }
    // #[test]
    // fn test_nbe_with_primitives() {
    //     use super::*;
    //     use crate::judgment::{NatPrim, Primitive};
    //     use xi_proc_macro::term;
    //     use NatPrim::{Add, Add3, Nat};

    //     let add3: Judgment<NatPrim, ()> = term!([Add3]);
    //     assert_eq!(add3.nbe(), term!(Lam |n : [Nat]| [Add3] n));
    // }
}
