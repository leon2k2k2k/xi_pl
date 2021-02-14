/* We introduce semantical judgments for xi. We introduce the
syntax_to_semantics and semantics_to_syntax function needed for
normalization by evaluation. This is attempt two after mu. */
use crate::free_var::FreeVar;
use crate::xi_syntax::Judgment;
use std::rc::Rc;

#[derive(Clone)]
pub enum SJudgment {
    Syn(Judgment),
    Lam(Box<SJudgment>, Rc<dyn Fn(SJudgment) -> SJudgment>),
    Pi(Box<SJudgment>, Rc<dyn Fn(SJudgment) -> SJudgment>),
}

pub fn semantics_to_syntax(sem: SJudgment) -> Judgment {
    fn up(syn: Judgment) -> SJudgment {
        let syn_clone = syn.clone();
        match syn.type_of() {
            Some(type_of_syn) => match type_of_syn {
                Judgment::UInNone => SJudgment::Syn(syn),
                Judgment::Pi(var_type, _expr) => SJudgment::Lam(
                    Box::new(SJudgment::Syn(*var_type)),
                    Rc::new(move |S| {
                        up(Judgment::Application(
                            Box::new(syn_clone.clone()),
                            Box::new(down(S)),
                        ))
                    }),
                ),

                Judgment::Lam(_, _) => panic!("shoudn't see Lambda on type of syn"),
                Judgment::BoundVar(_, _) => SJudgment::Syn(syn),
                Judgment::Application(_, _) => SJudgment::Syn(syn),
            },
            None => SJudgment::Syn(syn),
        }
    }

    fn down(sem: SJudgment) -> Judgment {
        fn rebind(s: Judgment, free_var: u32) -> Judgment {
            fn rebind_rec(s: Judgment, free_var: u32, depth: u32) -> Judgment {
                match s {
                    Judgment::UInNone => Judgment::UInNone,
                    Judgment::Pi(var_type, expr) => Judgment::pi(
                        rebind_rec(*var_type, free_var, depth + 1),
                        rebind_rec(*expr, free_var, depth + 1),
                    ),
                    Judgment::Lam(var_type, expr) => Judgment::lam(
                        rebind_rec(*var_type, free_var, depth + 1),
                        rebind_rec(*expr, free_var, depth + 1),
                    ),
                    Judgment::BoundVar(i, var_type) => {
                        if i == free_var {
                            Judgment::BoundVar(depth, var_type)
                        } else {
                            Judgment::boundvar(i, rebind_rec(*var_type, free_var, depth))
                        }
                    }
                    Judgment::Application(lhs, rhs) => Judgment::app_strict(
                        rebind_rec(*lhs, free_var, depth),
                        rebind_rec(*rhs, free_var, depth),
                    ),
                }
            }
            rebind_rec(s, free_var, 0)
        }

        match sem {
            SJudgment::Syn(judgment) => judgment,
            SJudgment::Lam(svar_type, func) => {
                let free_var = FreeVar::new().0;
                let expr = down(func(up(Judgment::boundvar(
                    free_var,
                    down(*svar_type.clone()),
                ))));
                let expr_rebound = rebind(expr, free_var);

                Judgment::lam(down(*svar_type.clone()), expr_rebound)
            }
            SJudgment::Pi(svar_type, func) => {
                let free_var = FreeVar::new().0;
                let expr = down(func(up(Judgment::boundvar(
                    free_var,
                    down(*svar_type.clone()),
                ))));
                let expr_rebound = rebind(expr, free_var);

                Judgment::pi(down(*svar_type.clone()), expr_rebound)
            }
        }
    }

    down(sem)
}

fn add_to_ctx(v: Vec<SJudgment>, x: &SJudgment) -> Vec<SJudgment> {
    let mut v = v;
    v.push(x.clone());
    v
}

pub fn syntax_to_semantics(syn: Judgment, ctx: Vec<SJudgment>) -> SJudgment {
    let ctx_clone = ctx.clone();
    let ctx_clone2 = ctx.clone();
    match syn {
        Judgment::UInNone => SJudgment::Syn(Judgment::UInNone),
        Judgment::Pi(var_type, expr) => SJudgment::Pi(
            Box::new(syntax_to_semantics(
                *var_type,
                add_to_ctx(ctx, &SJudgment::Syn(Judgment::UInNone)),
            )),
            Rc::new(move |S| syntax_to_semantics(*expr.clone(), add_to_ctx(ctx_clone.clone(), &S))),
        ),
        Judgment::Lam(var_type, expr) => SJudgment::Lam(
            Box::new(syntax_to_semantics(
                *var_type,
                add_to_ctx(ctx, &SJudgment::Syn(Judgment::UInNone)),
            )),
            Rc::new(move |S| {
                syntax_to_semantics(*expr.clone(), add_to_ctx(ctx_clone2.clone(), &S))
            }),
        ),
        Judgment::BoundVar(i, _var_type) => ctx[ctx.len() - 1 - i as usize].clone(),
        Judgment::Application(func, elem) => match syntax_to_semantics(*func, ctx.clone()) {
            SJudgment::Syn(_) => panic!("syntax_to_semantics(func) should match to Lam"),
            SJudgment::Lam(_, sfunc) => sfunc(syntax_to_semantics(*elem, ctx)),
            SJudgment::Pi(_, _) => panic!("syntax_to_semantics(func) should match to Lam"),
        },
    }
}

mod test {
    use super::*;

    #[test]
    fn test_nbe() {
        use Judgment::*;
        let id = Judgment::lam(
            Judgment::UInNone,
            Judgment::BoundVar(0, Box::new(Judgment::UInNone)),
        );
        assert_eq!(id.clone().nbe(), id);

        let id_on_term = Judgment::lam(
            Judgment::u(),
            Judgment::lam(
                Judgment::boundvar(1, Judgment::u()),
                Judgment::boundvar(0, Judgment::boundvar(1, Judgment::u())),
            ),
        );
        assert_eq!(id_on_term.clone().nbe(), id_on_term);

        let unit = Judgment::pi(
            Judgment::u(),
            Judgment::pi(
                Judgment::boundvar(1, Judgment::u()),
                Judgment::boundvar(1, Judgment::u()),
            ),
        );
        assert_eq!(Judgment::app_strict(id, unit.clone()).nbe(), unit);
    }
}
