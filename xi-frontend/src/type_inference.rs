use std::{collections::BTreeMap, rc::Rc};

use crate::desugar::{text_to_judg_ment, Judg_ment, Judg_mentKind, Var};
use xi_core::judgment::{Frontend, Judgment, JudgmentKind, Metadata, Primitive};
use xi_uuid::VarUuid;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
struct TypeVar(u32);

impl Primitive for TypeVar {
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S> {
        Judgment::u(None)
    }
}
fn less_than(type_var1: TypeVar, type_var2: TypeVar) -> bool {
    if type_var1.0 < type_var2.0 {
        return true;
    } else {
        return false;
    }
}
#[derive(Clone, Debug)]
struct Context {
    var_map: BTreeMap<VarUuid, TypeVar>,
    type_map: BTreeMap<TypeVar, Judgment<TypeVar, ()>>,
    next: u32,
}

impl Context {
    fn new() -> Context {
        Context {
            var_map: BTreeMap::new(),
            type_map: BTreeMap::new(),
            next: 0,
        }
    }
    // some way of replacing greek letters with things
    fn add_constraint(
        &mut self,
        type_var: TypeVar,
        replacement: Judgment<TypeVar, ()>,
    ) -> Result<(), TypeError> {
        match self.type_map.get(&type_var) {
            Some(result) => {
                let new_type = self.unify(result.clone(), replacement)?;
                self.type_map.insert(type_var, new_type);
            }
            None => {
                self.type_map.insert(type_var, replacement);
            }
        }
        Ok(())
    }

    fn lookup_var(&self, var: &Var) -> Judgment<TypeVar, ()> {
        let type_var = self
            .var_map
            .get(&var.index)
            .expect("we should have this lol");
        self.lookup_type_var(type_var)
    }

    fn lookup_type_var(&self, type_var: &TypeVar) -> Judgment<TypeVar, ()> {
        match self.type_map.get(type_var) {
            Some(inferred_type) => inferred_type.clone(),
            None => Judgment::prim(*type_var, None),
        }
    }

    fn new_expicit_type_var(&mut self, var_index: VarUuid) -> TypeVar {
        let type_var = self.new_type_var();
        self.var_map.insert(var_index, type_var);
        type_var
    }

    fn new_type_var(&mut self) -> TypeVar {
        let res = TypeVar(self.next);
        self.next += 1;
        res
    }

    fn update_one(&mut self, expr: Judgment<TypeVar, ()>, var: TypeVar) -> Judgment<TypeVar, ()> {
        let replacement = self.lookup_type_var(&var);
        expr.define_prim(Rc::new(move |type_var| {
            if type_var == var {
                replacement.clone()
            } else {
                Judgment::prim(type_var, None)
            }
        }))
    }

    fn update_all(&mut self, expr: Judgment<TypeVar, ()>) -> Judgment<TypeVar, ()> {
        let type_map = self.type_map.clone();
        expr.define_prim(Rc::new(move |type_var| {
            type_map.get(&type_var).unwrap().clone()
        }))
    }

    // This is the unify function in Hindley-Milner systems.
    fn unify(
        &mut self,
        lhs: Judgment<TypeVar, ()>,
        rhs: Judgment<TypeVar, ()>,
    ) -> Result<Judgment<TypeVar, ()>, TypeError> {
        let lhs = lhs.nbe().clone();
        let rhs = rhs.nbe().clone();

        if let JudgmentKind::Prim(type_var) = &lhs.tree {
            self.add_constraint(*type_var, rhs.clone());
            return Ok(rhs);
        }
        if let JudgmentKind::Prim(type_var) = &rhs.tree {
            self.add_constraint(*type_var, lhs.clone());
            return Ok(lhs);
        }

        let result = match (lhs.tree, rhs.tree) {
            (JudgmentKind::UInNone, JudgmentKind::UInNone) => Judgment::u(None),
            (JudgmentKind::FreeVar(index1, var_type), JudgmentKind::FreeVar(index2, _))
                if index1 == index2 =>
            {
                Judgment::free(index1, *var_type, None)
            }

            (JudgmentKind::Pi(var_type1, expr1), JudgmentKind::Pi(var_type2, expr2)) => {
                Judgment::pi(
                    self.unify(*var_type1, *var_type2)?,
                    self.unify(*expr1, *expr2)?,
                    None,
                )
            }

            (JudgmentKind::Lam(_, _), _) => {
                panic!("our nbe lord has failed us, this is all over... here goes dong")
            }
            (JudgmentKind::BoundVar(index1, var_type), JudgmentKind::BoundVar(index2, _))
                if index1 == index2 =>
            {
                Judgment::bound_var(index1, *var_type, None)
            }

            (JudgmentKind::Application(func1, elem1), JudgmentKind::Application(func2, elem2)) => {
                Judgment::app_unchecked(
                    self.unify(*func1, *func2)?,
                    self.unify(*elem1, *elem2)?,
                    None,
                )
            }
            _ => return Err(TypeError()),
        };

        Ok(result)
    }

    fn final_lookup(
        &mut self,
        judgment: Judgment<TypeVar, ()>,
        seen: &mut Vec<TypeVar>,
    ) -> Result<Judgment<Frontend, ()>, TypeError> {
        let result: Judgment<Frontend, ()> = match judgment.tree {
            JudgmentKind::UInNone => Judgment::u(None),
            JudgmentKind::Prim(type_var) => {
                if seen.iter().any(|&i| i.0 == type_var.0) {
                    panic!("there is an infinite loop in the type variables")
                }
                seen.push(type_var);
                let looked_up = self.lookup_type_var(&type_var);
                self.final_lookup(looked_up, seen)?
            }
            JudgmentKind::FreeVar(int, var_type) => {
                Judgment::free(int, self.final_lookup(*var_type, seen)?, None)
            }
            JudgmentKind::Pi(var_type, expr) => Judgment::pi(
                self.final_lookup(*var_type, seen)?,
                self.final_lookup(*expr, seen)?,
                None,
            ),
            JudgmentKind::Lam(var_type, expr) => Judgment::lam(
                self.final_lookup(*var_type, seen)?,
                self.final_lookup(*expr, seen)?,
                None,
            ),
            JudgmentKind::BoundVar(int, var_type) => {
                Judgment::bound_var(int, self.final_lookup(*var_type, seen)?, None)
            }
            JudgmentKind::Application(func, arg) => Judgment::app(
                self.final_lookup(*func, seen)?,
                self.final_lookup(*arg, seen)?,
                None,
            ),
        };
        Ok(result)
    }
}
#[derive(Clone, Debug)]
struct TypeError();
/// Take a Judg_ment and context and infer all the free variables in the Judg_ment, written in the context.
fn type_infer(
    judg_ment: Judg_ment,
    ctx: &mut Context,
    // TODO: add metadata
) -> Result<Judgment<TypeVar, ()>, TypeError> {
    // dbg!(judg_ment.clone());
    let result = match *judg_ment.0 {
        Judg_mentKind::Type => Judgment::u(None),
        Judg_mentKind::Var(var) => {
            let var_type = ctx.lookup_var(&var);
            Judgment::free(var.index, var_type, None)
        }
        Judg_mentKind::Fun(lhs, rhs) => {
            let new_lhs = type_infer(lhs, ctx)?;
            let new_rhs = type_infer(rhs, ctx)?;

            Judgment::pi(new_lhs, new_rhs, None)
        }
        Judg_mentKind::Pi(var, expr) => {
            let beta = ctx.new_expicit_type_var(var.index);
            match var.var_type {
                Some(var_type) => {
                    let infered_type = type_infer(var_type, ctx)?;
                    ctx.type_map.insert(beta, infered_type);
                }
                None => {}
            }

            let body = type_infer(expr, ctx)?;

            let body_bound = Judgment::rebind(body, var.index);

            Judgment::pi(ctx.lookup_type_var(&beta), body_bound, None)
        }
        Judg_mentKind::Lam(var, expr) => {
            let beta = ctx.new_expicit_type_var(var.index);
            match var.var_type {
                Some(var_type) => {
                    let infered_type = type_infer(var_type, ctx)?;
                    ctx.type_map.insert(beta, infered_type);
                }
                None => {}
            }

            let body = type_infer(expr, ctx)?;

            let body_bound = Judgment::rebind(body, var.index);

            Judgment::lam(ctx.lookup_type_var(&beta), body_bound, None)
        }
        Judg_mentKind::App(func, arg) => {
            let func_expr = type_infer(func, ctx)?;
            match func_expr.type_of().expect("please not none").tree {
                JudgmentKind::Pi(var_type, expr) => {
                    let new_arg = type_infer(arg, ctx)?;
                    ctx.unify(*var_type, new_arg.type_of().expect("failure"));

                    Judgment::app_unchecked(func_expr, new_arg, None)
                }
                JudgmentKind::Prim(type_var) => {
                    let epsilon = ctx.new_type_var();
                    let delta = ctx.new_type_var();
                    ctx.add_constraint(
                        type_var,
                        Judgment::pi(
                            Judgment::prim(epsilon, None),
                            Judgment::prim(delta, None),
                            None,
                        ),
                    );
                    let new_func_expr = ctx.update_one(func_expr, type_var);
                    let new_body = type_infer(arg, ctx)?;
                    ctx.add_constraint(epsilon, new_body.type_of().expect("failure"));
                    Judgment::app_unchecked(new_func_expr, new_body, None)
                }
                _ => return Err(TypeError()),
            }
        }
        Judg_mentKind::Bind(_, _, _) => todo!(),
        Judg_mentKind::StringLit(_) => todo!(),
        Judg_mentKind::Iota(_) => todo!(),
    };
    Ok(result)
}

fn to_judgment(judg_ment: Judg_ment) -> Result<Judgment<Frontend, ()>, TypeError> {
    let ctx = &mut Context::new();
    let judgment_with_type_var = type_infer(judg_ment, ctx)?;
    ctx.final_lookup(judgment_with_type_var, &mut vec![])
}

fn frontend(text: &str) -> Result<Judgment<Frontend, ()>, TypeError> {
    let judg_ment = text_to_judg_ment(text);
    to_judgment(judg_ment)
}

mod test {

    #[test]
    fn test_to_judgment() {
        use super::*;
        let text1 = "fn foo |x : Type| -> Type {val x}
        val foo";
        use crate::desugar::*;
        let judgment1 = frontend(text1);
        dbg!(judgment1);

        let text2 = "fn foo |x| {val x} 
         val foo (Pi|y: Type| y)";
        let ctx2 = &mut Context::new();
        let judgment2 = frontend(text1);
        dbg!(judgment2);
        // dbg!(ctx2);
    }

    //     let text2 = "fn foo |x| -> {val x} val foo \"hello world\" ";
    //     let judgment2 = front_end(text2);
    //     dbg!(judgment2);

    //     let text3 = "let y = \"hello world\"  let x = y";
    //     let judgment3 = front_end(text3);
    //     dbg!(judgment3);
    // }
}
