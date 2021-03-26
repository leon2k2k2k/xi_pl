use std::{collections::BTreeMap, rc::Rc};

use crate::desugar::{Judg_ment, Judg_mentKind, Var};
use crate::resolve::ResolvePrim;
use xi_core::judgment::{Judgment, JudgmentKind, Metadata, Primitive};
use xi_uuid::VarUuid;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct TypeVar(u32);

impl Primitive for TypeVar {
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S> {
        Judgment::u(None)
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    var_map: BTreeMap<VarUuid, TypeVar>,
    type_map: BTreeMap<TypeVar, Judgment<TypeVarPrim, UiMetadata>>,
    next: u32,
}

impl Context {
    pub fn new() -> Context {
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
        replacement: Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<(), TypeError> {
        match self.type_map.get(&type_var) {
            Some(result) => {
                let result = result.clone();
                let new_type = self.unify(result, replacement)?;
                self.type_map.insert(type_var, new_type);
            }
            None => {
                self.type_map.insert(type_var, replacement);
            }
        }
        Ok(())
    }

    fn lookup_var(&self, var: &Var) -> Judgment<TypeVarPrim, UiMetadata> {
        dbg!(self);
        dbg!(var);
        let type_var = self
            .var_map
            .get(&var.index)
            .expect("we should have this lol");
        self.lookup_type_var(type_var)
    }

    fn lookup_type_var(&self, type_var: &TypeVar) -> Judgment<TypeVarPrim, UiMetadata> {
        match self.type_map.get(type_var) {
            Some(inferred_type) => inferred_type.clone(),
            None => Judgment::prim(TypeVarPrim::TypeVar(*type_var), None),
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

    fn update_one(
        &mut self,
        expr: Judgment<TypeVarPrim, UiMetadata>,
        var: TypeVar,
    ) -> Judgment<TypeVarPrim, UiMetadata> {
        let replacement = self.lookup_type_var(&var);
        expr.define_prim(Rc::new(move |type_var_prim| {
            if let TypeVarPrim::TypeVar(type_var) = type_var_prim {
                if type_var == var {
                    replacement.clone()
                } else {
                    Judgment::prim(type_var_prim, None)
                }
            } else {
                Judgment::prim(type_var_prim, None)
            }
        }))
    }

    // fn update_all(&mut self, expr: Judgment<TypeVarPrim, ()>) -> Judgment<TypeVarPrim, ()> {
    //     let type_map = self.type_map.clone();
    //     expr.define_prim(Rc::new(move |type_var_prim| match type_var_prim {
    //         TypeVarPrim::Prim(prim) => todo!(),
    //         TypeVarPrim::TypeVar(type_var) => type_map.get(&type_var).unwrap().clone(),
    //     }))
    // }

    // This is the unify function in Hindley-Milner systems.
    fn unify(
        &mut self,
        lhs: Judgment<TypeVarPrim, UiMetadata>,
        rhs: Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
        let lhs = lhs.clone();
        let rhs = rhs.clone();

        if let JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var)) = &lhs.tree {
            self.add_constraint(*type_var, rhs.clone())?;
            return Ok(rhs);
        }
        if let JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var)) = &rhs.tree {
            self.add_constraint(*type_var, lhs.clone())?;
            return Ok(lhs);
        }

        let result = match (lhs.tree, rhs.tree) {
            (JudgmentKind::UInNone, JudgmentKind::UInNone) => Judgment::u(None),
            (JudgmentKind::FreeVar(index1, var_type), JudgmentKind::FreeVar(index2, _))
                if index1 == index2 =>
            {
                Judgment::free(index1, *var_type, None)
            }

            (
                JudgmentKind::Prim(TypeVarPrim::Prim(prim1)),
                JudgmentKind::Prim(TypeVarPrim::Prim(prim2)),
            ) if prim1 == prim2 => Judgment::prim(TypeVarPrim::Prim(prim1), None),

            (JudgmentKind::Pi(var_type1, expr1), JudgmentKind::Pi(var_type2, expr2)) => {
                Judgment::pi(
                    self.unify(*var_type1, *var_type2)?,
                    self.unify(*expr1, *expr2)?,
                    None,
                )
            }

            (JudgmentKind::Lam(_, _), _) => {
                panic!("our nbe lord has failed us, this is all over... here goes dongi")
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

    pub fn final_lookup(
        &mut self,
        judgment: Judgment<TypeVarPrim, UiMetadata>,
        seen: &Vec<TypeVar>,
    ) -> Result<Judgment<UiPrim, UiMetadata>, TypeError> {
        // dbg!(judgment.clone());
        dbg!(&self);
        let metadata = judgment.metadata;
        let result: Judgment<UiPrim, UiMetadata> = match judgment.tree {
            JudgmentKind::UInNone => Judgment::u(Some(metadata)),
            JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var)) => {
                if seen.iter().any(|&i| i.0 == type_var.0) {
                    panic!("there is an infinite loop in the type variables")
                }
                let mut seen2 = seen.clone();
                seen2.push(type_var);
                let looked_up = self.lookup_type_var(&type_var);
                self.final_lookup(looked_up, &seen2)?
            }
            JudgmentKind::Prim(TypeVarPrim::Prim(prim)) => Judgment::prim(prim, Some(metadata)),
            JudgmentKind::FreeVar(int, var_type) => {
                Judgment::free(int, self.final_lookup(*var_type, seen)?, Some(metadata))
            }
            JudgmentKind::Pi(var_type, expr) => Judgment::pi(
                self.final_lookup(*var_type, seen)?,
                self.final_lookup(*expr, seen)?,
                Some(metadata),
            ),
            JudgmentKind::Lam(var_type, expr) => Judgment::lam(
                self.final_lookup(*var_type, seen)?,
                self.final_lookup(*expr, seen)?,
                Some(metadata),
            ),
            JudgmentKind::BoundVar(int, var_type) => {
                Judgment::bound_var(int, self.final_lookup(*var_type, seen)?, Some(metadata))
            }
            JudgmentKind::Application(func, arg) => Judgment::app_unchecked(
                dbg!(self.final_lookup(*func, seen)?),
                dbg!(self.final_lookup(*arg, seen)?),
                Some(metadata),
            ),
        };
        Ok(result)
    }
}
#[derive(Clone, Debug)]
pub struct TypeError();
/// Take a Judg_ment and context and infer all the free variables in the Judg_ment, written in the context.
pub fn type_infer(
    judg_ment: Judg_ment,
    ctx: &mut Context,
    // TODO: add metadata
) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
    use xi_proc_macro::term;

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
            let func_expr = type_infer(func.clone(), ctx)?;
            match func_expr.type_of().expect("please not none").tree {
                JudgmentKind::Pi(var_type, _expr) => {
                    let new_arg = type_infer(arg, ctx)?;
                    ctx.unify(*var_type, new_arg.type_of().expect("failure"))?;

                    Judgment::app_unchecked(func_expr, new_arg, None)
                }
                JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var)) => {
                    let epsilon = ctx.new_type_var();
                    let delta = ctx.new_type_var();
                    ctx.add_constraint(
                        type_var,
                        Judgment::pi(
                            Judgment::prim(TypeVarPrim::TypeVar(epsilon), None),
                            Judgment::prim(TypeVarPrim::TypeVar(delta), None),
                            None,
                        ),
                    )?;

                    let new_func_expr = ctx.update_one(func_expr, type_var);
                    let new_body = type_infer(arg, ctx)?;
                    ctx.add_constraint(epsilon, new_body.type_of().expect("failure"))?;
                    Judgment::app_unchecked(new_func_expr, new_body, None)
                }
                _ => return Err(TypeError()),
            }
        }
        Judg_mentKind::Bind(input, rest) => {
            // M should be IO right
            // Bind TV(0) TV(1) input rest)
            // App IOMOnad TV(0) = infer_input.type_of.ok_or(TypeError())?);
            // TV(0) -> App IOMonad TV(1) = infer_rest.type_of.ok_or(TypeError())?);
            use TypeVarPrim::*;
            use UiPrim::*;
            let tv0 = ctx.new_type_var();
            let tv1 = ctx.new_type_var();
            let infer_input = type_infer(input, ctx)?;
            let infer_rest = type_infer(rest, ctx)?;

            let judgment_tv0 = Judgment::prim(TypeVarPrim::TypeVar(tv0), None);
            let lhs1 = term!([Prim(IOMonad)] {judgment_tv0});
            ctx.unify(lhs1, infer_input.type_of().ok_or(TypeError())?)?;

            let judgment_tv1 = Judgment::prim(TypeVarPrim::TypeVar(tv1), None);
            let lhs2 = term!({judgment_tv0} -> [Prim(UiPrim::IOMonad)] {judgment_tv1});
            ctx.unify(lhs2, infer_rest.type_of().ok_or(TypeError())?)?;

            Judgment::app_unchecked_vec(
                Judgment::prim(Prim(IOBind), None),
                &mut vec![judgment_tv0, judgment_tv1, infer_input, infer_rest],
                None,
            )
        }
        Judg_mentKind::StringLit(str) => {
            Judgment::prim(TypeVarPrim::Prim(UiPrim::StringElem(str)), None)
        }
        Judg_mentKind::Pure(expr) => {
            use TypeVarPrim::*;
            use UiPrim::*;
            let epsilon = ctx.new_type_var();
            let infer_expr = type_infer(expr, ctx)?;
            ctx.add_constraint(epsilon, infer_expr.type_of().ok_or(TypeError())?)?;
            let judgment_epsilon = Judgment::prim(TypeVarPrim::TypeVar(epsilon), None);

            Judgment::app_unchecked_vec(
                Judgment::prim(Prim(IOPure), None),
                &mut vec![judgment_epsilon, infer_expr],
                None,
            )
        }
        Judg_mentKind::Prim(prim) => {
            use UiPrim::*;
            let ui_prim = match prim {
                ResolvePrim::IOMonad => IOMonad,
                ResolvePrim::StringType => StringType,
                ResolvePrim::UnitType => UnitType,
                ResolvePrim::UnitElem => UnitElem,
                ResolvePrim::ConsoleInput => ConsoleInput,
                ResolvePrim::ConsoleOutput => ConsoleOutput,
            };
            Judgment::prim(TypeVarPrim::Prim(ui_prim), None)
        }
        Judg_mentKind::Ffi(file_name, func_name) => {
            dbg!(&func_name);
            let index = VarUuid::new();
            let var_type = ctx.new_expicit_type_var(index);
            Judgment::free(
                index,
                Judgment::prim(TypeVarPrim::TypeVar(var_type), None),
                Some(UiMetadata {
                    ffi: Some((file_name, func_name)),
                }),
            )
        }
    };
    Ok(result)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum UiPrim {
    IOMonad,
    IOBind,
    StringType,
    StringElem(String),
    UnitType,
    UnitElem,
    ConsoleInput,
    ConsoleOutput,
    IOPure,
}

impl Primitive for UiPrim {
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S> {
        use xi_proc_macro::term;
        use UiPrim::*;
        match self {
            UiPrim::StringType => Judgment::u(None),
            UiPrim::StringElem(_) => Judgment::prim(UiPrim::StringType, None),
            UiPrim::IOMonad => term!(U -> U),
            UiPrim::IOBind => {
                term!(Pi |A : U, B : U| [IOMonad] A -> (A -> [IOMonad] B) -> [IOMonad] B)
            }
            UiPrim::UnitType => term!(U),
            UiPrim::UnitElem => Judgment::prim(UiPrim::UnitType, None),
            UiPrim::ConsoleInput => {
                term!([IOMonad][StringType])
            }
            UiPrim::ConsoleOutput => {
                term!([StringType] -> [IOMonad] [UnitType])
            }
            UiPrim::IOPure => {
                term!(Pi |T : U| T -> [IOMonad] T)
            }
        }
    }
}

pub fn to_judgment(judg_ment: Judg_ment) -> Result<Judgment<UiPrim, UiMetadata>, TypeError> {
    let ctx = &mut Context::new();
    let judgment_with_type_var = type_infer(judg_ment, ctx)?;
    ctx.final_lookup(judgment_with_type_var, &vec![])
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeVarPrim {
    TypeVar(TypeVar),
    Prim(UiPrim),
}

impl Primitive for TypeVarPrim {
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S> {
        match self {
            TypeVarPrim::TypeVar(type_var) => type_var
                .type_of()
                .define_prim(Rc::new(|s| Judgment::prim(TypeVarPrim::TypeVar(s), None))),
            TypeVarPrim::Prim(type_var) => type_var
                .type_of()
                .define_prim(Rc::new(|s| Judgment::prim(TypeVarPrim::Prim(s), None))),
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct UiMetadata {
    pub ffi: Option<(String, String)>,
    // span: TextRange todo :)
}

impl Metadata for UiMetadata {}
