use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use crate::desugar::{Judg_ment, Judg_mentKind};
use crate::resolve::ResolvePrim;
use rowan::TextRange;
use xi_core::judgment::{Judgment, JudgmentKind, Metadata, Primitive};
use xi_uuid::VarUuid;

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub struct TypeVar(u32);

impl TypeVar {
    fn new() -> TypeVar {
        let id = VarUuid::new().index();
        TypeVar(id)
    }
}

impl Primitive for TypeVar {
    fn maybe_prim_type<S: Metadata>(&self) -> Option<Judgment<Self, S>> {
        Some(Judgment::u(None))
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    var_map: Vec<TypeVar>,
    rebind_map: Vec<VarUuid>,
    constraints: BTreeMap<TypeVar, Judgment<TypeVarPrim, UiMetadata>>,
}

impl Context {
    fn new() -> Context {
        Context {
            var_map: vec![],
            rebind_map: vec![],
            constraints: BTreeMap::new(),
        }
    }

    fn contains_free_var(&self, var: VarUuid) -> bool {
        for (type_var, constraint) in &self.constraints {
            if Judgment::contains_free_var(&*constraint, var) {
                return true;
            }
        }

        return false;
    }

    fn replace_with_free_var(
        &self,
        expr: Judgment<TypeVarPrim, UiMetadata>,
    ) -> Judgment<TypeVarPrim, UiMetadata> {
        fn replace_with_free_var_rec(
            expr: Judgment<TypeVarPrim, UiMetadata>,
            depth: u32,
            ctx: &Context,
        ) -> Judgment<TypeVarPrim, UiMetadata> {
            let result_tree = match expr.tree {
                JudgmentKind::Type => JudgmentKind::Type,
                JudgmentKind::Prim(prim, prim_type) => JudgmentKind::Prim(
                    prim,
                    Box::new(replace_with_free_var_rec(*prim_type, depth, ctx)),
                ),
                JudgmentKind::FreeVar(var, var_type) => JudgmentKind::FreeVar(
                    var,
                    Box::new(replace_with_free_var_rec(*var_type, depth, ctx)),
                ),
                JudgmentKind::Pi(var_type, expr) => {
                    let new_var_type = replace_with_free_var_rec(*var_type, depth, ctx);
                    let new_expr = replace_with_free_var_rec(*expr, depth + 1, ctx);
                    JudgmentKind::Pi(Box::new(new_var_type), Box::new(new_expr))
                }
                JudgmentKind::Lam(var_type, expr) => {
                    let new_var_type = replace_with_free_var_rec(*var_type, depth, ctx);
                    let new_expr = replace_with_free_var_rec(*expr, depth + 1, ctx);
                    JudgmentKind::Lam(Box::new(new_var_type), Box::new(new_expr))
                }
                JudgmentKind::BoundVar(index, var_type) => {
                    if index >= depth {
                        dbg!(&ctx);
                        dbg!(&index);
                        JudgmentKind::FreeVar(
                            ctx.rebind_map[ctx.rebind_map.len() - 1 - depth as usize],
                            Box::new(replace_with_free_var_rec(*var_type, depth, ctx)),
                        )
                    } else {
                        JudgmentKind::BoundVar(
                            index,
                            Box::new(replace_with_free_var_rec(*var_type, depth, ctx)),
                        )
                    }
                }
                JudgmentKind::Application(func, arg) => {
                    let new_func = replace_with_free_var_rec(*func, depth, ctx);
                    let new_arg = replace_with_free_var_rec(*arg, depth, ctx);
                    JudgmentKind::Application(Box::new(new_func), Box::new(new_arg))
                }
            };

            Judgment {
                tree: result_tree,
                metadata: expr.metadata,
            }
        }

        replace_with_free_var_rec(expr, 0, self)
    }

    // some way of replacing greek letters with things
    fn add_constraint(
        &mut self,
        type_var: TypeVar,
        replacement: Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<(), TypeError> {
        let replacement = self.replace_with_free_var(replacement);
        let result = match self.constraints.get(&type_var) {
            Some(result) => {
                let result = result.clone();
                self.unify(&result, &replacement)?
            }
            None => replacement,
        };

        // dbg!(&type_var, &result);

        self.constraints.insert(type_var, result);
        Ok(())
    }

    // fn lookup_var(&self, var: &BoundVar) -> Judgment<TypeVarPrim, UiMetadata> {
    //     // dbg!(self);
    //     // dbg!(var);
    //     let type_var = self
    //         .var_map
    //         .get(&var.index)
    //         .expect("we should have this lol");
    //     self.lookup_type_var(type_var)
    // }

    fn lookup_type_var(&self, type_var: &TypeVar) -> Judgment<TypeVarPrim, UiMetadata> {
        match self.constraints.get(type_var) {
            Some(inferred_type) => inferred_type.clone(),
            None => Judgment::prim(TypeVarPrim::TypeVar(*type_var), Judgment::u(None), None),
        }
    }

    // fn new_expicit_type_var(&mut self, var_index: VarUuid) -> TypeVar {
    //     let type_var = self.new_type_var();
    //     self.var_map.insert(var_index, type_var);
    //     self.var_map_inv.insert(type_var, var_index);
    //     type_var
    // }

    // fn new_type_var(&mut self) -> TypeVar {
    //     let res = TypeVar(self.next);
    //     self.next += 1;
    //     res
    // }

    // fn update_one(
    //     &mut self,
    //     expr: Judgment<TypeVarPrim, UiMetadata>,
    //     var: TypeVar,
    // ) -> Judgment<TypeVarPrim, UiMetadata> {
    //     let replacement = self.lookup_type_var(&var);
    //     expr.define_prim(Rc::new(move |type_var_prim| {
    //         if let TypeVarPrim::TypeVar(type_var) = type_var_prim {
    //             if type_var == var {
    //                 replacement.clone()
    //             } else {
    //                 Judgment::prim(type_var_prim, None)
    //             }
    //         } else {
    //             Judgment::prim(type_var_prim, None)
    //         }
    //     }))
    // }

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
        lhs: &Judgment<TypeVarPrim, UiMetadata>,
        rhs: &Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
        // dbg!(&lhs);
        // dbg!(&rhs);
        // // dbg!(&self);
        // dbg!("");
        let lhs = lhs.clone();
        let rhs = rhs.clone();

        if let JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var), prim_type) = &lhs.tree {
            self.add_constraint(*type_var, rhs.clone())?;
            return Ok(rhs);
        }
        if let JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var), prim_type) = &rhs.tree {
            self.add_constraint(*type_var, lhs.clone())?;
            return Ok(lhs);
        }

        let result = match (&lhs.tree, &rhs.tree) {
            (JudgmentKind::Type, JudgmentKind::Type) => Judgment::u(None),
            (JudgmentKind::FreeVar(index1, var_type), JudgmentKind::FreeVar(index2, _))
                if index1 == index2 =>
            {
                Judgment::free(*index1, *var_type.clone(), None)
            }

            (
                JudgmentKind::Prim(TypeVarPrim::Prim(prim1), prim_type1),
                JudgmentKind::Prim(TypeVarPrim::Prim(prim2), prim_type2),
            ) if prim1 == prim2 => {
                Judgment::prim(TypeVarPrim::Prim(prim1.clone()), *prim_type1.clone(), None)
            }

            (JudgmentKind::Pi(var_type1, expr1), JudgmentKind::Pi(var_type2, expr2)) => {
                Judgment::pi(
                    self.unify(&*var_type1, &*var_type2)?,
                    self.unify(&*expr1, &*expr2)?,
                    None,
                )
            }

            (JudgmentKind::Lam(_, _), _) => {
                panic!("our nbe lord has failed us, this is all over... here goes dongi")
            }
            (JudgmentKind::BoundVar(index1, var_type), JudgmentKind::BoundVar(index2, _))
                if index1 == index2 =>
            {
                Judgment::bound_var(*index1, *var_type.clone(), None)
            }

            (JudgmentKind::Application(func1, elem1), JudgmentKind::Application(func2, elem2)) => {
                Judgment::app_unchecked(
                    self.unify(&*func1, &*func2)?,
                    self.unify(&*elem1, &*elem2)?,
                    None,
                )
            }
            _ => {
                panic!(
                    "lhs and rhs failed to unify, lhs: {:?}, rhs: {:?}",
                    lhs, rhs
                );
            }
        };

        Ok(result)
    }

    // Takes in a list of responsibilities. Eliminates any references of it in the context, and returns a new list.
    fn elim_resps(&mut self, resps: Vec<TypeVar>) -> Result<Vec<TypeVar>, TypeError> {
        let mut new_resps = vec![];

        for resp in resps {
            if !self.constraints.contains_key(&resp) {
                // We didn't learn anything about this type var, abdicate responsibliity
                new_resps.push(resp);
                break;
            }

            let resolved_type = self.substitute(&Judgment::prim(
                TypeVarPrim::TypeVar(resp),
                Judgment::u(None),
                None,
            ))?;

            let sub_fn = Rc::new(
                move |prim,
                      prim_type,
                      define_prim: Rc<
                    dyn Fn(Judgment<TypeVarPrim, UiMetadata>) -> Judgment<TypeVarPrim, UiMetadata>,
                >| {
                    if let TypeVarPrim::TypeVar(type_var) = prim {
                        if type_var == resp {
                            resolved_type.clone()
                        } else {
                            Judgment::prim(prim, define_prim(prim_type), None)
                        }
                    } else {
                        Judgment::prim(prim, define_prim(prim_type), None)
                    }
                },
            );

            self.constraints.remove(&resp);

            for constraint in self.constraints.iter_mut() {
                *constraint.1 = constraint.1.define_prim(sub_fn.clone());
            }
        }

        Ok(new_resps)
    }
    // function formaly known as final_lookup.
    pub fn substitute(
        &mut self,
        judgment: &Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
        fn substitute_rec(
            ctx: &mut Context,
            judgment: &Judgment<TypeVarPrim, UiMetadata>,
            seen: &BTreeSet<TypeVar>,
        ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
            let metadata = judgment.metadata.clone();
            let result = match &judgment.tree {
                JudgmentKind::Type => Judgment::u(Some(metadata)),
                JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var), prim_type) => {
                    if seen.contains(&type_var) {
                        panic!("there is an infinite loop in the type variables")
                    }
                    match ctx.constraints.get(&type_var) {
                        Some(constraint) => {
                            let mut seen2 = seen.clone();
                            seen2.insert(*type_var);
                            let constraint_clone = constraint.clone();
                            substitute_rec(ctx, &constraint_clone, &seen2)?
                        }
                        None => Judgment::prim(
                            TypeVarPrim::TypeVar(*type_var),
                            *prim_type.clone(),
                            Some(metadata),
                        ),
                    }
                }
                JudgmentKind::Prim(TypeVarPrim::Prim(prim), prim_type) => Judgment::prim(
                    TypeVarPrim::Prim(prim.clone()),
                    substitute_rec(ctx, &prim_type, seen)?,
                    Some(metadata),
                ),
                JudgmentKind::FreeVar(int, var_type) => {
                    Judgment::free(*int, substitute_rec(ctx, &var_type, seen)?, Some(metadata))
                }
                JudgmentKind::Pi(var_type, expr) => Judgment::pi(
                    substitute_rec(ctx, &var_type, seen)?,
                    substitute_rec(ctx, &expr, seen)?,
                    Some(metadata),
                ),
                JudgmentKind::Lam(var_type, expr) => Judgment::lam(
                    substitute_rec(ctx, &var_type, seen)?,
                    substitute_rec(ctx, &expr, seen)?,
                    Some(metadata),
                ),
                JudgmentKind::BoundVar(int, var_type) => {
                    Judgment::bound_var(*int, substitute_rec(ctx, &var_type, seen)?, Some(metadata))
                }
                JudgmentKind::Application(func, arg) => Judgment::app_unchecked(
                    substitute_rec(ctx, &func, seen)?,
                    substitute_rec(ctx, &arg, seen)?,
                    Some(metadata),
                ),
            };
            Ok(result)
        }
        substitute_rec(self, judgment, &BTreeSet::new())
    }
}

#[derive(Clone, Debug)]
pub struct TypeError {
    string: String,
}
// Take a Judg_ment and the local context and try to deduce the type of the judg_ment as best as we can.
pub fn type_infer(
    judg_ment: &Judg_ment<UiPrim, UiMetadata>,
    ctx: &mut Context,
    // TODO: add metadata
) -> Result<(Judgment<TypeVarPrim, UiMetadata>, Vec<TypeVar>), TypeError> {
    use xi_proc_macro::term;

    let ctx_copy = (*ctx).clone();

    // dbg!(judg_ment.clone());
    let result: (Judgment<TypeVarPrim, UiMetadata>, Vec<TypeVar>) = match &*judg_ment.0 {
        Judg_mentKind::Type => (Judgment::u(None), vec![]),
        Judg_mentKind::BoundVar(index) => {
            // get the typevar corresponding to the Boundvar, then also look that up in the
            let var_type =
                ctx.lookup_type_var(&ctx.var_map[ctx.var_map.len() - 1 - *index as usize]);
            dbg!("========================");
            dbg!(&index);
            dbg!(&ctx);
            dbg!(&var_type);
            (Judgment::bound_var(*index, var_type, None), vec![])
        }
        Judg_mentKind::Pi(var_type, expr) => {
            let type_var = TypeVar::new();
            let mut resps = vec![type_var];

            if let Some(var_type) = var_type {
                let (inferred_type, mut sub_resps) = type_infer(var_type, ctx)?;
                resps.append(&mut sub_resps);
                ctx.add_constraint(type_var, inferred_type)?;
            }

            ctx.var_map.push(type_var);
            let rebind_var = VarUuid::new();
            ctx.rebind_map.push(rebind_var);

            let (expr, mut sub_resps) = type_infer(expr, ctx)?;
            resps.append(&mut sub_resps);

            // We substitute eagerly
            let new_var_type = ctx.substitute(&Judgment::prim(
                TypeVarPrim::TypeVar(type_var),
                Judgment::u(None),
                None,
            ))?;
            let new_expr = ctx.substitute(&expr)?;

            ctx.var_map.pop();
            ctx.rebind_map.pop();

            // we check if a variable can be eliminated from the responsibility or not,
            // if it is, then we remove all occurences of it in the context and removes it.
            let new_resps = ctx.elim_resps(resps)?;

            // We rebind any occurence of our free variable
            let newer_expr = Judgment::rebind(new_expr, rebind_var);
            (Judgment::pi(new_var_type, newer_expr, None), new_resps)
        }
        Judg_mentKind::Lam(var_type, expr) => {
            let type_var = TypeVar::new();
            let mut resps = vec![type_var];

            if let Some(var_type) = var_type {
                let (inferred_type, mut sub_resps) = type_infer(var_type, ctx)?;
                resps.append(&mut sub_resps);
                ctx.add_constraint(type_var, inferred_type)?;
            }

            ctx.var_map.push(type_var);
            let rebind_var = VarUuid::new();
            ctx.rebind_map.push(rebind_var);

            let (expr, mut sub_resps) = type_infer(expr, ctx)?;
            resps.append(&mut sub_resps);

            // We substitute eagerly
            let new_var_type = ctx.substitute(&Judgment::prim(
                TypeVarPrim::TypeVar(type_var),
                Judgment::u(None),
                None,
            ))?;
            let new_expr = ctx.substitute(&expr)?;

            ctx.var_map.pop();
            ctx.rebind_map.pop();

            // we check if a variable can be eliminated from the responsibility or not,
            // if it is, then we remove all occurences of it in the context and removes it.
            let new_resps = ctx.elim_resps(resps)?;

            // We rebind any occurence of our free variable
            let newer_expr = Judgment::rebind(new_expr, rebind_var);
            (Judgment::lam(new_var_type, newer_expr, None), new_resps)
        }
        Judg_mentKind::App(old_func, old_arg) => {
            let mut resps = vec![];
            let (func, mut func_resps) = type_infer(old_func, ctx)?;
            resps.append(&mut func_resps);

            let (arg, mut arg_resps) = type_infer(old_arg, ctx)?;
            resps.append(&mut arg_resps);

            dbg!(&func.type_of());
            dbg!(&arg);
            dbg!(&arg.type_of());

            match func.type_of().expect("please not none").tree {
                JudgmentKind::Pi(arg_type, expr) => {
                    let func_type = TypeVar::new();
                    resps.push(func_type);

                    ctx.add_constraint(func_type, *arg_type)?;
                    ctx.add_constraint(func_type, arg.type_of().unwrap())?;
                }
                JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var), prim_type) => {
                    let epsilon = TypeVar::new();
                    let delta = TypeVar::new();
                    resps.push(epsilon);
                    resps.push(delta);

                    ctx.add_constraint(
                        type_var,
                        Judgment::pi(
                            Judgment::prim_wo_prim_type(TypeVarPrim::TypeVar(epsilon), None),
                            Judgment::prim_wo_prim_type(TypeVarPrim::TypeVar(delta), None),
                            None,
                        ),
                    )?;

                    ctx.add_constraint(epsilon, arg.type_of().expect("failure"))?;
                }
                _ => panic!(format!(
                    "func_expr doesn't have the correct type, func : {:?}, func.type_of: {:?}",
                    func,
                    func.type_of()
                )),
            }

            let new_func = ctx.substitute(&func)?;
            let new_arg = ctx.substitute(&arg)?;

            let new_resps = ctx.elim_resps(resps)?;

            (Judgment::app_unchecked(new_func, new_arg, None), new_resps)
        }
        Judg_mentKind::Bind(old_arg, old_func) => {
            use TypeVarPrim::{Prim, TypeVar};
            use UiPrim::{IOBind, IOMonad};

            let alpha = self::TypeVar::new();
            let beta = self::TypeVar::new();
            let io_alpha = self::TypeVar::new();
            let alpha_to_io_beta = self::TypeVar::new();

            let mut resps = vec![alpha, beta, io_alpha, alpha_to_io_beta];

            let (func, mut func_resps) = type_infer(old_func, ctx)?;
            resps.append(&mut func_resps);

            let (arg, mut arg_resps) = type_infer(old_arg, ctx)?;
            resps.append(&mut arg_resps);

            ctx.add_constraint(io_alpha, term!([Prim(IOMonad)][TypeVar(alpha)]))?;
            ctx.add_constraint(io_alpha, arg.type_of().unwrap())?;

            ctx.add_constraint(
                alpha_to_io_beta,
                term!([TypeVar(alpha)] -> [Prim(UiPrim::IOMonad)] [TypeVar(beta)]),
            )?;
            ctx.add_constraint(alpha_to_io_beta, func.type_of().unwrap())?;

            let tmp_term = term!([Prim(IOBind)] [TypeVar(alpha)] [TypeVar(beta)] {arg} {func});

            let result_term = ctx.substitute(&tmp_term)?;

            let new_resps = ctx.elim_resps(resps)?;
            // dbg!(&ctx);
            (result_term, new_resps)
        }

        Judg_mentKind::Pure(old_expr) => {
            use TypeVarPrim::Prim;
            use UiPrim::IOPure;

            let mut resps = vec![];

            let (expr, mut expr_resps) = type_infer(old_expr, ctx)?;
            resps.append(&mut expr_resps);

            let expr_type = expr.type_of().unwrap();

            let new_resps = ctx.elim_resps(resps)?;

            // let bu: Judgment<UiPrim, UiMetadata> = Judgment::prim_wo_prim_type(IOPure, None);
            // dbg!(bu.type_of());

            // dbg!(Judgment::app(
            //     bu,
            //     Judgment::prim_wo_prim_type(UiPrim::StringType, None),
            //     None
            // ));

            (term!([Prim(IOPure)] {expr_type} {expr}), new_resps)
        }
        Judg_mentKind::Prim(prim) => match prim.maybe_prim_type::<UiMetadata>() {
            Some(_prim_type) => (
                Judgment::prim_wo_prim_type(TypeVarPrim::Prim(prim.clone()), None),
                vec![],
            ),
            None => {
                let alpha = TypeVar::new();
                (
                    Judgment::prim(
                        TypeVarPrim::Prim(prim.clone()),
                        Judgment::prim_wo_prim_type(TypeVarPrim::TypeVar(alpha), None),
                        None,
                    ),
                    vec![alpha],
                )
            }
        },

        Judg_mentKind::FreeVar(_) => {
            panic!("we shouldn't see a FreeVar at this stage!")
        }
        Judg_mentKind::Ffi(_, _) => {
            panic!("we don't support this right now, Frank will do it!")
        }
    };
    // dbg!(ctx_copy, judg_ment, &result);
    unsafe {
        if Judgment::contains_free_var(&result.0, std::mem::transmute::<u32, VarUuid>(15)) {
            // dbg!(&result.0);
        }
    }
    Ok(result)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum UiPrim {
    IOMonad,
    IOBind,
    IOPure,
    StringType,
    StringElem(String),
    NumberType,
    NumberElem(String),
    Binary(UiBinaryOp),
    Ffi(String, String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum UiBinaryOp {
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
}

impl Primitive for UiPrim {
    fn maybe_prim_type<S: Metadata>(&self) -> Option<Judgment<Self, S>> {
        use xi_proc_macro::term;
        use UiPrim::*;
        match self {
            StringType => Some(Judgment::u(None)),
            StringElem(_) => Some(Judgment::prim_wo_prim_type(UiPrim::StringType, None)),
            NumberType => Some(Judgment::u(None)),
            NumberElem(_) => Some(Judgment::prim_wo_prim_type(UiPrim::NumberType, None)),
            IOMonad => Some(term!(U -> U)),
            IOBind => {
                Some(term!(Pi |A : U, B : U| [IOMonad] A -> (A -> [IOMonad] B) -> [IOMonad] B))
            }
            IOPure => Some(term!(Pi |T : U| T -> [IOMonad] T)),
            NumberType => Some(term!(U)),
            Binary(operator) => {
                let result = match operator {
                    UiBinaryOp::And => todo!(),
                    UiBinaryOp::Or => todo!(),
                    UiBinaryOp::Equal => todo!(),
                    UiBinaryOp::NotEqual => todo!(),
                    UiBinaryOp::LessThan => todo!(),
                    UiBinaryOp::LessThanEqual => todo!(),
                    UiBinaryOp::GreaterThan => todo!(),
                    UiBinaryOp::GreaterThanEqual => todo!(),
                    UiBinaryOp::Plus => term!([NumberType] -> [NumberType] -> [NumberType]),
                    UiBinaryOp::Minus => term!([NumberType] -> [NumberType] -> [NumberType]),
                    UiBinaryOp::Multiply => term!([NumberType] -> [NumberType] -> [NumberType]),
                    UiBinaryOp::Divide => term!([NumberType] -> [NumberType] -> [NumberType]),
                    UiBinaryOp::Modulo => term!([NumberType] -> [NumberType] -> [NumberType]),
                };
                Some(result)
            }
            Ffi(_, _) => None,
        }
    }
}

pub fn to_judgment(
    judg_ment: Judg_ment<UiPrim, UiMetadata>,
) -> Result<Judgment<UiPrim, UiMetadata>, TypeError> {
    let ctx = &mut Context::new();
    let (judgment_with_type_var, resps) = type_infer(&judg_ment, ctx)?;

    if !resps.is_empty() {
        panic!("we shouldn't have any type variables left");
    }
    // dbg!(&judgment_with_type_var);
    Ok(
        judgment_with_type_var.define_prim(Rc::new(|type_var_prim, prim_type, define_prim| {
            match type_var_prim {
                TypeVarPrim::TypeVar(_) => unreachable!("resps should be empty"),
                TypeVarPrim::Prim(prim) => Judgment::prim(prim, define_prim(prim_type), None),
            }
        })),
    )
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeVarPrim {
    TypeVar(TypeVar),
    Prim(UiPrim),
}

impl Primitive for TypeVarPrim {
    fn maybe_prim_type<S: Metadata>(&self) -> Option<Judgment<Self, S>> {
        let ans = match self {
            TypeVarPrim::TypeVar(type_var) => Judgment::u(None),
            TypeVarPrim::Prim(type_var) => match type_var.maybe_prim_type() {
                None => {
                    return None;
                }
                Some(prim_type) => prim_type.define_prim(Rc::new(|t, prim_type, define_prim| {
                    Judgment::prim(TypeVarPrim::Prim(t), define_prim(prim_type), None)
                })),
            },
        };
        Some(ans)
    }
}

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct UiMetadata {
    // pub ffi: Option<(String, String)>,
// span: TextRange todo :)
// span: Option<TextRange>,
// var_name: Option<String>,
}

impl Metadata for UiMetadata {}

// #[derive(Clone, Default, PartialEq, Eq, Debug)]
// pub struct UiMetadataHelper {
//     pub ffi: Option<(String, String)>,
//     pub bind_var_index: Option<VarUuid>,
// }

// impl Metadata for UiMetadataHelper {}

// Tests to see that Type inference works!
// Do not erase!!!!!!!
mod test {
    use xi_core::judgment::Judgment;

    use crate::desugar::source_file_to_judg_ment;

    #[test]
    fn type_infer_test() {
        // use crate::desugar::source_file_to_judg_ment;
        // use crate::resolve::parse_source_file;
        // use crate::rowan_ast::{string_to_syntax, to_tree};
        // use crate::type_inference::{to_judgment, TypeError, UiMetadata, UiPrim};
        // use xi_core::judgment::Judgment;

        // pub fn frontend(text: &str) -> Result<Judgment<UiPrim, UiMetadata>, TypeError> {
        //     let syntax_node = string_to_syntax(text);
        //     dbg!(&syntax_node);
        //     // syntax_node is the rowan tree level
        //     let source_file = parse_source_file(&syntax_node);
        //     dbg!(&source_file);
        //     // source_file is at the name resolution level
        //     let judg_ment = source_file_to_judg_ment(source_file);
        //     // judg_ment is at the desugar level.
        //     dbg!(&judg_ment);
        //     let judgment = to_judgment(judg_ment);
        //     dbg!(judgment.unwrap())
        //     // this lands in Judgment<UiPrim, Metadata>
        // }

        // This is good! For > and >=
        let text1 = "fn f |x| {val x}  val f 5";
        dbg!(crate::frontend(text1));

        // This is good! For > and > =
        let text2 = "fn f |x : Int| -> Int {val x}
        fn g |x| {val f x}
        val g 5";
        dbg!(crate::frontend(text2));

        // This is also good! for both
        let text3 = "fn f |x| -> Int {val x}
        val 4";
        dbg!(crate::frontend(text3));

        //This is also good! FOr > and >=
        let text4 = "fn f |x: Int| {val x}
        val 4";
        dbg!(crate::frontend(text4));

        //This works now. for >= line 84
        let text5 = "val lambda |T : Type, t : T| t
        ";
        dbg!(crate::frontend(text5));

        //This is not good!
        let text6 = " fn test |T : Type, t : T| { let id = lambda |x| x val id t }
        val 4";
        dbg!(crate::frontend(text6));

        let text7 = "let id = lambda |T : Type, t : T| t

        val 5";
        dbg!(crate::frontend(text7));
    }

    #[test]
    fn program2_test() {
        let text = "
        ffi \"./some_file.js\"{
            UnitType : Type,
            unit : UnitType,
            console_output: String -> IO UnitType,
        }
        let me = console_output(\"hello\")!
        val unit!
        ";
        // let text = "

        // ffi \"./some_file.js\"{
        //     five : String,
        //     console_output: String -> IO String,
        // }

        // let me = console_output(\"hello\")!
        // val five!
        // ";
        // let text = "

        // ffi \"./some_file.js\"{
        //     UnitType : Type,
        //     unit : UnitType,
        // }
        // val unit
        // ";
        dbg!(crate::frontend(text));
    }
}
