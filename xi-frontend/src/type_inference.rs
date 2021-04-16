use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use crate::{
    desugar::{Judg_ment, Judg_mentKind, Mod_uleItem},
    DefineItem, Module, ModuleItem,
};
// use crate::resolve::ResolvePrim;
// use rowan::TextRange;
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

#[derive(Debug)]
pub struct Context<'a, 'b> {
    var_map: BTreeMap<VarUuid, TypeVar>,
    resps: Vec<TypeVar>,
    constraints: &'a mut BTreeMap<TypeVar, Judgment<TypeVarPrim, UiMetadata>>,
    module: &'b Module,
}

impl<'a, 'b> Context<'a, 'b> {
    fn subcontext(
        &mut self,
        func: impl FnOnce(&mut Context) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError>,
    ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
        let mut subcontext = Context {
            var_map: self.var_map.clone(),
            resps: vec![],
            constraints: self.constraints,
            module: self.module,
        };

        let result = func(&mut subcontext)?;

        let new_result = subcontext.substitute(&result)?;

        let mut new_resps = subcontext.elim_resps(subcontext.resps.clone())?;

        self.resps.append(&mut new_resps);

        Ok(new_result)
    }

    fn new_type_var(&mut self) -> TypeVar {
        let type_var = TypeVar::new();
        // if type_var.0 == 8 {
        //     panic!("we found type_var 8")
        // }
        self.resps.push(type_var);
        type_var
    }

    fn assign_type_var(&mut self, index: VarUuid, type_var: TypeVar) {
        self.var_map.insert(index, type_var);
    }

    fn type_infer(
        &mut self,
        judg_ment: Judg_ment<UiPrim, UiMetadata>,
    ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
        match *judg_ment.0 {
            Judg_mentKind::Type => Ok(Judgment::u(None)),
            Judg_mentKind::Prim(prim) => self.subcontext(|ctx| {
                Ok(match prim.maybe_prim_type::<UiMetadata>() {
                    Some(_prim_type) => {
                        Judgment::prim_wo_prim_type(TypeVarPrim::Prim(prim.clone()), None)
                    }
                    None => {
                        if let UiPrim::Global(index) = prim {
                            let module_item = ctx.module.module_items.get(&index).unwrap();
                            if let ModuleItem::Define(define_item) = &*module_item {
                                let type_ = &define_item.type_;
                                let type_var_type =
                                    type_.define_prim(Rc::new(|t, prim_type, define_prim| {
                                        Judgment::prim(
                                            TypeVarPrim::Prim(t),
                                            define_prim(prim_type),
                                            None,
                                        )
                                    }));
                                Judgment::prim(TypeVarPrim::Prim(prim.clone()), type_var_type, None)
                            } else {
                                panic!("expected variable to be a global")
                            }
                        } else {
                            let alpha = ctx.new_type_var();
                            Judgment::prim(
                                TypeVarPrim::Prim(prim.clone()),
                                Judgment::prim_wo_prim_type(TypeVarPrim::TypeVar(alpha), None),
                                None,
                            )
                        }
                    }
                })
            }),
            Judg_mentKind::FreeVar(index) => self.subcontext(|ctx| {
                let type_var = ctx
                    .var_map
                    .get(&index)
                    .expect("FreeVar index should be available in the context");

                Ok(Judgment::free(index, ctx.lookup_type_var(type_var), None))
            }),
            Judg_mentKind::Pi(index, var_type, expr) => self.subcontext(|ctx| {
                let type_var = ctx.new_type_var();
                ctx.assign_type_var(index, type_var);

                if let Some(var_type) = var_type {
                    let inferred_type = ctx.type_infer(var_type)?;
                    ctx.add_constraint(type_var, inferred_type)?;
                }

                let expr = ctx.type_infer(expr)?;

                let pi_scope = expr.bind(index);

                Ok(Judgment::pi(ctx.lookup_type_var(&type_var), pi_scope, None))
            }),
            Judg_mentKind::Lam(index, var_type, expr) => self.subcontext(|ctx| {
                let type_var = ctx.new_type_var();
                ctx.assign_type_var(index, type_var);

                if let Some(var_type) = var_type {
                    let inferred_type = ctx.type_infer(var_type)?;
                    ctx.add_constraint(type_var, inferred_type)?;
                }

                let expr = ctx.type_infer(expr)?;

                let lam_scope = expr.bind(index);

                Ok(Judgment::lam(
                    ctx.lookup_type_var(&type_var),
                    lam_scope,
                    None,
                ))
            }),
            Judg_mentKind::App(old_func, old_arg) => self.subcontext(|ctx| {
                let func = ctx.type_infer(old_func)?;

                match *func.type_of().expect("please not none").tree {
                    JudgmentKind::Pi(arg_type, _sexpr) => {
                        let arg = ctx.type_check(old_arg, arg_type)?;

                        Ok(Judgment::app_unchecked(func, arg, None))
                    }
                    JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var), _prim_type) => {
                        let epsilon = ctx.new_type_var();
                        let delta = ctx.new_type_var();
                        let arg = ctx.type_infer(old_arg)?;
                        ctx.add_constraint(
                            type_var,
                            Judgment::pi_unchecked(
                                Judgment::prim_wo_prim_type(TypeVarPrim::TypeVar(epsilon), None),
                                Judgment::prim_wo_prim_type(TypeVarPrim::TypeVar(delta), None)
                                    .to_scoped(),
                                None,
                            ),
                        )?;

                        ctx.add_constraint(epsilon, arg.type_of().expect("failure"))?;

                        Ok(Judgment::app_unchecked(func, arg, None))
                    }
                    _ => panic!("the type of func should be a pi or a type variable"),
                }
            }),
            Judg_mentKind::Let(old_arg, index, old_var_type, rest) => {
                // if the old_var_type is not None, then we use it to type check arg.
                // if var_type is none, we type_infer arg and use it as the var_type for fv(index)
                self.subcontext(|ctx| {
                    let (arg, var_type) = match old_var_type {
                        Some(old_var_type) => {
                            let var_type = ctx.type_infer(old_var_type.clone())?;
                            let arg = ctx.type_check(old_arg, var_type.clone())?;
                            (arg, var_type)
                        }
                        None => {
                            let arg = ctx.type_infer(old_arg)?;
                            let var_type = arg.type_of().unwrap();
                            (arg, var_type)
                        }
                    };

                    // we create a new type_var and assign it to var_type_inferred:
                    let type_var = ctx.new_type_var();
                    ctx.assign_type_var(index, type_var);
                    ctx.add_constraint(type_var, var_type)?;

                    let rest = ctx.type_infer(rest)?;

                    let lam_scope = rest.bind(index);
                    let lam_judgment =
                        Judgment::lam(ctx.lookup_type_var(&type_var), lam_scope, None);

                    Ok(Judgment::app_unchecked(lam_judgment, arg, None))
                })
            }
            Judg_mentKind::Bind(old_arg, old_func) => self.subcontext(|ctx| {
                use xi_proc_macro::term;
                use TypeVarPrim::{Prim, TypeVar};
                use UiPrim::{IOBind, IOMonad};

                let alpha = ctx.new_type_var();
                let beta = ctx.new_type_var();
                let io_alpha = ctx.new_type_var();
                let alpha_to_io_beta = ctx.new_type_var();

                let func = ctx.type_infer(old_func)?;
                let arg = ctx.type_infer(old_arg)?;

                ctx.add_constraint(io_alpha, term!([Prim(IOMonad)][TypeVar(alpha)]))?;
                ctx.add_constraint(io_alpha, arg.type_of().unwrap())?;

                ctx.add_constraint(
                    alpha_to_io_beta,
                    term!([TypeVar(alpha)] -> [Prim(UiPrim::IOMonad)] [TypeVar(beta)]),
                )?;
                ctx.add_constraint(alpha_to_io_beta, func.type_of().unwrap())?;

                Ok(Judgment::app_unchecked_vec(
                    Judgment::prim_wo_prim_type(Prim(IOBind), None),
                    vec![
                        Judgment::prim_wo_prim_type(TypeVar(alpha), None),
                        Judgment::prim_wo_prim_type(TypeVar(beta), None),
                        arg,
                        func,
                    ],
                    None,
                ))
            }),
            Judg_mentKind::Pure(old_expr) => self.subcontext(|ctx| {
                use TypeVarPrim::Prim;
                use UiPrim::IOPure;

                let expr = ctx.type_infer(old_expr)?;

                let expr_type = expr.type_of().unwrap();

                Ok(Judgment::app_unchecked_vec(
                    Judgment::prim_wo_prim_type(Prim(IOPure), None),
                    vec![expr_type, expr],
                    None,
                ))
            }),
            Judg_mentKind::Original(judgment) => Ok(judgment.define_prim(Rc::new(
                |ui_prim, prim_type, define_prim| {
                    Judgment::prim(TypeVarPrim::Prim(ui_prim), define_prim(prim_type), None)
                },
            ))),
        }
    }

    fn type_check(
        &mut self,
        expr: Judg_ment<UiPrim, UiMetadata>,
        expected: Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
        match (*expr.0.clone(), *expected.tree.clone()) {
            (
                Judg_mentKind::Lam(index, lam_var_type, lam_body),
                JudgmentKind::Pi(pi_var_type, pi_body),
            ) => self.subcontext(|ctx| {
                let type_var = ctx.new_type_var();
                ctx.assign_type_var(index, type_var);

                if let Some(var_type) = lam_var_type {
                    let inferred_type = ctx.type_infer(var_type.clone())?;
                    ctx.add_constraint(type_var, inferred_type)?;
                }

                ctx.add_constraint(type_var, pi_var_type)?;

                let new_pi_body = pi_body.replace_free_var(index);

                let new_lam_body = ctx.type_check(lam_body, new_pi_body)?;

                let new_lam_sbody = new_lam_body.bind(index);
                Ok(Judgment::lam(
                    ctx.lookup_type_var(&type_var),
                    new_lam_sbody,
                    None,
                ))
            }),
            _ => self.subcontext(move |ctx| {
                let type_var = ctx.new_type_var();

                let inferred_expr = ctx.type_infer(expr.clone())?;

                ctx.add_constraint(type_var, inferred_expr.type_of().unwrap())?;
                ctx.add_constraint(type_var, expected.clone())?;
                Ok(inferred_expr)
            }),
        }
    }

    // some way of replacing greek letters with things
    fn add_constraint(
        &mut self,
        type_var: TypeVar,
        replacement: Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<(), TypeError> {
        let result = match self.constraints.get(&type_var) {
            Some(result) => {
                let result = result.clone();
                self.unify(&result, &replacement)?
            }
            None => replacement,
        };

        self.constraints.insert(type_var, result);
        Ok(())
    }

    fn lookup_type_var(&self, type_var: &TypeVar) -> Judgment<TypeVarPrim, UiMetadata> {
        match self.constraints.get(type_var) {
            Some(inferred_type) => inferred_type.clone(),
            None => Judgment::prim(TypeVarPrim::TypeVar(*type_var), Judgment::u(None), None),
        }
    }

    // This is the unify function in Hindley-Milner systems.
    fn unify(
        &mut self,
        lhs: &Judgment<TypeVarPrim, UiMetadata>,
        rhs: &Judgment<TypeVarPrim, UiMetadata>,
    ) -> Result<Judgment<TypeVarPrim, UiMetadata>, TypeError> {
        let lhs = lhs.clone();
        let rhs = rhs.clone();

        if let JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var), _prim_type) = &*lhs.tree {
            self.add_constraint(*type_var, rhs.clone())?;
            return Ok(rhs);
        }
        if let JudgmentKind::Prim(TypeVarPrim::TypeVar(type_var), _prim_type) = &*rhs.tree {
            self.add_constraint(*type_var, lhs.clone())?;
            return Ok(lhs);
        }

        let result = match (&*lhs.tree, &*rhs.tree) {
            (JudgmentKind::Type, JudgmentKind::Type) => Judgment::u(None),
            (JudgmentKind::FreeVar(index1, var_type), JudgmentKind::FreeVar(index2, _))
                if index1 == index2 =>
            {
                Judgment::free(*index1, var_type.clone(), None)
            }

            (
                JudgmentKind::Prim(TypeVarPrim::Prim(prim1), prim_type1),
                JudgmentKind::Prim(TypeVarPrim::Prim(prim2), _prim_type2),
            ) if prim1 == prim2 => {
                Judgment::prim(TypeVarPrim::Prim(prim1.clone()), prim_type1.clone(), None)
            }

            (JudgmentKind::Pi(var_type1, sexpr1), JudgmentKind::Pi(var_type2, sexpr2)) => {
                let (index1, expr1) = sexpr1.clone().unbind();
                let expr2 = sexpr2.clone().replace_free_var(index1);

                Judgment::pi_unchecked(
                    self.unify(&*var_type1, &*var_type2)?,
                    self.unify(&expr1, &expr2)?.bind(index1),
                    None,
                )
            }

            (JudgmentKind::Lam(_, _), _) => {
                panic!("our nbe lord has failed us, this is all over... here goes dongi")
            }
            (JudgmentKind::BoundVar(_index1, _var_type), _) => {
                unreachable!();
            }

            (JudgmentKind::App(func1, elem1), JudgmentKind::App(func2, elem2)) => {
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
            let result = match &*judgment.tree {
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
                            prim_type.clone(),
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
                JudgmentKind::Pi(var_type, sexpr) => {
                    let (index, expr) = sexpr.clone().unbind();
                    Judgment::pi_unchecked(
                        substitute_rec(ctx, &var_type, seen)?,
                        substitute_rec(ctx, &expr, seen)?.bind(index),
                        Some(metadata),
                    )
                }
                JudgmentKind::Lam(var_type, sexpr) => {
                    let (index, expr) = sexpr.clone().unbind();
                    Judgment::lam(
                        substitute_rec(ctx, &var_type, seen)?,
                        substitute_rec(ctx, &expr, seen)?.bind(index),
                        Some(metadata),
                    )
                }
                JudgmentKind::BoundVar(_int, _var_type) => {
                    unreachable!();
                }
                JudgmentKind::App(func, arg) => Judgment::app_unchecked(
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
    Global(VarUuid),
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
            Global(_index) => None,
        }
    }
}

// pub fn to_judgment(
//     judg_ment: Judg_ment<UiPrim, UiMetadata>,
// ) -> Result<Judgment<UiPrim, UiMetadata>, TypeError> {
//     let mut ctx = Context {
//         var_map: BTreeMap::new(),
//         resps: vec![],
//         constraints: &mut BTreeMap::new(),
//     };

//     let judgment_with_type_var = ctx.type_infer(judg_ment)?;

//     if !ctx.resps.is_empty() {
//         panic!("we shouldn't have any type variables left");
//     }

//     Ok(
//         judgment_with_type_var.define_prim(Rc::new(|type_var_prim, prim_type, define_prim| {
//             match type_var_prim {
//                 TypeVarPrim::TypeVar(_) => unreachable!("resps should be empty"),
//                 TypeVarPrim::Prim(prim) => Judgment::prim(prim, define_prim(prim_type), None),
//             }
//         })),
//     )
// }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeVarPrim {
    TypeVar(TypeVar),
    Prim(UiPrim),
}

impl Primitive for TypeVarPrim {
    fn maybe_prim_type<S: Metadata>(&self) -> Option<Judgment<Self, S>> {
        let ans = match self {
            TypeVarPrim::TypeVar(_type_var) => Judgment::u(None),
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

pub fn type_infer_mod_ule_item(module: &Module, mod_ule_item: Mod_uleItem) -> ModuleItem {
    let mut ctx = Context {
        var_map: BTreeMap::new(),
        resps: vec![],
        constraints: &mut BTreeMap::new(),
        module: module,
    };
    let type_var_judgment = match mod_ule_item.expected_type {
        Some(expected_type) => {
            let expected_type = ctx.type_infer(expected_type).unwrap();
            ctx.type_check(mod_ule_item.impl_, expected_type).unwrap()
        }
        None => ctx.type_infer(mod_ule_item.impl_).unwrap(),
    };

    let judgment: Judgment<UiPrim, UiMetadata> =
        type_var_judgment.define_prim(Rc::new(|type_var_prim, prim_type, define_prim| {
            match type_var_prim {
                TypeVarPrim::TypeVar(_) => unreachable!("resps should be empty"),
                TypeVarPrim::Prim(prim) => Judgment::prim(prim, define_prim(prim_type), None),
            }
        }));

    let define_item = DefineItem {
        type_: judgment.type_of().unwrap(),
        impl_: judgment,
        type_dependencies: vec![],
    };
    ModuleItem::Define(define_item)
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
// #[cfg(test)]
// mod test {
//     use super::UiPrim::*;
//     use crate::frontend;
//     use xi_core::judgment::Judgment;
//     use xi_core::judgment::Primitive;
//     use xi_proc_macro::term;
//     use xi_uuid::VarUuid;

//     #[test]
//     fn sanity_test() {
//         let text0 = "val lambda |T : Type| T";
//         assert_eq!(frontend(text0).unwrap(), term!(Lam | T: U | T));

//         let text1 = "val lambda |T : Type, t : T| t";
//         assert_eq!(frontend(text1).unwrap(), term!(Lam | T: U, t: T | t))
//     }

//     #[test]
//     fn type_infer_test() {
//         let text1 = "fn f |x| {val x}  val f 5";
//         assert_eq!(
//             frontend(text1).unwrap(),
//             term!((Lam |bv0 : [NumberType] -> [NumberType]| bv0 [NumberElem("5".into())]) (
//                     Lam | bv0: [NumberType] | bv0
//                 )
//             )
//         );

//         let text2 = "fn f |x : Int| -> Int {val x}
//         fn g |x| {val f x}
//         val g 5";
//         assert_eq!(
//             frontend(text2).unwrap(),
//             term!((Lam |bv0: [NumberType] -> [NumberType]| (Lam |bv1: [NumberType] -> [NumberType]| bv1 [NumberElem("5".into())]) (Lam |bv1: [NumberType]| bv0 bv1)) (Lam |bv0: [NumberType]| bv0)),
//         );

//         let text3 = "fn f |x| -> Int {val x}
//         val 4";
//         assert_eq!(
//             frontend(text3).unwrap(),
//             term!((Lam |bv0: [NumberType] -> [NumberType]| [NumberElem("4".into())]) (Lam |bv0: [NumberType]| bv0)),
//         );

//         let text4 = "val lambda |T : Type, t : T| t
//         ";
//         assert_eq!(frontend(text4).unwrap(), term!(Lam | T: U, t: T | t));

//         let text5 = " fn test |T : Type, t : T| { let id = lambda |x| x val id t }
//         val 4";
//         assert_eq!(
//             frontend(text5).unwrap(),
//             term!((Lam |bv0: Pi |bv0: U| bv0 -> bv0| [NumberElem("4".into())]) (Lam |bv0: U, bv1: bv0| (Lam |bv2: bv0 -> bv0| bv2 bv1) (Lam |bv2: bv0| bv2))),
//         );
//     }

//     #[test]
//     fn ffi_test() {
//         let text = "
//         ffi \"./some_file.js\"{
//             UnitType : Type,
//             unit : UnitType,
//             console_output: String -> IO UnitType,
//         }
//         let me = console_output(\"hello\")!
//         val unit!
//         ";

//         crate::frontend(text).unwrap();
//     }

//     #[test]
//     fn dependent_type_test() {
//         let text = " val lambda |T: Type, succ: T -> T| succ ";
//         assert_eq!(
//             crate::frontend(text).unwrap(),
//             term!(Lam |T : U, succ : T -> T| succ),
//         );

//         let text2 = " val lambda |T: Type, zero : T, succ: T -> T| succ zero";
//         assert_eq!(
//             crate::frontend(text2).unwrap(),
//             term!(Lam |T : U, zero : T, succ : T -> T| succ zero),
//         );
//     }

//     #[test]
//     fn actual_type_inference_test() {
//         let text = "let id : Pi | T : Type, t : T| T = lambda |T, t| t
//         val 5
//         ";
//         assert_eq!(
//             crate::frontend(text).unwrap(),
//             term!((Lam |bv0: Pi |bv0: U| bv0 -> bv0| [NumberElem("5".into())]) (Lam |bv0: U, bv1: bv0| bv1)),
//         );
//     }
// }
