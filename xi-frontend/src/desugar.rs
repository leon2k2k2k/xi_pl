use std::collections::{BTreeMap, BTreeSet};

use crate::resolve::{self, Binders, Expr, ExprKind, Stmt, StmtKind};
use crate::type_inference::UiPrim;
use crate::{resolve::ResolvePrim, type_inference::UiMetadata};
use resolve::{new_span, LocalOrGlobal, StringTokenKind, Var};
use xi_core::judgment::{Metadata, Primitive};
use xi_uuid::VarUuid;

pub use crate::resolve::Span;

#[allow(non_camel_case_types)]
#[derive(Clone)]
pub struct Judg_ment<T, S>(pub Box<Judg_mentKind<T, S>>, pub S);

#[allow(non_camel_case_types)]
#[derive(Clone)]
pub enum Judg_mentKind<T, S> {
    Type,
    Prim(T),
    FreeVar(VarUuid),
    BoundVar(u32),
    Pi(Option<Judg_ment<T, S>>, ScopedJudg_ment<T, S>),
    Lam(Option<Judg_ment<T, S>>, ScopedJudg_ment<T, S>),
    App(Judg_ment<T, S>, Judg_ment<T, S>),
    Let(
        Judg_ment<T, S>,
        Option<Judg_ment<T, S>>,
        ScopedJudg_ment<T, S>,
    ),
    Bind(Judg_ment<T, S>, Judg_ment<T, S>),
    Pure(Judg_ment<T, S>),
}

#[allow(non_camel_case_types)]
#[derive(Clone)]
pub struct ScopedJudg_ment<T, S>(pub(crate) Judg_ment<T, S>);

impl<T: Primitive, S: Metadata> ScopedJudg_ment<T, S> {
    pub fn unbind(self) -> (VarUuid, Judg_ment<T, S>) {
        let index = VarUuid::new();
        fn unbind_rec<T: Primitive, S: Metadata>(
            judgment: Judg_ment<T, S>,
            index: VarUuid,
            depth: u32,
        ) -> Judg_ment<T, S> {
            let judgmentkind = match *judgment.0 {
                Judg_mentKind::Type => Judg_mentKind::Type,
                Judg_mentKind::Prim(prim) => Judg_mentKind::Prim(prim),
                Judg_mentKind::FreeVar(i) => Judg_mentKind::FreeVar(i),
                Judg_mentKind::BoundVar(i) => {
                    if i == depth {
                        Judg_mentKind::FreeVar(index)
                    } else {
                        Judg_mentKind::BoundVar(i)
                    }
                }
                Judg_mentKind::Pi(var_type, sexpr) => Judg_mentKind::Pi(
                    var_type.map(|var_type| unbind_rec(var_type, index, depth)),
                    ScopedJudg_ment(unbind_rec(sexpr.0, index, depth + 1)),
                ),
                Judg_mentKind::Lam(var_type, sexpr) => Judg_mentKind::Lam(
                    var_type.map(|var_type| unbind_rec(var_type, index, depth)),
                    ScopedJudg_ment(unbind_rec(sexpr.0, index, depth + 1)),
                ),
                Judg_mentKind::App(func, arg) => Judg_mentKind::App(
                    unbind_rec(func, index, depth),
                    unbind_rec(arg, index, depth),
                ),
                Judg_mentKind::Let(expr, expr_type, rest) => Judg_mentKind::Let(
                    unbind_rec(expr, index, depth),
                    expr_type.map(|expr_type| unbind_rec(expr_type, index, depth)),
                    ScopedJudg_ment(unbind_rec(rest.0, index, depth + 1)),
                ),
                Judg_mentKind::Bind(expr, rest) => Judg_mentKind::Bind(
                    unbind_rec(expr, index, depth),
                    unbind_rec(rest, index, depth),
                ),
                Judg_mentKind::Pure(expr) => Judg_mentKind::Pure(unbind_rec(expr, index, depth)),
            };
            Judg_ment(Box::new(judgmentkind), judgment.1)
        }
        (index, unbind_rec(self.0, index, 0))
    }

    pub fn instantiate(self, sub: &Judg_ment<T, S>) -> Judg_ment<T, S> {
        let (index, judgment) = self.unbind();
        fn instantiate_rec<T: Primitive, S: Metadata>(
            judgment: Judg_ment<T, S>,
            index: VarUuid,
            sub: &Judg_ment<T, S>,
        ) -> Judg_ment<T, S> {
            let judgmentkind = match *judgment.0 {
                Judg_mentKind::Type => Judg_mentKind::Type,
                Judg_mentKind::Prim(prim) => Judg_mentKind::Prim(prim),
                Judg_mentKind::FreeVar(i) => {
                    if i == index {
                        *sub.0.clone()
                    } else {
                        Judg_mentKind::FreeVar(i)
                    }
                }
                Judg_mentKind::BoundVar(_) => unreachable!(),
                Judg_mentKind::Pi(var_type, sexpr) => {
                    let (i, expr) = sexpr.unbind();
                    let inst_expr = instantiate_rec(expr, index, sub);
                    let inst_sexpr = inst_expr.bind(i);
                    let new_var_type =
                        var_type.map(|var_type| instantiate_rec(var_type, index, sub));
                    Judg_mentKind::Pi(new_var_type, inst_sexpr)
                }
                Judg_mentKind::Lam(var_type, sexpr) => {
                    let (i, expr) = sexpr.unbind();
                    let inst_expr = instantiate_rec(expr, index, sub);
                    let inst_sexpr = inst_expr.bind(i);
                    let new_var_type =
                        var_type.map(|var_type| instantiate_rec(var_type, index, sub));
                    Judg_mentKind::Lam(new_var_type, inst_sexpr)
                }
                Judg_mentKind::App(func, arg) => Judg_mentKind::App(
                    instantiate_rec(func, index, sub),
                    instantiate_rec(arg, index, sub),
                ),
                Judg_mentKind::Let(expr, expr_type, rest) => {
                    let (i, rest) = rest.unbind();
                    let new_expr_type =
                        expr_type.map(|expr_type| instantiate_rec(expr_type, index, sub));
                    Judg_mentKind::Let(
                        instantiate_rec(expr, index, sub),
                        new_expr_type,
                        instantiate_rec(rest, index, sub).bind(i),
                    )
                }
                Judg_mentKind::Bind(arg, rest) => Judg_mentKind::App(
                    instantiate_rec(arg, index, sub),
                    instantiate_rec(rest, index, sub),
                ),
                Judg_mentKind::Pure(expr) => Judg_mentKind::Pure(instantiate_rec(expr, index, sub)),
            };
            Judg_ment(Box::new(judgmentkind), judgment.1)
        }

        instantiate_rec(judgment, index, sub)
    }
}

impl<T: Primitive, S: Metadata> Judg_ment<T, S> {
    pub fn bind(self, index: VarUuid) -> ScopedJudg_ment<T, S> {
        fn bind_rec<T: Primitive, S: Metadata>(
            judgment: Judg_ment<T, S>,
            index: VarUuid,
            depth: u32,
        ) -> Judg_ment<T, S> {
            let judgmentkind = match *judgment.0 {
                Judg_mentKind::Type => Judg_mentKind::Type,
                Judg_mentKind::Prim(prim) => Judg_mentKind::Prim(prim),
                Judg_mentKind::FreeVar(i) => {
                    if i == index {
                        Judg_mentKind::BoundVar(depth)
                    } else {
                        Judg_mentKind::FreeVar(i)
                    }
                }
                Judg_mentKind::BoundVar(i) => Judg_mentKind::BoundVar(i),
                Judg_mentKind::Pi(var_type, sexpr) => Judg_mentKind::Pi(
                    var_type.map(|var_type| bind_rec(var_type, index, depth)),
                    ScopedJudg_ment(bind_rec(sexpr.0, index, depth + 1)),
                ),
                Judg_mentKind::Lam(var_type, sexpr) => Judg_mentKind::Lam(
                    var_type.map(|var_type| bind_rec(var_type, index, depth)),
                    ScopedJudg_ment(bind_rec(sexpr.0, index, depth + 1)),
                ),
                Judg_mentKind::App(func, arg) => {
                    Judg_mentKind::App(bind_rec(func, index, depth), bind_rec(arg, index, depth))
                }
                Judg_mentKind::Let(expr, expr_type, rest) => Judg_mentKind::Let(
                    bind_rec(expr, index, depth),
                    expr_type.map(|expr_type| bind_rec(expr_type, index, depth)),
                    ScopedJudg_ment(bind_rec(rest.0, index, depth + 1)),
                ),
                Judg_mentKind::Bind(arg, rest) => {
                    Judg_mentKind::Bind(bind_rec(arg, index, depth), bind_rec(rest, index, depth))
                }
                Judg_mentKind::Pure(expr) => Judg_mentKind::Pure(bind_rec(expr, index, depth)),
            };
            Judg_ment(Box::new(judgmentkind), judgment.1)
        }
        ScopedJudg_ment(bind_rec(self, index, 0))
    }
}

impl Judg_ment<UiPrim, UiMetadata> {
    fn u() -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Type), UiMetadata {})
    }

    fn prim(prim: UiPrim) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Prim(prim)), UiMetadata {})
    }

    fn freevar(var: VarUuid) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::FreeVar(var)), UiMetadata {})
    }

    fn app(
        func: Judg_ment<UiPrim, UiMetadata>,
        arg: Judg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::App(func, arg)), UiMetadata {})
    }

    fn bind_(
        ma: Judg_ment<UiPrim, UiMetadata>,
        a_to_mb: Judg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Bind(ma, a_to_mb)), UiMetadata {})
    }

    fn pure(a: Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Pure(a)), UiMetadata {})
    }

    pub fn lam(
        var_type: Option<Judg_ment<UiPrim, UiMetadata>>,
        sexpr: ScopedJudg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Lam(var_type, sexpr)), UiMetadata {})
    }

    fn fun(
        arg_type: Judg_ment<UiPrim, UiMetadata>,
        ret_type: Judg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment::pi(Some(arg_type), ret_type.bind(VarUuid::new()))
    }

    fn pi(
        var_type: Option<Judg_ment<UiPrim, UiMetadata>>,
        sexpr: ScopedJudg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Pi(var_type, sexpr)), UiMetadata {})
    }

    fn let_(
        arg: Judg_ment<UiPrim, UiMetadata>,
        var_type: Option<Judg_ment<UiPrim, UiMetadata>>,
        rest: ScopedJudg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(
            Box::new(Judg_mentKind::Let(arg, var_type, rest)),
            UiMetadata {},
        )
    }
}

struct Context {
    prim_map: BTreeMap<VarUuid, ResolvePrim>,
}

impl Context {
    fn binders_to_pi(
        &self,
        binders: Binders,
        init: Judg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        let mut result = init;
        let var_list = &binders.0;
        for (var, var_type) in var_list.iter().rev() {
            let var_type = var_type
                .clone()
                .map(|var_type| self.desugar_expr(&var_type));
            result = Judg_ment::pi(var_type, result.bind(var.index));
        }
        result
    }

    fn binders_to_lam(
        &self,
        binders: Binders,
        init: Judg_ment<UiPrim, UiMetadata>,
    ) -> Judg_ment<UiPrim, UiMetadata> {
        let mut result = init;
        let var_list = &binders.0;
        for (var, var_type) in var_list.iter().rev() {
            let var_type = var_type
                .clone()
                .map(|var_type| self.desugar_expr(&var_type));
            result = Judg_ment::lam(var_type, result.bind(var.index));
        }
        result
    }
    // this desugars the body of a stmt_expr
    fn desugar_stmt_vec(&self, stmts: &[Stmt]) -> Judg_ment<UiPrim, UiMetadata> {
        if stmts.is_empty() {
            panic!("expected a val or something");
        }

        let stmt = &stmts[0];
        let stmt_rest = &stmts[1..];

        match &stmt.0 {
            StmtKind::Let(var, expected_type, expr, _with_set) => {
                if let ExprKind::Bang(expr1) = &*expr.0 {
                    let bind_arg = self.desugar_expr(&expr1);
                    let rest = self.desugar_stmt_vec(stmt_rest);

                    let rest_kind = Judg_ment::lam(
                        expected_type
                            .clone()
                            .map(|var_type| self.desugar_expr(&var_type)),
                        rest.bind(var.index),
                    );
                    Judg_ment::bind_(bind_arg, rest_kind)
                } else {
                    let arg = self.desugar_expr(expr);
                    let rest = self.desugar_stmt_vec(stmt_rest);
                    let var_type = expected_type
                        .clone()
                        .map(|var_type| self.desugar_expr(&var_type));
                    Judg_ment::let_(arg, var_type, rest.bind(var.index))
                }
            }

            StmtKind::Val(expr) => self.desugar_expr(&expr),
            StmtKind::Ffi(file_name, vars) => {
                let rest = self.desugar_stmt_vec(stmt_rest);

                let mut result = rest;

                let mut vars = vars.clone();

                vars.reverse();

                for (var, expr) in vars {
                    let func =
                        Judg_ment::lam(Some(self.desugar_expr(&expr)), result.bind(var.index));

                    let ffi = Judg_ment::prim(UiPrim::Ffi(file_name.clone(), var.name));
                    result = Judg_ment::app(func, ffi)
                }
                result
            }
            StmtKind::Enum(_, _, _, _) => {
                unimplemented!()
            }
            StmtKind::Struct(_, _, _) => {
                unimplemented!()
            }
            StmtKind::Import(_, _) => {
                unreachable!("we handle imports differently")
            }
        }

        // let first_span = stmts[0].1;
        // let last_span = stmts[stmts.len() - 1].1;
        // Judg_ment(Box::new(result_kind), first_span.cover(last_span))
        // Judg_ment(Box::new(result_kind), UiMetadata{})
    }

    // fn desugar_var_binder(&mut self, var: &resolve::VarBinder) -> (VarBinder, Context) {
    //     let new_ctx = self.clone();
    //     let depth =
    //     new_ctx.var_map.ins

    //     let var_binder = VarBinder {
    //         var_type: var.var_type.map(|var_type| self.desugar_expr(&var_type)),
    //         name: var.name,
    //         span: var.span,
    //     }
    // }

    // fn new_var_binder(&mut self, ) -> (VarBinder, Context) {

    // }

    fn desugar_var(&self, var: &resolve::Var) -> Judg_ment<UiPrim, UiMetadata> {
        match self.prim_map.get(&var.index) {
            Some(prim) => {
                let prim = match prim {
                    ResolvePrim::IOMonad => UiPrim::IOMonad,
                    ResolvePrim::String => UiPrim::StringType,
                    ResolvePrim::Int => UiPrim::NumberType,
                };
                Judg_ment::prim(prim.clone())
            }
            None => match var.local_or_global {
                resolve::LocalOrGlobal::Local => {
                    Judg_ment(Box::new(Judg_mentKind::FreeVar(var.index)), UiMetadata {})
                }
                resolve::LocalOrGlobal::Global => Judg_ment::prim(UiPrim::Global(var.index)),
            },
        }
    }

    fn desugar_expr(&self, expr: &Expr) -> Judg_ment<UiPrim, UiMetadata> {
        match &*expr.0 {
            ExprKind::Var(var) => self.desugar_var(var),
            ExprKind::Type => Judg_ment::u(),
            ExprKind::Bang(expr) => Judg_ment::pure(self.desugar_expr(expr)),
            ExprKind::App(func, elem) => {
                Judg_ment::app(self.desugar_expr(&func), self.desugar_expr(&elem))
            }
            ExprKind::Fun(source, target) => {
                Judg_ment::fun(self.desugar_expr(&source), self.desugar_expr(&target))
            }
            ExprKind::Lam(binders, lam_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&lam_expr);

                let mut result = expr;

                for (var, var_type) in var_list.iter().rev() {
                    let var_type = var_type
                        .clone()
                        .map(|var_type| self.desugar_expr(&var_type));
                    result = Judg_ment::lam(var_type, result.bind(var.index));
                }
                result
            }
            ExprKind::Pi(binders, pi_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&pi_expr);

                let mut result = expr;

                for (var, var_type) in var_list.iter().rev() {
                    let var_type = var_type
                        .clone()
                        .map(|var_type| self.desugar_expr(&var_type));
                    result = Judg_ment::pi(var_type, result.bind(var.index));
                }
                result
            }
            ExprKind::Stmt(stmt_vec) => self.desugar_stmt_vec(&stmt_vec),
            ExprKind::Member(_, _) => todo!(),
            ExprKind::StringLit(string_components) => {
                if string_components.len() > 1 {
                    panic!("we don't support string escapes yet :)");
                }

                // empty string
                if string_components.len() == 0 {
                    Judg_ment::prim(UiPrim::StringElem("".into()))
                } else {
                    match string_components[0].0.clone() {
                        StringTokenKind::Escape(_) => {
                            panic!("we don't support string escapes yet :)");
                        }
                        StringTokenKind::String(string) => {
                            Judg_ment::prim(UiPrim::StringElem(string))
                        }
                    }
                }
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let judg_ment_op = Judg_ment::prim(UiPrim::Binary(op.clone()));
                Judg_ment::app(
                    Judg_ment::app(judg_ment_op, self.desugar_expr(lhs)),
                    self.desugar_expr(rhs),
                )
            }
            ExprKind::Number(num) => Judg_ment::prim(UiPrim::NumberElem(num.clone())),
            ExprKind::Tuple(_exprs) => {
                unimplemented!("we don't support tuples yet")
            }
        }
    }
    fn desugar_module_stmt(&self, module_stmt: Stmt) -> Vec<Mod_uleItem> {
        match module_stmt.0 {
            StmtKind::Let(var, var_type, expr, with_list) => {
                let expected_type = var_type.map(|expr| self.desugar_expr(&expr));
                let impl_ = self.desugar_expr(&expr);

                vec![Mod_uleItem {
                    var: var,
                    impl_: impl_,
                    expected_type: expected_type,
                    backend: module_stmt.1,
                    with_list: with_list,
                }]
            }
            StmtKind::Val(_) => {
                panic!("there shouldn't be a val statement as module statement")
            }
            StmtKind::Ffi(file_name, result) => {
                let mut mod_ule_items = vec![];
                // we construct a module_let_stmt with an ffi_func declared inside
                //for every ffi function
                for (ffi_var, ffi_type) in result {
                    let var = Var {
                        index: ffi_var.clone().index,
                        name: ffi_var.clone().name,
                        span: new_span(),
                        local_or_global: LocalOrGlobal::Local,
                    };
                    let rest_kind: StmtKind =
                        StmtKind::Val(Expr(Box::new(ExprKind::Var(var)), new_span()));
                    let rest = Stmt(rest_kind, module_stmt.1.clone(), new_span());
                    let ffi_stmt_kind =
                        StmtKind::Ffi(file_name.clone(), vec![(ffi_var.clone(), ffi_type)]);
                    let ffi_stmt = Stmt(ffi_stmt_kind, module_stmt.1.clone(), new_span());
                    let stmts = vec![ffi_stmt, rest];
                    let mod_ule_item = Mod_uleItem {
                        var: ffi_var,
                        impl_: self.desugar_stmt_vec(&stmts),
                        expected_type: None,
                        backend: module_stmt.1.clone(),
                        with_list: BTreeSet::new(),
                    };
                    mod_ule_items.push(mod_ule_item)
                }
                mod_ule_items
            }
            StmtKind::Enum(var_binder, binders, self_var, variants) => {
                let mut mod_ule_items = vec![];

                // first we need to create: let [var_binder] = lam|binders| Pi|T : Type| (T or (variant1 -> T)) -> (T or (variant2 -> T)) -> T.
                //  each exprs1 is variant1[0] -> variant1[1] -> variant1[2]...
                //to do that, for each enum_item, we first create variant1[0] -> variant1[1] -> variant1[2]... -> T:

                let mut vec = vec![];

                for (_var, exprs) in &variants {
                    let mut judg_ment = Judg_ment::freevar(self_var.index);
                    for expr in exprs.iter().rev() {
                        judg_ment = Judg_ment::fun(self.desugar_expr(&expr), judg_ment);
                    }
                    vec.push(judg_ment)
                }

                let mut result = Judg_ment::freevar(self_var.index);
                // now we map them all together:
                for judg_ment in vec.into_iter().rev() {
                    result = Judg_ment::fun(judg_ment, result)
                }
                // now we bind the T:
                let result = Judg_ment::pi(Some(Judg_ment::u()), result.bind(self_var.index));
                // we need this later for eliminator.
                let result2 = result.clone();
                // now we add the lam|binders|
                let result = if let Some(binders) = binders.clone() {
                    self.binders_to_lam(binders, result)
                } else {
                    result
                };

                let first_mod_ule_item = Mod_uleItem {
                    var: var_binder.clone(),
                    impl_: result,
                    expected_type: None,
                    backend: module_stmt.1.clone(),
                    with_list: BTreeSet::new(),
                };

                mod_ule_items.push(first_mod_ule_item);

                // we will need with [name] for all the other statements, [name] is the name of the enum.
                let mut with_var = BTreeSet::new();
                with_var.insert(var_binder.clone().index);

                // we also need [name][binders]
                let mut name_binders = Judg_ment::prim(UiPrim::Global(var_binder.clone().index));

                if let Some(binders) = binders.clone() {
                    let var_list = &binders.0;
                    for (var, _var_type) in var_list {
                        name_binders = Judg_ment::app(name_binders, Judg_ment::freevar(var.index));
                    }
                };

                // now we need the constructors: for each (var, variant_i) in variants,
                // we get a map [variants.var.name]: pi |binders| expr0 -> expr1 -> ... -> [name] [binders]
                // = lambda|binders, e0: expr0, e1: expr,... T, variant1, variant2..., | variant_i e0 e1..., where e replace self with

                for (var, exprs) in variants.clone() {
                    // first we do expected_type, if there is a self, it need to be [name][binders]
                    let mut expected_type = name_binders.clone();
                    for expr in exprs.iter().rev() {
                        expected_type = Judg_ment::fun(self.desugar_expr(&expr), expected_type)
                    }
                    expected_type = expected_type
                        .bind(self_var.index.clone())
                        .instantiate(&name_binders.clone());

                    // if there is expr, then have expr -> [name] [binders]
                    // if let Some(expr) = expr {
                    //     let judg_ment1 = self.desugar_expr(&expr);
                    //     // we need to replace all self with name_binders:
                    //     let lam_judg_ment =
                    //         Judg_ment::lam(Some(Judg_ment::u()), judg_ment1.bind(self_var.index));
                    //     let app_judg_ment = Judg_ment::app(lam_judg_ment, name_binders.clone());
                    //     expected_type = Judg_ment::fun(app_judg_ment, expected_type)
                    // }
                    // now we add in the pi|binders|:
                    expected_type = if let Some(binders) = binders.clone() {
                        self.binders_to_pi(binders, expected_type)
                    } else {
                        expected_type
                    };

                    // now we move on to impl_:
                    // lambda|binders, e0: expr0, e1: expr,... T, variant1, variant2..., |
                    //variant_i e0 e1..., where e replace self with the complicated thingy
                    let t_index = VarUuid::new();

                    // first we do variant_i e0 e1:
                    let variant_i_index = var.index;
                    let mut impl_ = Judg_ment::freevar(variant_i_index);
                    let mut e_indexes = vec![];
                    for expr in &exprs {
                        let e_index = VarUuid::new();
                        e_indexes.push(e_index);
                        let arg = if let ExprKind::Var(var) = &*expr.0 {
                            if var.index == self_var.clone().index {
                                //example: for nat: we need succ (n T zero succ), basically we need to apply everything to n,
                                // so that's e_index T variant0 variant1
                                let mut arg = Judg_ment::freevar(e_index);
                                // apply T
                                arg = Judg_ment::app(arg, Judg_ment::freevar(t_index));

                                for (var, _exprs) in variants.clone() {
                                    arg = Judg_ment::app(arg, Judg_ment::freevar(var.index))
                                }
                                arg
                            } else {
                                Judg_ment::freevar(e_index)
                            }
                        } else {
                            Judg_ment::freevar(e_index)
                        };
                        impl_ = Judg_ment::app(impl_, arg);
                    }
                    // now we lambda over all the variant (different exprs):
                    for (var, _exprs) in variants.iter().rev() {
                        impl_ = Judg_ment::lam(None, impl_.bind(var.index))
                    }
                    // now we lam over T:
                    impl_ = Judg_ment::lam(None, impl_.bind(t_index));
                    // now we lam over expr0, expr1...
                    for e_index in e_indexes.iter().rev() {
                        impl_ = Judg_ment::lam(None, impl_.bind(*e_index));
                    }
                    // now we lam over binders...

                    impl_ = if let Some(binders) = binders.clone() {
                        self.binders_to_lam(binders, impl_)
                    } else {
                        impl_
                    };

                    let mod_ule_item = Mod_uleItem {
                        var: var,
                        impl_: impl_,
                        expected_type: Some(expected_type),
                        backend: module_stmt.1.clone(),
                        with_list: with_var.clone(),
                    };
                    mod_ule_items.push(mod_ule_item);
                }

                // lastly we get the eliminator:
                // [name]Elim : pi |binders| [name][binders] -> pi thing above = lambda||binders|,f, x| x
                // let's do the expected type:
                let mut expected_type = name_binders.clone();
                // map to the pi thing above
                expected_type = Judg_ment::fun(expected_type, result2);
                // pi all the binders:
                expected_type = if let Some(binders) = binders.clone() {
                    self.binders_to_pi(binders, expected_type)
                } else {
                    expected_type
                };

                // now the implementation: lambda||binders|, x| x
                let x_index = VarUuid::new();
                let mut impl_ = Judg_ment::lam(None, Judg_ment::freevar(x_index).bind(x_index));
                // now the binders:
                impl_ = if let Some(binders) = binders.clone() {
                    self.binders_to_lam(binders, impl_)
                } else {
                    impl_
                };
                let var = Var {
                    index: VarUuid::new(),
                    name: var_binder.clone().name + "Elim",
                    span: new_span(),
                    local_or_global: LocalOrGlobal::Global,
                };
                let mod_ule_item = Mod_uleItem {
                    var: var,
                    impl_: impl_,
                    expected_type: Some(expected_type),
                    backend: module_stmt.1,
                    with_list: with_var.clone(),
                };
                mod_ule_items.push(mod_ule_item);

                mod_ule_items
            }
            StmtKind::Struct(var_binder, binders, fields) => {
                let mut mod_ule_items = vec![];

                // creating the first mod_ule_stmt: let [name] = lam|binders| Pi|T : Type| (first_field -> second_field..  -> T) -> T

                let t_index = VarUuid::new();
                let mut result = Judg_ment::freevar(t_index);
                for field in fields.iter().rev() {
                    result = Judg_ment::fun(self.desugar_expr(&(field.1)), result)
                }
                result = Judg_ment::fun(result, Judg_ment::freevar(t_index));
                // now adding the Pi|T : Type|
                result = Judg_ment::pi(Some(Judg_ment::u()), result.bind(t_index));

                // now adding lams
                let result = if let Some(binders) = binders.clone() {
                    self.binders_to_lam(binders, result)
                } else {
                    result
                };

                let first_mod_ule_item = Mod_uleItem {
                    var: var_binder.clone(),
                    impl_: result,
                    expected_type: None,
                    backend: module_stmt.1.clone(),
                    with_list: BTreeSet::new(),
                };

                mod_ule_items.push(first_mod_ule_item);

                // we will need with [var_binder] for all the other statements

                let mut with_var = BTreeSet::new();
                with_var.insert(var_binder.clone().index);

                let mut struct_apply_to_binders =
                    Judg_ment::prim(UiPrim::Global(var_binder.clone().index));

                if let Some(binders) = binders.clone() {
                    let var_list = &binders.0;
                    for (var, _var_type) in var_list {
                        struct_apply_to_binders =
                            Judg_ment::app(struct_apply_to_binders, Judg_ment::freevar(var.index));
                    }
                };

                // now for each field we have an eliminator: with [name] let [field.0]: Pi |[binders]| ([name] [binders]) -> [field.1] =
                // lambda |[binders], f : [name]| f [field.1] (projection_to_i_factor)
                for (field_var, field_type) in &fields {
                    // we first construct projection to the i^th factor function:
                    let field_type = self.desugar_expr(&field_type);
                    let mut proj_i = Judg_ment::freevar(field_var.index);
                    for field in fields.iter().rev().clone() {
                        proj_i = Judg_ment::lam(None, proj_i.bind(field.0.index))
                    }

                    // now we do lambda |f : [name]| f [field.1] (proj_i):
                    let f_index = VarUuid::new();
                    let proj_i = Judg_ment::app(
                        Judg_ment::app(Judg_ment::freevar(f_index), field_type.clone()),
                        proj_i,
                    );
                    let mut proj_i = Judg_ment::lam(None, proj_i.bind(f_index));
                    // now we add the binders
                    if let Some(binders) = binders.clone() {
                        proj_i = self.binders_to_lam(binders, proj_i)
                    }

                    // now we can move on to the expected types: Pi |[binders]| ([name] [binders]) -> [field.1]
                    // we have done [name][binders] above.
                    let mut expected_type =
                        Judg_ment::fun(struct_apply_to_binders.clone(), field_type.clone());
                    // now we add on P |[binders]|
                    if let Some(binders) = binders.clone() {
                        let var_list = &binders.0;
                        for (var, var_type) in var_list.iter().rev() {
                            let var_type = var_type
                                .clone()
                                .map(|var_type| self.desugar_expr(&var_type));
                            expected_type = Judg_ment::pi(var_type, expected_type.bind(var.index));
                        }
                    }
                    let mod_ule_item = Mod_uleItem {
                        var: field_var.clone(),
                        impl_: proj_i,
                        expected_type: Some(expected_type),
                        backend: module_stmt.1.clone(),
                        with_list: with_var.clone(),
                    };
                    mod_ule_items.push(mod_ule_item)
                }

                // now we have an constructor,
                //[name]+Cons: Pi |[binders]| [first_field] -> [second_field] .. -> [name] [binders] =
                // lam|[binders], 1st_field, 2nd_field,..., T, f : [field1] -> [field2] .. -> T | f 1st_field 2nd_field...
                let var = Var {
                    index: VarUuid::new(),
                    name: var_binder.clone().name + "Cons",
                    span: new_span(),
                    local_or_global: LocalOrGlobal::Local,
                };

                // first we define the impl_:
                // lam|[binders], 1st_field, 2nd_field,..., T, f : [field1] -> [field2] .. -> T | f 1st_field 2nd_field...
                let f_index = VarUuid::new();
                let mut impl_ = Judg_ment::freevar(f_index);
                for field in &fields {
                    impl_ = Judg_ment::app(impl_, Judg_ment::freevar(field.0.index))
                }
                // now we add lambda|f|:
                impl_ = Judg_ment::lam(None, impl_.bind(f_index));
                // now we add lambda|T|:
                let t_index = VarUuid::new();
                impl_ = Judg_ment::lam(Some(Judg_ment::u()), impl_.bind(t_index));
                //now we add lambda|fields|
                for field in fields.iter().rev() {
                    impl_ =
                        Judg_ment::lam(Some(self.desugar_expr(&field.1)), impl_.bind(field.0.index))
                }
                // now we add in the binder:
                if let Some(binders) = binders.clone() {
                    let var_list = &binders.0;
                    for (var, var_type) in var_list.iter().rev() {
                        let var_type = var_type
                            .clone()
                            .map(|var_type| self.desugar_expr(&var_type));
                        impl_ = Judg_ment::lam(var_type, impl_.bind(var.index));
                    }
                }

                // now we can do the type:  Pi |[binders]| [first_field] -> [second_field] .. -> [name] [binders]
                // first we have [name] [binders]
                let mut expected_type = struct_apply_to_binders.clone();
                // now we add in all the arrows:
                for field in fields.iter().rev() {
                    expected_type = Judg_ment::fun(self.desugar_expr(&(field.1)), expected_type)
                }
                // now we add the Pi| [binders]|
                if let Some(binders) = binders.clone() {
                    let var_list = &binders.0;
                    for (var, var_type) in var_list.iter().rev() {
                        let var_type = var_type
                            .clone()
                            .map(|var_type| self.desugar_expr(&var_type));
                        expected_type = Judg_ment::pi(var_type, expected_type.bind(var.index));
                    }
                }

                let mod_ule_item = Mod_uleItem {
                    var: var,
                    impl_: impl_,
                    expected_type: Some(expected_type),
                    backend: module_stmt.1,
                    with_list: with_var.clone(),
                };

                mod_ule_items.push(mod_ule_item);
                mod_ule_items
            }
            StmtKind::Import(_, _) => {
                unreachable!("we handle imports differently")
            }
        }
    }
}

pub fn desugar_module_stmt(
    module_stmt: Stmt,
    resolve_var: BTreeMap<VarUuid, ResolvePrim>,
) -> Vec<Mod_uleItem> {
    let ctx = Context {
        prim_map: resolve_var,
    };
    ctx.desugar_module_stmt(module_stmt)
}

impl<T: Primitive, S: Metadata> std::fmt::Debug for Judg_ment<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.0 {
            Judg_mentKind::Type => f.write_str("Type")?,
            Judg_mentKind::Prim(prim) => f.write_str(&format!("{:?}", prim))?,
            Judg_mentKind::FreeVar(index) => f.write_str(&format!("v{:?}", index.index()))?,
            Judg_mentKind::BoundVar(index) => f.write_str(&format!("bv{:?}", index))?,

            Judg_mentKind::Pi(var_type, body) => {
                f.write_str("Pi |")?;
                let type_str = match var_type {
                    Some(var_type) => format!("{:?}", var_type),
                    None => "Unknown".into(),
                };
                f.write_str(&type_str)?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body.0))?;
            }
            Judg_mentKind::Lam(var_type, body) => {
                f.write_str("Lam |")?;
                let type_str = match var_type {
                    Some(var_type) => format!("{:?}", var_type),
                    None => "Unknown".into(),
                };
                f.write_str(&type_str)?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body.0))?;
            }
            Judg_mentKind::App(func, arg) => {
                f.write_str(&format!("App ({:?}) ", func))?;
                f.write_str(&format!("({:?})", arg))?;
            }
            Judg_mentKind::Let(arg, var_type, rest) => {
                f.write_str(&format!("Let {:?} = {:?};", var_type, arg))?;
                f.write_str(&format!("{:?}", rest.0))?;
            }
            Judg_mentKind::Bind(arg, func) => {
                f.write_str(&format!("(Bind {:?} {:?})", arg, func))?
            }
            Judg_mentKind::Pure(arg) => {
                f.write_str(&format!("(Pure {:?})", arg))?;
            }
        }
        Ok(())
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug)]
pub struct Mod_uleItem {
    pub var: Var,
    pub impl_: Judg_ment<UiPrim, UiMetadata>,
    pub expected_type: Option<Judg_ment<UiPrim, UiMetadata>>,
    pub backend: Option<String>,
    pub with_list: BTreeSet<VarUuid>,
}
