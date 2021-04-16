use crate::{ Module, resolve::{self, Expr, ExprKind, ModuleStmt, Stmt, StmtKind}};
use resolve::StringTokenKind;
use xi_core::judgment::{Judgment,  Metadata, Primitive};
use xi_uuid::VarUuid;
use crate::type_inference::UiPrim;
use crate::type_inference::UiMetadata;

pub use crate::resolve::Span;

// #[derive(Clone, Debug)]
// pub struct VarBinder {
//     pub var_type: Option<Judg_ment>,
//     pub name: String,
//     pub span : Span,
// }

// #[derive(Clone, Debug)]
// pub struct BoundVar {
//     pub index: u32,
//     pub name: String,
//     pub span : Span,
// }

// #[derive(Clone, Debug)]
// pub struct FreeVar {
//     pub index: VarUuid,
//     pub name: String,
//     pub span : Span,
// }

///////// Note that no freevars are getting binded in desugar.
#[derive(Clone)]
pub struct Judg_ment<T, S>(pub Box<Judg_mentKind<T, S>>, pub S);

#[derive(Clone)]
pub enum Judg_mentKind<T, S> {
    Type,
    Prim(T),
    FreeVar(VarUuid),
    Pi(VarUuid, Option<Judg_ment<T, S>>, Judg_ment<T, S>),
    Lam(VarUuid, Option<Judg_ment<T, S>>, Judg_ment<T, S>),
    App(Judg_ment<T, S>, Judg_ment<T, S>),
    Let(Judg_ment<T, S>, VarUuid, Option<Judg_ment<T, S>>, Judg_ment<T, S>),
    Bind(Judg_ment<T, S>, Judg_ment<T, S>),
    Pure(Judg_ment<T, S>),
    Original(Judgment<T,S>)
}


impl Judg_ment<UiPrim, UiMetadata> {
    fn u() -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Type), UiMetadata{})
    }

    fn prim(prim : UiPrim) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Prim(prim)), UiMetadata{})
    }
    
    fn app(func: Judg_ment<UiPrim, UiMetadata>, arg: Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::App(func, arg)), UiMetadata{})
    }

    fn bind(ma : Judg_ment<UiPrim, UiMetadata>, a_to_mb : Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Bind(ma, a_to_mb)), UiMetadata{})
    }

    fn pure(a : Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Pure(a)), UiMetadata{})
    }

    pub fn lam(var : VarUuid, var_type: Option<Judg_ment<UiPrim, UiMetadata>>, expr: Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Lam(var, var_type, expr)), UiMetadata{})
    }

    fn pi(var : VarUuid, var_type: Option<Judg_ment<UiPrim, UiMetadata>>, expr: Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Pi(var, var_type, expr)), UiMetadata{})
    }

    fn let_(arg: Judg_ment<UiPrim, UiMetadata>, var : VarUuid, var_type: Option<Judg_ment<UiPrim, UiMetadata>>, rest: Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Let(arg, var, var_type, rest)), UiMetadata{})
    }

}

struct Context {
    // todo: maybe there should be something here?
}

impl Context {
    // this desugars the body of a stmt_expr
    fn desugar_stmt_vec(&self, stmts: &[Stmt]) -> Judg_ment<UiPrim, UiMetadata>  {
        if stmts.is_empty() {
            panic!("expected a val or something");
        }

        let stmt = &stmts[0];
        let stmt_rest = &stmts[1..];

        match &stmt.0 {
            StmtKind::Let(var, expr) => {
                if let ExprKind::Bang(expr1) = &*expr.0 {
                    let bind_arg = self.desugar_expr(&expr1);
                    let rest = self.desugar_stmt_vec(stmt_rest);


                    let rest_kind = Judg_ment::lam(var.index, var.var_type.clone().map(|var_type| self.desugar_expr(&var_type)), rest);
                    Judg_ment::bind(bind_arg, rest_kind)
                } else {
                    let arg = self.desugar_expr(expr);
                    let rest = self.desugar_stmt_vec(stmt_rest);
                    let index = var.index;
                    let var_type =  var.var_type.clone().map(|var_type| self.desugar_expr(&var_type));
                    Judg_ment::let_(arg, index, var_type, rest)
                    // let rest_kind = Judg_ment::lam(var.index, var.var_type.clone().map(|var_type| self.desugar_expr(&var_type)), rest);

                    // Judg_ment::app(
                    //     rest_kind, arg
                    // )

                }
            }
            StmtKind::Val(expr) => self.desugar_expr(&expr),
            StmtKind::Ffi(file_name, vars) => {
                let rest = self.desugar_stmt_vec(stmt_rest);

                let mut result = rest;

                let mut vars = vars.clone();
                
                vars.reverse();

                for var in vars {
                    // let result_span = result.1;
                    let func = Judg_ment::lam(var.index, Some(self.desugar_expr(&var.var_type.unwrap())), result);
                    // let func = Judg_ment(Box::new(func_kind), UiMetadata{});
                    // let ffi_kind = Judg_mentKind::Ffi(file_name.clone(), var.name.clone());
                    // let ffi = Judg_ment(Box::new(ffi_kind), UiMetadata{});
                    let ffi = Judg_ment::prim(UiPrim::Ffi(file_name.clone(), var.name.clone()));
                    
                    // let result_kind = Judg_mentKind::App(func, ffi);
                    // result = Judg_ment(Box::new(result_kind), result_span);
                    result = Judg_ment::app(func, ffi)
                }
                result
            }
            // StmtKind::Import(_) => todo!(),
            // StmtKind::Error(_) => todo!(),
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
        match var.local_or_global {
            resolve::LocalOrGlobal::Local => Judg_ment(Box::new(Judg_mentKind::FreeVar(var.index)), UiMetadata{})  ,
            resolve::LocalOrGlobal::Global => Judg_ment::prim(UiPrim::Global(var.index)),
        }
    }

    fn desugar_expr(&self, expr: &Expr) -> Judg_ment<UiPrim, UiMetadata> {
        match &*expr.0 {
            ExprKind::Var(var) => self.desugar_var(var),
            ExprKind::Type => Judg_ment::u(),
            ExprKind::Bang(expr) => {
                Judg_ment::pure(self.desugar_expr(expr))
            }
            ExprKind::App(func, elem) => {
                Judg_ment::app(self.desugar_expr(&func), self.desugar_expr(&elem))
            }
            ExprKind::Fun(source, target) => {
                let var = VarUuid::new();
                Judg_ment::pi(var, Some(self.desugar_expr(&source)), self.desugar_expr(&target))
            }
            ExprKind::Lam(binders, lam_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&lam_expr);

                let mut result = expr;

                for var in var_list.iter().rev() {
                    let var_type = var.var_type.clone().map(|var_type| self.desugar_expr(&var_type));
                    result = Judg_ment::lam(var.index, var_type, result);
                }
                result

            }
            ExprKind::Pi(binders, pi_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&pi_expr);

                let mut result = expr;

                for var in var_list.iter().rev() {
                    let var_type = var.var_type.clone().map(|var_type| self.desugar_expr(&var_type));
                    result = Judg_ment::pi(var.index, var_type, result);
                }
                result
            }
            ExprKind::Stmt(stmt_vec) => self.desugar_stmt_vec(&stmt_vec),
            ExprKind::Member(_, _) => todo!(),
            ExprKind::StringLit(string_components) => {
                if string_components.len() > 1 {
                    panic!("we don't support string escapes yet :)");
                }

                match string_components[0].0.clone() {
                    StringTokenKind::Escape(_) => {
                        panic!("we don't support string escapes yet :)");
                    }
                    StringTokenKind::String(string) => Judg_ment::prim(UiPrim::StringElem(string)),
                }
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let judg_ment_op = Judg_ment::prim(UiPrim::Binary(op.clone()));
                Judg_ment::app(Judg_ment::app(judg_ment_op, self.desugar_expr(lhs)), self.desugar_expr(rhs))
            }
            ExprKind::Number(num) => {
                Judg_ment::prim(UiPrim::NumberElem(num.clone()))            
            }
        }
    }
}

impl <T : Primitive, S : Metadata >std::fmt::Debug for Judg_ment<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.0 {
            Judg_mentKind::Type => f.write_str("Type")?,
            Judg_mentKind::Prim(prim) => f.write_str(&format!("{:?}", prim))?,
            Judg_mentKind::FreeVar(index) => f.write_str(&format!("v{:?}", index.index()))?,
            Judg_mentKind::Pi(index, var_type, body) => {
                f.write_str("Pi |")?;
                f.write_str(&format!("v{:?} : ", index.index()))?;
                let type_str = match var_type {
                    Some(var_type) => format!("{:?}", var_type),
                    None => "Unknown".into(),
                };
                f.write_str(&type_str)?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body))?;
            }
            Judg_mentKind::Lam(index, var_type, body) => {
                f.write_str("Lam |")?;
                f.write_str(&format!("v{} : ", index.index()))?;
                let type_str = match var_type {
                    Some(var_type) => format!("{:?}", var_type),
                    None => "Unknown".into(),
                };
                f.write_str(&type_str)?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body))?;
            }
            Judg_mentKind::App(func, arg) => {
                f.write_str(&format!("App ({:?}) ", func))?;
                f.write_str(&format!("({:?})", arg))?;
            }
            Judg_mentKind::Let(arg, index, var_type, rest) => {
                f.write_str(&format!("Let {} : {:?} = {:?};", index.index(), var_type, arg))?;
                f.write_str(&format!("{:?}", rest))?;
            }
            Judg_mentKind::Bind(arg, func) => {
                f.write_str(&format!("(Bind {:?} {:?})", arg, func))?}
            Judg_mentKind::Pure(arg) => {
                f.write_str(&format!("(Pure {:?})", arg))?;
            }
            Judg_mentKind::Original(judgment) => {
                f.write_str(&format!("Original {:?}", judgment))?;
            }
        }
        Ok(())
    }
}


pub fn desugar_module_stmt(_module: &mut Module, module_stmt : ModuleStmt) -> Mod_uleItem {
    match module_stmt.impl_.0 {
        StmtKind::Let(var_bind, expr) => {
            let ctx = Context{};
            let judg_ment = ctx.desugar_expr(&expr);
            let expected_type = match var_bind.var_type{
                Some(var_type) => {Some(ctx.desugar_expr(&var_type))},
                None => None,
            };

            Mod_uleItem{
                impl_: judg_ment,
                expected_type: expected_type,
            }
        }
        StmtKind::Val(_) => unreachable!(),
        StmtKind::Ffi(_, _) => unreachable!(),
    }
}
pub struct Mod_uleItem {
    pub impl_: Judg_ment<UiPrim, UiMetadata>,
    pub expected_type: Option<Judg_ment<UiPrim, UiMetadata>>,
}
// mod test {
//     #[test]
//     fn test_de_sugar() {
//         pub use super::*;
//         use crate::rowan_ast::{string_to_syntax};
//         use crate::type_inference::{ UiMetadata, UiPrim};
//         fn text_to_judg_ment(text : &str) -> Judg_ment<UiPrim, UiMetadata>{
//             let syntax_node = string_to_syntax(text);
//             // dbg!(&syntax_node);
//             // syntax_node is the rowan tree level
//             let source_file = parse_source_file(&syntax_node);
//             // dbg!(&source_file);
//             // source_file is at the name resolution level
//             let judg_ment = source_file_to_judg_ment(source_file);
//             // judg_ment is at the desugar level.
//             judg_ment
//         }
//         // let text = "fn foo |x : Type| -> Type { val x}
//         // val foo
//         // ";
//         // let judg_ment = text_to_judg_ment(text);
//         // dbg!(judg_ment);

//         let text2 = "fn foo |x| {val x}
//         val foo (Pi |y: Type| y)";
//         let judg_ment2 = text_to_judg_ment(text2);
//         dbg!(judg_ment2);

//         // let text3 = "let in = console_input!
//         // let y = console_output(in)!
//         // val unit!";
//         // let judg_ment3 = text_to_judg_ment(text3);
//         // dbg!(judg_ment3);

//         // let ffi_text = "ffi \"some_file.js\"{
//         //     Int : Type,
//         //     five : Int,
//         //     six : Int,
//         //     add : Int -> Int -> Int,
//         //     int_to_string : Int -> String 
//         // }
        
//         // let ans = add five six
//         // let better_ans = add ans six
//         // let even_better_ans = int_to_string(better_ans)
        
//         // do console_output(even_better_ans)!
//         // val unit!";

//         // let judg_ment = frontend(ffi_text);
//         // dbg!(judg_ment);
//     }
// }
