use crate::{resolve::{self, Expr, ExprKind, ResolvePrim, SourceFile, Stmt, StmtKind}};
use resolve::StringTokenKind;
use std::collections::BTreeMap;
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


#[derive(Clone)]
pub struct Judg_ment<T, S>(pub Box<Judg_mentKind<T, S>>, pub S);

#[derive(Clone)]
pub enum Judg_mentKind<T, S> {
    Type,
    Prim(T),
    BoundVar(u32),
    FreeVar(VarUuid),
    Pi(Option<Judg_ment<T, S>>, Judg_ment<T, S>),
    Lam(Option<Judg_ment<T, S>>, Judg_ment<T, S>),
    App(Judg_ment<T, S>, Judg_ment<T, S>),
    Bind(Judg_ment<T, S>, Judg_ment<T, S>),
    Pure(Judg_ment<T, S>),
}

impl <T, S>Judg_ment<T, S> {
    fn rebind(expr: Judg_ment<T, S>, free_var : VarUuid) -> Judg_ment<T, S> {
        fn rebind_rec<T, S>(expr : Judg_ment<T, S>,  free_var: VarUuid, depth : u32) -> Judg_ment<T, S>{
            use Judg_mentKind::*;
            let result_tree = match *expr.0 {
                Type => Type,
                BoundVar(var) => BoundVar(var),
                FreeVar(index) => if index == free_var {
                    BoundVar(depth)
                } else {
                    FreeVar(index)
                },
                Pi(var_type, body) => {
                    Pi(var_type.map(|var_type| rebind_rec(var_type, free_var, depth + 1)), rebind_rec(body, free_var, depth + 1))
                }
                Lam(var_type, body) => {
                    Lam(var_type.map(|var_type| rebind_rec(var_type, free_var, depth + 1)), rebind_rec(body, free_var, depth + 1))
                }
                App(lhs, rhs) => {
                    App(rebind_rec(lhs, free_var, depth), rebind_rec(rhs, free_var, depth))
                }
                Bind(lhs, rhs) => {
                    Bind(rebind_rec(lhs, free_var, depth), rebind_rec(rhs, free_var, depth))
                }
                Pure(arg) => {
                    Pure(rebind_rec(arg, free_var, depth))
                }
                Prim(prim) => {Prim(prim)}
            };
        Judg_ment(Box::new(result_tree), expr.1)
        }
        rebind_rec(expr, free_var, 0)
    }
    
}

impl Judg_ment<UiPrim, UiMetadata> {
    fn u() -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Type), UiMetadata{})
    }

    fn prim(prim : UiPrim) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Prim(prim)), UiMetadata{})
    }
    
    fn app(lhs : Judg_ment<UiPrim, UiMetadata>, rhs : Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::App(lhs, rhs)), UiMetadata{})
    }

    fn bind(lhs : Judg_ment<UiPrim, UiMetadata>, rhs : Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Bind(lhs, rhs)), UiMetadata{})
    }

    fn lam(lhs : Option<Judg_ment<UiPrim, UiMetadata>>, rhs : Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Lam(lhs, rhs)), UiMetadata{})
    }

    fn pi(lhs : Option<Judg_ment<UiPrim, UiMetadata>>, rhs : Judg_ment<UiPrim, UiMetadata>) -> Judg_ment<UiPrim, UiMetadata> {
        Judg_ment(Box::new(Judg_mentKind::Pi(lhs, rhs)), UiMetadata{})
    }
}

struct Context {
    prim_map: BTreeMap<VarUuid, ResolvePrim>,
}

impl Context {
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
                    let rest = Judg_ment::rebind(self.desugar_stmt_vec(stmt_rest), var.index);


                    let rest_kind = Judg_ment::lam(var.var_type.clone().map(|var_type| self.desugar_expr(&var_type)), rest);
                    Judg_ment::bind(bind_arg, rest_kind)
                } else {
                    let arg = self.desugar_expr(expr);
                    let rest = Judg_ment::rebind(self.desugar_stmt_vec(stmt_rest), var.index);
                    let rest_kind = Judg_ment::lam(var.var_type.clone().map(|var_type| self.desugar_expr(&var_type)), rest);

                    Judg_ment::app(
                        rest_kind, arg
                    )
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
                    let func = Judg_ment::lam(Some(self.desugar_expr(&var.var_type.unwrap())), Judg_ment::rebind(result, var.index));
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
        Judg_ment(Box::new(Judg_mentKind::FreeVar(var.index)), UiMetadata{})
    }

    fn desugar_expr(&self, expr: &Expr) -> Judg_ment<UiPrim, UiMetadata> {
        match &*expr.0 {
            ExprKind::Var(var) => match self.prim_map.get(&var.index) {
                Some(prim) => {
                    let prim = match prim {
                        ResolvePrim::IOMonad => UiPrim::IOMonad,
                        ResolvePrim::String => UiPrim::StringType,
                        ResolvePrim::Int => UiPrim::NumberType,
                    };
                    Judg_ment::prim(prim.clone())}
                None => self.desugar_var(var),
            },
            ExprKind::Type => Judg_ment::u(),
            ExprKind::Bang(expr) => { 
                Judg_ment::app(Judg_ment::prim(UiPrim::IOPure) ,self.desugar_expr(expr)) // val x! => iopure(x)
            }
            ExprKind::App(func, elem) => {
                Judg_ment::app(self.desugar_expr(&func), self.desugar_expr(&elem))
            }
            ExprKind::Fun(source, target) => {
                Judg_ment::pi(Some(self.desugar_expr(&source)), self.desugar_expr(&target))
            }
            ExprKind::Lam(binders, lam_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&lam_expr);

                let mut result = expr;

                for var in var_list.iter().rev() {
                    let var_type = var.var_type.clone().map(|var_type| self.desugar_expr(&var_type));
                    result = Judg_ment::lam(var_type, Judg_ment::rebind(result, var.index));
                }
                result

            }
            ExprKind::Pi(binders, pi_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&pi_expr);

                let mut result = expr;

                for var in var_list.iter().rev() {
                    let var_type = var.var_type.clone().map(|var_type| self.desugar_expr(&var_type));
                    result = Judg_ment::pi(var_type, Judg_ment::rebind(result, var.index));
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

pub fn source_file_to_judg_ment(source_file: SourceFile) -> Judg_ment<UiPrim, UiMetadata> {

    let stmts = &source_file.0;
    let ctx = Context {
        prim_map: source_file.1,
    };

    ctx.desugar_stmt_vec(stmts)
}

impl <T : std::fmt::Debug + Clone, S : std::fmt::Debug + Clone>std::fmt::Debug for Judg_ment<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.0 {
            Judg_mentKind::Type => f.write_str("Type")?,
            Judg_mentKind::Prim(prim) => f.write_str(&format!("{:?}", prim))?,
            Judg_mentKind::BoundVar(var) => f.write_str(&format!("bv{:?}", var))?,
            Judg_mentKind::FreeVar(var) => f.write_str(&format!("fv{:?}", var))?,
            Judg_mentKind::Pi(var_type, body) => {
                f.write_str("Pi |")?;
                // f.write_str()?;
                f.write_str(&format!("{:?}", var_type))?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body))?;
            }
            Judg_mentKind::Lam(var_type, body) => {
                f.write_str("Lam |")?;
                // f.write_str( &var.name)?;
                f.write_str(&format!("{:?}", var_type))?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body))?;
            }
            Judg_mentKind::App(func, arg) => {
                f.write_str(&format!("App ({:?}) ", func))?;
                f.write_str(&format!("({:?})", arg))?;
            }
            Judg_mentKind::Bind(arg, func) => {
                f.write_str(&format!("(Bind {:?} {:?})", arg, func))?}
            Judg_mentKind::Pure(arg) => {
                f.write_str(&format!("(Pure {:?})", arg))?;
            }
        }
        Ok(())
    }
}

mod test {
    #[test]
    fn test_de_sugar() {
        use super::*;
        // let text = "fn foo |x : Type| -> Type { val x}
        // val foo
        // ";
        // let judg_ment = text_to_judg_ment(text);
        // dbg!(judg_ment);

        // let text2 = "fn foo |x| {val x}
        // val foo (Pi |y: Type| y)";
        // let judg_ment2 = text_to_judg_ment(text2);
        // dbg!(judg_ment2);

        // let text3 = "let in = console_input!
        // let y = console_output(in)!
        // val unit!";
        // let judg_ment3 = text_to_judg_ment(text3);
        // dbg!(judg_ment3);

        let ffi_text = "ffi \"some_file.js\"{
            Int : Type,
            five : Int,
            six : Int,
            add : Int -> Int -> Int,
            int_to_string : Int -> String 
        }
        
        let ans = add five six
        let better_ans = add ans six
        let even_better_ans = int_to_string(better_ans)
        
        do console_output(even_better_ans)!
        val unit!";

        // let judg_ment = frontend(ffi_text);
        // dbg!(judg_ment);
    }
}
