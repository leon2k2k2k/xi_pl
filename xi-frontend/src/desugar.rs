use crate::{
    resolve::{self, parse_source_file, Expr, ExprKind, ResolvePrim, Stmt, StmtKind},
    rowan_ast::string_to_syntax,
};
use resolve::StringTokenKind;
use std::collections::BTreeMap;
use xi_uuid::VarUuid;

pub use crate::resolve::Span;

#[derive(Clone, Debug)]
pub struct Var {
    pub index: VarUuid,
    pub var_type: Option<Judg_ment>,
    pub name: String,
    pub span: Span,
}

#[derive(Clone)]
pub struct Judg_ment(pub Box<Judg_mentKind>, pub Span);

#[derive(Clone)]
pub enum Judg_mentKind {
    Type,
    Var(Var),
    Ffi(String, String),
    Fun(Judg_ment, Judg_ment),
    Pi(Var, Judg_ment),
    Lam(Var, Judg_ment),
    App(Judg_ment, Judg_ment),
    Bind(Judg_ment, Judg_ment),
    StringLit(String),
    Pure(Judg_ment),
    Prim(ResolvePrim),
}

struct Context {
    prim_map: BTreeMap<VarUuid, ResolvePrim>,
}

impl Context {
    fn desugar_stmt_vec(&self, stmts: &[Stmt]) -> Judg_ment {
        if stmts.is_empty() {
            panic!("expected a val or something");
        }

        let stmt = &stmts[0];
        let stmt_rest = &stmts[1..];

        let result_kind = match &stmt.0 {
            StmtKind::Let(var, expr) => {
                if let ExprKind::Bang(expr1) = &*expr.0 {
                    let bind_arg = self.desugar_expr(&expr1);
                    let bind_fun = self.desugar_stmt_vec(stmt_rest);

                    let bind_fun_span = bind_fun.1;

                    let rest_kind = Judg_mentKind::Lam(self.desugar_var(var), bind_fun);
                    let rest = Judg_ment(Box::new(rest_kind), bind_fun_span);
                    Judg_mentKind::Bind(bind_arg, rest)
                } else {
                    let rest_body = self.desugar_stmt_vec(stmt_rest);
                    let rest = Judg_mentKind::Lam(self.desugar_var(var), rest_body.clone());
                    Judg_mentKind::App(
                        Judg_ment(Box::new(rest), rest_body.1),
                        self.desugar_expr(&expr),
                    )
                }
            }
            StmtKind::Val(expr) => *self.desugar_expr(&expr).0,
            StmtKind::Ffi(file_name, vars) => {
                let rest = self.desugar_stmt_vec(stmt_rest);

                let mut result = rest;

                let mut vars = vars.clone();
                
                vars.reverse();

                for var in vars {
                    let result_span = result.1;
                    let func_kind = Judg_mentKind::Lam(self.desugar_var(&var), result);
                    let func = Judg_ment(Box::new(func_kind), result_span);
                    let ffi_kind = Judg_mentKind::Ffi(file_name.clone(), var.name.clone());
                    let ffi = Judg_ment(Box::new(ffi_kind), var.span);
                    
                    let result_kind = Judg_mentKind::App(func, ffi);
                    result = Judg_ment(Box::new(result_kind), result_span);
                }
                *result.0
            }
            // StmtKind::Import(_) => todo!(),
            // StmtKind::Error(_) => todo!(),
        };

        let first_span = stmts[0].1;
        let last_span = stmts[stmts.len() - 1].1;
        Judg_ment(Box::new(result_kind), first_span.cover(last_span))
    }

    fn desugar_var(&self, var: &resolve::Var) -> Var {
        Var {
            index: var.index,
            var_type: var
                .var_type
                .as_ref()
                .map(|var_type| self.desugar_expr(&var_type)),
            name: var.name.clone(),
            span: var.span,
        }
    }

    fn desugar_expr(&self, expr: &Expr) -> Judg_ment {
        let result_kind: Judg_mentKind = match &*expr.0 {
            ExprKind::Var(var) => match self.prim_map.get(&var.index) {
                Some(prim) => Judg_mentKind::Prim(prim.clone()),
                None => Judg_mentKind::Var(self.desugar_var(var)),
            },
            ExprKind::Type => Judg_mentKind::Type,
            ExprKind::Bang(expr) => {
                Judg_mentKind::Pure(self.desugar_expr(expr)) // val x! => iopure(x)
            }
            ExprKind::App(func, elem) => {
                Judg_mentKind::App(self.desugar_expr(&func), self.desugar_expr(&elem))
            }
            ExprKind::Fun(source, target) => {
                Judg_mentKind::Fun(self.desugar_expr(&source), self.desugar_expr(&target))
            }
            ExprKind::Lam(binders, lam_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&lam_expr);
                if var_list.len() == 1 {
                    let var = self.desugar_var(&var_list[0]);
                    Judg_mentKind::Lam(var, expr)
                } else {
                    panic!("lam binder should only have one ident for now")
                }
            }
            ExprKind::Pi(binders, pi_expr) => {
                let var_list = &binders.0;
                let expr = self.desugar_expr(&pi_expr);
                if var_list.len() == 1 {
                    let var = &var_list[0];
                    let var = self.desugar_var(var);
                    Judg_mentKind::Pi(var, expr)
                } else {
                    panic!("lam binder should only have one ident for now")
                }
            }
            ExprKind::Stmt(stmt_vec) => *self.desugar_stmt_vec(&stmt_vec).0,
            ExprKind::Member(_, _) => todo!(),
            ExprKind::StringLit(string_components) => {
                if string_components.len() > 1 {
                    panic!("we don't support string escapes yet :)");
                }

                match string_components[0].0.clone() {
                    StringTokenKind::Escape(_) => {
                        panic!("we don't support string escapes yet :)");
                    }
                    StringTokenKind::String(string) => Judg_mentKind::StringLit(string),
                }
            }
        };

        Judg_ment(Box::new(result_kind), expr.1)
    }
}
pub fn text_to_judg_ment(text: &str) -> Judg_ment {
    let node = string_to_syntax(text);
    let source_file = parse_source_file(&node);
    let stmts = &source_file.0;
    let ctx = Context {
        prim_map: source_file.1,
    };

    ctx.desugar_stmt_vec(stmts)
}

impl std::fmt::Debug for Judg_ment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self.0.clone() {
            Judg_mentKind::Type => f.write_str("Type")?,
            Judg_mentKind::Var(var) => f.write_str(&format!("({:?} : {:?})", &var.name, &var.index))?,
            Judg_mentKind::Fun(func, arg) => {
                f.write_str(&format!("{:?}", func))?;
                f.write_str(" -> ")?;
                f.write_str(&format!("{:?}", arg))?;
            }
            Judg_mentKind::Pi(var, body) => {
                f.write_str("Pi |")?;
                f.write_str(&var.name)?;
                f.write_str(&format!(": {:?}", var.var_type))?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body))?;
            }
            Judg_mentKind::Lam(var, body) => {
                f.write_str("Lam |")?;
                f.write_str(&format!("{} {:?}", &var.name, &var.index))?;
                f.write_str(&format!(": {:?}", var.var_type))?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body))?;
            }
            Judg_mentKind::App(func, arg) => {
                f.write_str(&format!("({:?}) ", func))?;
                f.write_str(&format!("({:?})", arg))?;
            }
            Judg_mentKind::Bind(input, rest) => {
                f.write_str(&format!("{:?}", input))?;
                f.write_str(">=")?;
                f.write_str(&format!("{:?}", rest))?;
            }
            Judg_mentKind::StringLit(str) => f.write_str(&format!("\"{}\"", str))?,
            Judg_mentKind::Pure(expr) => f.write_str(&format!("{:?}", expr))?,
            Judg_mentKind::Prim(prim) => f.write_str(&format!("{:?}", prim))?,
            Judg_mentKind::Ffi(filename, func_name) => {
                f.write_str(&format!("Ffi({}:{})", filename, func_name))?
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

        let judg_ment4 = text_to_judg_ment(ffi_text);
        dbg!(judg_ment4);
    }
}
