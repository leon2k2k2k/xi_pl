use crate::{
    resolve::{self, parse_source_file, Expr, ExprKind, ResolvePrim, Stmt, StmtKind},
    rowan_ast::string_to_syntax,
};
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
    Fun(Judg_ment, Judg_ment),
    Pi(Var, Judg_ment),
    Lam(Var, Judg_ment),
    App(Judg_ment, Judg_ment),
    Bind(Judg_ment, Judg_ment),
    StringLit(String),
    Iota(Judg_ment),
    Pure(Judg_ment),
}

fn desugar_stmt_vec(stmts: &[Stmt]) -> Judg_ment {
    if stmts.is_empty() {
        panic!("expected a val or something");
    }

    let stmt = &stmts[0];
    let result_kind = match &stmt.0 {
        StmtKind::Let(var, expr) => {
            let stmt_rest = &stmts[1..];
            if let ExprKind::Bang(expr1) = &*expr.0 {
                let bind_arg = desugar_expr(&expr1);
                let bind_fun = desugar_stmt_vec(stmt_rest);
                let bind_arg_span = bind_arg.1;
                let bind_fun_span = bind_fun.1;
                let rest_kind = Judg_mentKind::Lam(desugar_var(var), bind_fun);
                let rest = Judg_ment(Box::new(rest_kind), bind_fun_span);
                Judg_mentKind::Bind(bind_arg, rest)
            } else {
                let func_body = desugar_stmt_vec(stmt_rest);
                let func = Judg_mentKind::Lam(desugar_var(var), func_body.clone());
                Judg_mentKind::App(Judg_ment(Box::new(func), func_body.1), desugar_expr(&expr))
            }
        }
        StmtKind::Val(expr) => *desugar_expr(&expr).0,
        StmtKind::Import(_) => todo!(),
        StmtKind::Error(_) => todo!(),
    };

    let first_span = stmts[0].1;
    let last_span = stmts[stmts.len() - 1].1;
    Judg_ment(Box::new(result_kind), first_span.cover(last_span))
}

fn desugar_var(var: &resolve::Var) -> Var {
    Var {
        index: var.index,
        var_type: var
            .var_type
            .as_ref()
            .map(|var_type| desugar_expr(&var_type)),
        name: var.name.clone(),
        span: var.span,
    }
}

fn desugar_expr(expr: &Expr) -> Judg_ment {
    let result_kind: Judg_mentKind = match &*expr.0 {
        ExprKind::Var(var) => Judg_mentKind::Var(desugar_var(var)),
        ExprKind::Type => Judg_mentKind::Type,
        ExprKind::Bang(expr) => {
            Judg_mentKind::Pure(desugar_expr(expr)) // val x! => iopure(x)
        }
        ExprKind::App(func, elem) => Judg_mentKind::App(desugar_expr(&func), desugar_expr(&elem)),
        ExprKind::Fun(source, target) => {
            Judg_mentKind::Fun(desugar_expr(&source), desugar_expr(&target))
        }
        ExprKind::Lam(binders, lam_expr) => {
            let var_list = &binders.0;
            let expr = desugar_expr(&lam_expr);
            if var_list.len() == 1 {
                let var = desugar_var(&var_list[0]);
                Judg_mentKind::Lam(var, expr)
            } else {
                panic!("lam binder should only have one ident for now")
            }
        }
        ExprKind::Pi(binders, pi_expr) => {
            let var_list = &binders.0;
            let expr = desugar_expr(&pi_expr);
            if var_list.len() == 1 {
                let var = &var_list[0];
                let var = desugar_var(var);
                Judg_mentKind::Pi(var, expr)
            } else {
                panic!("lam binder should only have one ident for now")
            }
        }
        ExprKind::Stmt(stmt_vec) => *desugar_stmt_vec(&stmt_vec).0,
        ExprKind::Member(_, _) => todo!(),
        ExprKind::StringLit(_) => todo!(),
    };

    Judg_ment(Box::new(result_kind), expr.1)
}
pub fn text_to_judg_ment(text: &str) -> (Judg_ment, BTreeMap<VarUuid, ResolvePrim>) {
    let node = string_to_syntax(text);
    let source_file = parse_source_file(&node);
    let stmts = &source_file.0;
    (desugar_stmt_vec(stmts), source_file.1)
}

impl std::fmt::Debug for Judg_ment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self.0.clone() {
            Judg_mentKind::Type => f.write_str("Type")?,
            Judg_mentKind::Var(var) => f.write_str(&format!("{:?} {:?}", &var.name, &var.index))?,
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
                f.write_str(&var.name)?;
                f.write_str(&format!(": {:?}", var.var_type))?;
                f.write_str("| ")?;
                f.write_str(&format!("{:?}", body))?;
            }
            Judg_mentKind::App(func, arg) => {
                f.write_str(&format!("{:?}", func))?;
                f.write_str(&format!("{:?}", arg))?;
            }
            Judg_mentKind::Bind(input, rest) => {
                f.write_str(&format!("{:?}", input));
                f.write_str(">=");
                f.write_str(&format!("{:?}", rest));
            }
            Judg_mentKind::StringLit(_) => todo!(),
            Judg_mentKind::Iota(_) => todo!(),
            Judg_mentKind::Pure(expr) => f.write_str(&format!("{:?}", expr))?,
        }
        Ok(())
    }
}

mod test {
    use crate::{resolve::parse_source_file, rowan_ast::string_to_syntax};

    use super::{desugar_stmt_vec, Judg_ment};

    #[test]
    fn test_de_sugar() {
        use super::*;
        let text = "fn foo |x : Type| -> Type { val x}
        val foo
        ";
        let judg_ment = text_to_judg_ment(text);
        dbg!(judg_ment);

        let text2 = "fn foo |x| {val x} 
        val foo (Pi |y: Type| y)";
        let judg_ment2 = text_to_judg_ment(text2);
        dbg!(judg_ment2);
    }
}
