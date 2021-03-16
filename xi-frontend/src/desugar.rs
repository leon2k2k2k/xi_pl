use crate::{
    resolve::{self, parse_source_file, Expr, ExprKind, Stmt, StmtKind},
    syntax_tree::string_to_syntax,
};
use xi_uuid::VarUuid;

pub use crate::resolve::Span;

#[derive(Clone, Debug)]
pub struct Var {
    pub index: VarUuid,
    pub var_type: Option<Judg_ment>,
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Judg_ment(Box<Judg_mentKind>, Span);

#[derive(Clone, Debug)]
pub enum Judg_mentKind {
    None, // the type of Type
    Type,
    VarUuid(Var),
    Fun(Judg_ment, Judg_ment),
    Pi(Var, Judg_ment),
    Lam(Var, Judg_ment),
    App(Judg_ment, Judg_ment),
    Bind(Judg_ment, Var, Judg_ment),
    IdBind(Judg_ment, Var, Judg_ment),
    StringLit(String),
    Iota(Judg_ment),
    TypeVarUuid(Var), // Type variables, used in type_inference.rs,
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
                Judg_mentKind::Bind(
                    Judg_ment(Box::new(Judg_mentKind::Iota(bind_arg)), bind_arg_span),
                    desugar_var(var),
                    Judg_ment(Box::new(Judg_mentKind::Iota(bind_fun)), bind_fun_span),
                )
            } else {
                Judg_mentKind::Bind(
                    desugar_expr(&expr),
                    desugar_var(var),
                    desugar_stmt_vec(stmt_rest),
                )
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
        ExprKind::Var(var) => Judg_mentKind::VarUuid(desugar_var(var)),
        ExprKind::Type => Judg_mentKind::Type,
        ExprKind::Bang(expr) => {
            todo!("don't bang please");
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
                Judg_mentKind::Lam(var, expr)
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
pub fn text_to_judg_ment(text: &str) -> Judg_ment {
    let node = string_to_syntax(text);
    let source_file = parse_source_file(&node);
    let stmts = &source_file.0;
    desugar_stmt_vec(stmts)
}
mod test {
    use crate::{resolve::parse_source_file, syntax_tree::string_to_syntax};

    use super::{desugar_stmt_vec, Judg_ment};

    #[test]
    fn test_de_sugar() {
        use super::*;
        let text = "fn foo |x : Type| -> Type {let y = x! val y}
        val foo
        ";
        let judg_ment = text_to_judg_ment(text);
        dbg!(judg_ment);
    }
}
