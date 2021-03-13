use crate::resolve::{self, Expr, ExprKind, Stmt, StmtKind};
use free_var::FreeVar as VarIndex;

pub use crate::resolve::Span;

#[derive(Clone, Debug)]
struct Var {
    index: VarIndex,
    var_type: Option<Judg_ment>,
    name: String,
    span: Span,
}

#[derive(Clone, Debug)]
struct Judg_ment(Box<Judg_mentKind>, Span);

#[derive(Clone, Debug)]
enum Judg_mentKind {
    Type,
    FreeVar(Var),
    Fun(Judg_ment, Judg_ment),
    Undefined,
    Pi(Var, Judg_ment),
    Lam(Var, Judg_ment),
    App(Judg_ment, Judg_ment),
    Bind(Judg_ment, Var, Judg_ment),
    StringLit(String),
    Iota(Judg_ment),
}

fn desugar_stmt_vec(stmt: Vec<Stmt>, span: Span) -> Judg_ment {
    let stmt_kind = stmt.0;
    let result_kind = match stmt_kind {
        StmtKind::Let(var, expr) => {}
        StmtKind::Val(expr) => {}
        StmtKind::Import(_) => todo!(),
        StmtKind::Error(_) => todo!(),
    };

    Judg_ment(result_kind, span)
}

fn desugar_var(var: resolve::Var) -> Var {
    Var {
        index: var.index,
        var_type: var.var_type.map(|var_type| desugar_expr(var_type)),
        name: var.name,
        span: var.span,
    }
}

fn desugar_expr(expr: Expr) -> Judg_ment {
    let result_kind: Judg_mentKind = match *expr.0 {
        ExprKind::Var(var) => Judg_mentKind::FreeVar(desugar_var(var)),
        ExprKind::Type => Judg_mentKind::Type,
        ExprKind::Bang(_) => {}
        ExprKind::App(_, _) => {}
        ExprKind::Fun(_, _) => {}
        ExprKind::Lam(_, _) => {}
        ExprKind::Pi(_, _) => {}
        ExprKind::Stmt(stmt_vec) => desugar_stmt_vec(stmt_vec, expr.1),
        ExprKind::Member(_, _) => todo!(),
        ExprKind::StringLit(_) => {}
    };

    Judg_ment(Box::new(result_kind), expr.1)
}
