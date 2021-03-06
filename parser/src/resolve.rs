#[derive(Clone, Debug)]
struct SourceFile {
    stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
enum Stmt {
    Let(FreeVar, Option<Expr>, Expr),
    Do(Expr)
}

#[derive(Clone, Debug)]
enum Expr {
    Var(FreeVar),
    Type,
    StringLit()
    Bang(Expr),
    App(Expr, Expr),
    Fun(Expr, Expr),
    Lam(FreeVar, Binders, Expr),
    Pi(FreeVar, Binders, Expr),
    Stmt(Vec<Stmt>),

}