use crate::rowan_ast::{nonextra_children, SyntaxKind, SyntaxNode};
use rowan::{TextRange, TextSize};
use std::collections::BTreeMap;
use xi_uuid::VarUuid;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ResolvePrim {
    IOMonad,
    StringType,
    UnitType,
    UnitElem,
    ConsoleInput,
    ConsoleOutput,
}

impl std::fmt::Display for ResolvePrim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResolvePrim::*;
        let res = match self {
            IOMonad => "IO",
            StringType => "String",
            UnitType => "Unit",
            UnitElem => "unit",
            ConsoleInput => "console_input",
            ConsoleOutput => "console_output",
        };
        write!(f, "{}", res)
    }
}

impl ResolvePrim {
    pub fn prims() -> Vec<ResolvePrim> {
        use ResolvePrim::*;
        vec![
            IOMonad,
            StringType,
            UnitType,
            UnitElem,
            ConsoleInput,
            ConsoleOutput,
        ]
    }
    fn get_ctx() -> (Context, BTreeMap<VarUuid, ResolvePrim>) {
        let mut ident_map = BTreeMap::new();
        let mut resolve_map = BTreeMap::new();
        for prim in ResolvePrim::prims() {
            let var = Var {
                index: VarUuid::new(),
                name: prim.to_string(),
                span: Span::new(TextSize::from(0), TextSize::from(0)),
                var_type: None,
            };

            ident_map.insert(prim.to_string(), var.clone());
            resolve_map.insert(var.index, prim.clone());
        }

        (ident_map, resolve_map)
    }
}
#[derive(Clone, Debug)]
pub struct SourceFile(pub Vec<Stmt>, pub BTreeMap<VarUuid, ResolvePrim>);
#[derive(Clone, Debug)]
pub enum Error {
    // Stmt(StmtError),
// Expr(ExprError),
// Ident(IdentError),
// Binder(BinderError),
}

#[derive(Clone, Debug)]
pub struct Stmt(pub StmtKind, pub Span);

#[derive(Clone, Debug)]
pub enum StmtKind {
    Let(Var, Expr),
    // Do(Expr),
    Val(Expr),
    // Fn(Ident, Binders, Option<Expr>, Expr), // Expr should be a stmt_expr.
    // Import(Var),
    // Error(StmtError),
}
// #[derive(Clone, Debug)]
// pub struct StmtError(StmtErrorKind, Span);
// #[derive(Clone, Debug)]
// pub enum StmtErrorKind {
//     InvalidStmt,
//     FnStmt,
//     DoStmt,
//     LetStmt,
//     ImportStmt,
// }

#[derive(Clone, Debug)]
pub struct Expr(pub Box<ExprKind>, pub Span);
#[derive(Clone, Debug)]
pub enum ExprKind {
    Var(Var),
    Type,
    Bang(Expr),
    App(Expr, Expr),
    Fun(Expr, Expr),
    Lam(Binders, Expr),
    Pi(Binders, Expr),
    Stmt(Vec<Stmt>),
    Member(Expr, Expr),
    StringLit(Vec<StringToken>),
}

#[derive(Clone, Debug)]
pub struct StringToken(StringTokenKind, Span);

#[derive(Clone, Debug)]
pub enum StringTokenKind {
    String(String),
    Escape(String),
}

#[derive(Clone, Debug)]
pub struct Binders(pub Vec<Var>, pub Span);
#[derive(Clone, Debug)]
pub struct Var {
    pub index: VarUuid,
    pub var_type: Option<Expr>,
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct NameNotFoundError {
    name: String,
    span: Span,
}

pub type Span = rowan::TextRange;

type Context = BTreeMap<String, Var>;

pub fn parse_source_file(node: &SyntaxNode) -> SourceFile {
    let (mut ctx, resolve_var) = ResolvePrim::get_ctx();
    let mut stmts = vec![];
    // let mut errors = vec![];
    // let mut previous_error = false;

    for child in nonextra_children(node) {
        let child_stmt = parse_stmt(&child, &mut ctx);

        stmts.push(child_stmt);
        // if let SyntaxKind::ERROR = child.kind() {
        //     if previous_error == false {
        //         previous_error = true;
        //         errors.push(child_errors)
        //     }
        // }
    }
    SourceFile(stmts, resolve_var)
}

fn create_var(node: &SyntaxNode, var_type: Option<Expr>, ctx: &mut Context) -> Var {
    assert_eq!(node.kind(), SyntaxKind::IDENT);
    let index = VarUuid::new();
    let var_name: String = node
        .first_token()
        .expect("Expected a child for an var")
        .text()
        .into();
    let var = Var {
        index: index,
        var_type: var_type,
        name: var_name.clone(),
        span: node.text_range(),
    };
    ctx.insert(var_name.clone(), var.clone());

    var
}

fn parse_var(node: &SyntaxNode, ctx: &Context) -> Result<Var, NameNotFoundError> {
    assert!(node.kind() == SyntaxKind::IDENT);
    let var_name = node
        .first_token()
        .expect("Expected a child for an var")
        .text()
        .into();
    let var = match ctx.get(&var_name) {
        None => {
            return Err(NameNotFoundError {
                name: var_name,
                span: node.text_range(),
            })
        }
        Some(var) => var.clone(),
    };
    Ok(var)
}

fn parse_stmt(node: &SyntaxNode, ctx: &mut Context) -> Stmt {
    // dbg!(node);
    let children = nonextra_children(node).collect::<Vec<_>>();

    let stmt_kind = match node.kind() {
        SyntaxKind::LET_STMT => {
            if children.len() == 3 {
                let var_type = parse_expr(&children[1], ctx);

                let var = create_var(&children[0], Some(var_type), ctx);
                let body = parse_expr(&children[2], ctx);
                StmtKind::Let(var, body)
            } else if children.len() == 2 {
                let var = create_var(&children[0], None, ctx);
                let body = parse_expr(&children[1], ctx);
                StmtKind::Let(var, body)
            } else {
                panic!("The length of the let_stmt should be 2 or 4.")
            }
        }
        SyntaxKind::DO_STMT => {
            // do expr
            // becomes let _ = expr;

            if children.len() == 1 {
                let expr = parse_expr(&children[0], ctx);
                let var_index = VarUuid::new();
                let var = Var {
                    index: var_index,
                    var_type: None,
                    name: "_".into(),
                    span: TextRange::empty(expr.1.start()),
                };
                StmtKind::Let(var, expr)
            } else {
                panic!("the length of do_stmt should be 1")
            }
        }
        SyntaxKind::VAL_STMT => {
            // dbg!(children.clone());
            if children.len() == 1 {
                let expr = parse_expr(&children[0], ctx);
                StmtKind::Val(expr)
            } else {
                panic!("the length of val_stmt should be 1")
            }
        }
        SyntaxKind::FN_STMT => {
            if children.len() == 4 {
                // fn foo |binders| -> expr  body
                // becomes let foo (: pi |binders| expr) = lambda |binders| body
                let (binders, new_ctx) = parse_binders(&children[1], ctx);
                let expr = parse_expr(&children[2], ctx);
                let body = parse_expr(&children[3], &new_ctx);
                let func_expr_kind = ExprKind::Pi(binders.clone(), expr.clone());
                let func_span = TextRange::new(binders.1.start(), expr.1.end());
                let func_expr = Expr(Box::new(func_expr_kind), func_span);
                let var = create_var(&children[0], Some(func_expr), ctx);

                // StmtKind::Fn(var, binders, Some(expr), body);

                let body_expr_kind = ExprKind::Lam(binders, body.clone());
                let body_expr = Expr(Box::new(body_expr_kind), body.1);
                StmtKind::Let(var, body_expr)
            } else if children.len() == 3 {
                let (binders, new_ctx) = parse_binders(&children[1], ctx);
                let body = parse_expr(&children[2], &new_ctx); // lamda |binders| body
                let body_expr_kind = ExprKind::Lam(binders, body.clone());
                let body_expr = Expr(Box::new(body_expr_kind), body.1);
                let var = create_var(&children[0], None, ctx);

                StmtKind::Let(var, body_expr)
            } else {
                panic!("the length of fn_stmt should be 4 or 3")
            }
        }
        SyntaxKind::IMPORT_STMT => {
            todo!()
        }
        // SyntaxKind::ERROR => {
        //     let first_word = &children[0];
        //     todo!();
        // }
        _ => panic!("parse_stmt can only parse a stmt"),
    };

    Stmt(stmt_kind, node.text_range())
}

fn parse_expr(node: &SyntaxNode, ctx: &Context) -> Expr {
    // dbg!(node);
    let children = nonextra_children(node).collect::<Vec<_>>();

    let expr_kind = match node.kind() {
        SyntaxKind::IDENT_EXPR => {
            if children.len() == 1 {
                ExprKind::Var(parse_var(&children[0], ctx).unwrap())
            } else {
                panic!("var_expr should just have an var")
            }
        }
        SyntaxKind::TYPE_EXPR => ExprKind::Type,
        SyntaxKind::BANG_EXPR => {
            if children.len() == 1 {
                ExprKind::Bang(parse_expr(&children[0], ctx))
            } else {
                panic!("bang_expr should be of the form [expr] !")
            }
        }
        SyntaxKind::APP_EXPR => {
            if children.len() == 2 {
                let func = parse_expr(&children[0], ctx);
                let elem = parse_expr(&children[1], ctx);
                ExprKind::App(func, elem)
            } else {
                panic!("app_expr should be of the form [func] [elem]")
            }
        }
        SyntaxKind::FUN_EXPR => {
            if children.len() == 2 {
                let expr_1 = parse_expr(&children[0], ctx);
                let expr_2 = parse_expr(&children[1], ctx);
                ExprKind::Fun(expr_1, expr_2)
            } else {
                panic!("Fun_expr should be of the form [expr_1] -> [expr_2]")
            }
        }
        SyntaxKind::LAMBDA_EXPR => {
            if children.len() == 2 {
                let (binders, new_ctx) = parse_binders(&children[0], ctx);
                let expr = parse_expr(&children[1], &new_ctx);
                ExprKind::Lam(binders, expr)
            } else {
                panic!("lambda_expr should be of the form lambda [binders] [expr]")
            }
        }
        SyntaxKind::PI_EXPR => {
            if children.len() == 2 {
                let (binders, new_ctx) = parse_binders(&children[0], ctx);
                let expr = parse_expr(&children[1], &new_ctx);
                ExprKind::Pi(binders, expr)
            } else {
                panic!("pi_expr should be of the form pi [binders] [expr]")
            }
        }
        SyntaxKind::STMT_EXPR => {
            let mut stmt = vec![];
            let mut new_ctx = ctx.clone();
            for child in children {
                stmt.push(parse_stmt(&child, &mut new_ctx))
            }
            ExprKind::Stmt(stmt)
        }
        SyntaxKind::MEMBER_EXPR => {
            if children.len() == 2 {
                let lhs = parse_expr(&children[0], ctx);
                let rhs = parse_expr(&children[1], ctx);
                ExprKind::Member(lhs, rhs)
            } else {
                panic!("member_expr should be of the form [lhs].[rhs]")
            }
        }
        SyntaxKind::STRING_EXPR => {
            let mut string_component = vec![];
            for child in children {
                // dbg!(child.clone());
                let child_token = child.first_token().expect("Expected token");
                if child_token.text().chars().next().unwrap() == '\\' {
                    let string_token_kind = StringTokenKind::Escape(child_token.text()[1..].into());
                    string_component.push(StringToken(string_token_kind, child_token.text_range()));
                } else {
                    let string_token_kind = StringTokenKind::String(child_token.text().into());
                    string_component.push(StringToken(string_token_kind, child_token.text_range()));
                }
            }
            ExprKind::StringLit(string_component)
        }
        _ => panic!("parse_expr can only parse an expr"),
    };
    Expr(Box::new(expr_kind), node.text_range())
}

fn parse_binders(node: &SyntaxNode, ctx: &Context) -> (Binders, BTreeMap<String, Var>) {
    if let SyntaxKind::BINDERS = node.kind() {
        let children = nonextra_children(node).collect::<Vec<_>>();

        let mut ctx = ctx.clone();

        let mut binders = vec![];

        for child in children {
            let grandchildren = nonextra_children(&child).collect::<Vec<_>>();
            if grandchildren.len() == 2 {
                let expr = parse_expr(&grandchildren[1], &ctx);

                let binder_name = create_var(&grandchildren[0], Some(expr), &mut ctx);
                binders.push(binder_name);
            } else if grandchildren.len() == 1 {
                let binder_name = create_var(&grandchildren[0], None, &mut ctx);
                binders.push(binder_name);
            } else {
                panic!("bind_components should be of the form x")
            }
        }

        (Binders(binders, node.text_range()), ctx)
    } else {
        panic!("parse_binder con only parse a binder")
    }
}

#[cfg(test)]
mod test {
    use super::{parse_source_file, SourceFile};
    use crate::rowan_ast::string_to_syntax;

    fn source_code_to_parse(text: &str) -> SourceFile {
        let node = string_to_syntax(text);
        parse_source_file(&node)
    }

    #[test]
    fn test_parser() {
        let text = "fn foo |x : Type, y : Type| -> Type {
            val x}";
        let node = source_code_to_parse(text);
        dbg!(node);

        // let text2 = "fn foo |x| {val x}
        // val foo (Pi |y: Type| y)";
        // let node2 = source_code_to_parse(text2);
        // dbg!(node2);

        let text = "val \"hello\"";
        let node = source_code_to_parse(text);
        dbg!(node);
    }
}
