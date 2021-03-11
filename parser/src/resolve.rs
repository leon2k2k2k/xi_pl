use crate::syntax_tree::{nonextra_children, SyntaxKind, SyntaxNode, SyntaxToken};
use free_var::FreeVar as IdentIndex;
use rowan::{NodeOrToken, TextRange};
use std::collections::BTreeMap;

#[derive(Clone, Debug)]
struct SourceFile(Vec<Stmt>);

#[derive(Clone, Debug)]
struct Stmt(StmtKind, Span);

#[derive(Clone, Debug)]
enum StmtKind {
    Let(Ident, Option<Expr>, Expr),
    Do(Expr),
    Val(Expr),
    Fn(Ident, Binders, Option<Expr>, Expr), // Expr should be a stmt_expr.
    Import(Ident),
}

#[derive(Clone, Debug)]
struct Expr(Box<ExprKind>, Span);
#[derive(Clone, Debug)]
enum ExprKind {
    Var(Ident),
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
struct StringToken(StringTokenKind, Span);

#[derive(Clone, Debug)]
enum StringTokenKind {
    String(String),
    Escape(String),
}

#[derive(Clone, Debug)]
struct Binders(Vec<(Ident, Option<Expr>)>, Span);
#[derive(Clone, Debug)]
struct Ident(IdentIndex, String, Span);

#[derive(Clone, Debug)]
struct NameNotFoundError {
    name: String,
    span: Span,
}

type Span = rowan::TextRange;

fn default_ctx() -> BTreeMap<String, IdentIndex> {
    let mut ctx = BTreeMap::new();
    ctx.insert("console".into(), IdentIndex::new());
    ctx.insert("".into(), IdentIndex::new());
    ctx
}

fn parse_source_file(node: &SyntaxNode) -> SourceFile {
    let mut ctx = default_ctx();
    let mut stmts = vec![];
    for child in nonextra_children(node) {
        stmts.push(parse_stmt(&child, &mut ctx));
    }
    SourceFile(stmts)
}

fn create_ident(node: &SyntaxNode, ctx: &mut BTreeMap<String, IdentIndex>) -> Ident {
    assert_eq!(node.kind(), SyntaxKind::IDENT);
    let index = IdentIndex::new();
    let var_name: String = node
        .first_token()
        .expect("Expected a child for an ident")
        .text()
        .into();
    ctx.insert(var_name.clone(), index);

    Ident(index, var_name, node.text_range())
}

fn parse_ident(
    node: &SyntaxNode,
    ctx: &BTreeMap<String, IdentIndex>,
) -> Result<Ident, NameNotFoundError> {
    assert!(node.kind() == SyntaxKind::IDENT);
    let var_name = node
        .first_token()
        .expect("Expected a child for an ident")
        .text()
        .into();
    let index = match ctx.get(&var_name) {
        None => {
            return Err(NameNotFoundError {
                name: var_name,
                span: node.text_range(),
            })
        }
        Some(index) => index,
    };
    Ok(Ident(*index, var_name, node.text_range()))
}

fn parse_stmt(node: &SyntaxNode, ctx: &mut BTreeMap<String, IdentIndex>) -> Stmt {
    dbg!(node);
    let children = nonextra_children(node).collect::<Vec<_>>();

    let stmt_kind = match node.kind() {
        SyntaxKind::LET_STMT => {
            if children.len() == 3 {
                let ident = create_ident(&children[0], ctx);
                let var_type = parse_expr(&children[1], ctx);
                let body = parse_expr(&children[2], ctx);
                StmtKind::Let(ident, Some(var_type), body)
            } else if children.len() == 2 {
                let ident = create_ident(&children[0], ctx);
                let body = parse_expr(&children[1], ctx);
                StmtKind::Let(ident, None, body)
            } else {
                panic!("The length of the let_stmt should be 2 or 4.")
            }
        }
        SyntaxKind::DO_STMT => {
            if children.len() == 1 {
                let expr = parse_expr(&children[0], ctx);
                StmtKind::Do(expr)
            } else {
                panic!("the length of do_stmt should be 1")
            }
        }
        SyntaxKind::VAL_STMT => {
            dbg!(children.clone());
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
                let ident = create_ident(&children[0], ctx);
                let (binders, new_ctx) = parse_binders(&children[1], ctx);
                let expr = parse_expr(&children[2], ctx);
                let body = parse_expr(&children[3], &new_ctx);
                // StmtKind::Fn(ident, binders, Some(expr), body);
                let func_expr_kind = ExprKind::Pi(binders.clone(), expr.clone());
                let func_span = TextRange::new(binders.1.start(), expr.1.end());
                let func_expr = Expr(Box::new(func_expr_kind), func_span);
                let body_expr_kind = ExprKind::Lam(binders, body.clone());
                let body_expr = Expr(Box::new(body_expr_kind), body.1);
                StmtKind::Let(ident, Some(func_expr), body_expr)
            } else if children.len() == 3 {
                let ident = create_ident(&children[0], ctx);
                let (binders, new_ctx) = parse_binders(&children[1], ctx);
                let body = parse_expr(&children[2], &new_ctx); // lamda |binders| body
                StmtKind::Fn(ident, binders, None, body)
            } else {
                panic!("the length of fn_stmt should be 4 or 3")
            }
        }
        SyntaxKind::IMPORT_STMT => {
            if children.len() == 1 {
                let ident = create_ident(&children[0], ctx);
                todo!()
            } else {
                panic!("import_stmt have to be of the form import [import]")
            }
        }
        _ => panic!("parse_stmt can only parse a stmt"),
    };

    Stmt(stmt_kind, node.text_range())
}

fn parse_expr(node: &SyntaxNode, ctx: &BTreeMap<String, IdentIndex>) -> Expr {
    dbg!(node);
    let children = nonextra_children(node).collect::<Vec<_>>();

    let expr_kind = match node.kind() {
        SyntaxKind::IDENT_EXPR => {
            if children.len() == 1 {
                ExprKind::Var(parse_ident(&children[0], ctx).unwrap())
            } else {
                panic!("ident_expr should just have an ident")
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
                dbg!(child.clone());
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

fn parse_binders(
    node: &SyntaxNode,
    ctx: &BTreeMap<String, IdentIndex>,
) -> (Binders, BTreeMap<String, IdentIndex>) {
    if let SyntaxKind::BINDERS = node.kind() {
        let children = nonextra_children(node).collect::<Vec<_>>();

        let mut ctx = ctx.clone();

        let mut binders = vec![];

        for child in children {
            let grandchildren = nonextra_children(&child).collect::<Vec<_>>();
            if grandchildren.len() == 2 {
                let binder_name = create_ident(&grandchildren[0], &mut ctx);
                let expr = parse_expr(&grandchildren[1], &ctx);
                binders.push((binder_name, Some(expr)));
            } else if grandchildren.len() == 1 {
                let binder_name = create_ident(&grandchildren[0], &mut ctx);
                binders.push((binder_name, None));
            } else {
                panic!("bind_components should be of the form x")
            }
        }

        (Binders(binders, node.text_range()), ctx)
    } else {
        panic!("parse_binder con only parse a binder")
    }
}

mod test {
    use super::{parse_source_file, SourceFile};
    use crate::syntax_tree::string_to_syntax;
    fn source_code_to_parse(text: &str) -> SourceFile {
        let node = string_to_syntax(text);
        parse_source_file(&node)
    }

    #[test]
    fn test_parser() {
        use super::*;
        let text = "fn foo |x : Type, y : Type| -> Type {
            val x}";
        let node = source_code_to_parse(text);
        dbg!(node);
    }
}
