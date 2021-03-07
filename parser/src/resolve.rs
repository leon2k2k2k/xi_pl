use crate::syntax_tree::{nonextra_children, SyntaxKind, SyntaxNode, SyntaxToken};
use free_var::FreeVar as IdentIndex;
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
    Fn(Ident, Option<Binders>, Expr), // Expr should be a stmt_expr.
    Import(Expr),
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
    Lam(Ident, Binders, Expr),
    Pi(Ident, Binders, Expr),
    Stmt(Vec<Stmt>),
    Member(Expr, Expr),
    StringLit(StringLit),
}

#[derive(Clone, Debug)]
struct StringLit(Vec<StringToken>, Span);
#[derive(Clone, Debug)]
struct StringToken(StringTokenKind, Span);

#[derive(Clone, Debug)]
enum StringTokenKind {
    Char(char),
    Escape(String),
}

#[derive(Clone, Debug)]
struct Binders(Vec<(Ident, Expr)>, Span);
#[derive(Clone, Debug)]
struct Ident(IdentIndex, String, Span);

#[derive(Clone, Debug)]
struct NameNotFoundError {
    name: String,
    span: Span,
}

type Span = rowan::TextRange;

fn default_ctx() -> BTreeMap<String, IdentIndex> {
    BTreeMap::new()
}

fn parse_source_file(node: SyntaxNode) -> SourceFile {
    let mut ctx = default_ctx();
    let mut stmts = vec![];
    for child in nonextra_children(node) {
        stmts.push(parse_stmt(child, &mut ctx));
    }
    SourceFile(stmts)
}

fn create_ident(node: SyntaxNode, ctx: &mut BTreeMap<String, IdentIndex>) -> Ident {
    assert_eq!(node.kind(), SyntaxKind::IDENT);
    let index = IdentIndex::new();
    let var_name = node
        .first_token()
        .expect("Expected a child for an ident")
        .text()
        .into();
    ctx.insert(var_name, index);

    Ident(index, var_name, node.text_range())
}

fn parse_ident(
    node: SyntaxNode,
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

fn parse_stmt(node: SyntaxNode, ctx: &mut BTreeMap<String, IdentIndex>) -> Stmt {
    let children = nonextra_children(node).collect::<Vec<_>>();

    let stmt_kind = match node.kind() {
        SyntaxKind::LET_STMT => {
            if children.len() == 6 {
                assert_eq!(children[0].text(), "let");
                let ident = create_ident(children[1], ctx);
                assert_eq!(children[2].text(), ":");
                let var_type = parse_expr(children[3], ctx);
                assert_eq!(children[4].text(), "=");
                let body = parse_expr(children[5], ctx);
                StmtKind::Let(ident, Some(var_type), body)
            } else if children.len() == 4 {
                assert_eq!(children[0].text(), "let");
                let ident = create_ident(children[1], ctx);
                assert_eq!(children[2].text(), "=");
                let body = parse_expr(children[3], ctx);
                StmtKind::Let(ident, None, body)
            } else {
                panic!("The length of the let_stmt should be 4 or 6.")
            }
        }
        SyntaxKind::DO_STMT => {
            if children.len() == 1 {
                let expr = parse_expr(children[0], ctx);
                StmtKind::Do(expr)
            } else {
                panic!("the length of do_stmt should be 1")
            }
        }
        SyntaxKind::VAL_STMT => {
            if children.len() == 1 {
                let expr = parse_expr(children[0], ctx);
                StmtKind::Val(expr)
            } else {
                panic!("the length of val_stmt should be 1")
            }
        }
        SyntaxKind::FN_STMT => {}
        SyntaxKind::IMPORT_STMT => {}
        _ => panic!("parse_stmt can only parse a stmt"),
    };

    Stmt(stmt_kind, node.text_range())
}

fn parse_expr(node: SyntaxNode, ctx: &mut BTreeMap<String, IdentIndex>) -> Expr {
    match node.kind() {
        SyntaxKind::TYPE_EXPR => {}
        SyntaxKind::BANG_EXPR => {}
        SyntaxKind::APP_EXPR => {}
        SyntaxKind::FUN_EXPR => {}
        SyntaxKind::LAMBDA_EXPR => {}
        SyntaxKind::PI_EXPR => {}
        SyntaxKind::STMT_EXPR => {}
        SyntaxKind::PAREN_EXPR => {}
        SyntaxKind::MEMBER_EXPR => {}
        SyntaxKind::STRING_EXPR => {}
        _ => panic!("parse_expr can only parse an expr"),
    }
}
