use crate::syntax_tree::{nonextra_children, SyntaxKind, SyntaxNode, SyntaxToken};
use free_var::FreeVar as IdentIndex;
use rowan::NodeOrToken;
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
    let children = nonextra_children(node).collect::<Vec<_>>();

    let stmt_kind = match node.kind() {
        SyntaxKind::LET_STMT => {
            if children.len() == 6 {
                assert_eq!(children[0].text(), "let");
                let ident = create_ident(&children[1], ctx);
                assert_eq!(children[2].text(), ":");
                let var_type = parse_expr(&children[3], ctx);
                assert_eq!(children[4].text(), "=");
                let body = parse_expr(&children[5], ctx);
                StmtKind::Let(ident, Some(var_type), body)
            } else if children.len() == 4 {
                assert_eq!(children[0].text(), "let");
                let ident = create_ident(&children[1], ctx);
                assert_eq!(children[2].text(), "=");
                let body = parse_expr(&children[3], ctx);
                StmtKind::Let(ident, None, body)
            } else {
                panic!("The length of the let_stmt should be 4 or 6.")
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
            if children.len() == 1 {
                let expr = parse_expr(&children[0], ctx);
                StmtKind::Val(expr)
            } else {
                panic!("the length of val_stmt should be 1")
            }
        }
        SyntaxKind::FN_STMT => {
            if children.len() == 6 {
                // fn foo |binders| -> return_type  {stmt_expr}
                assert_eq!(children[0].text(), "fn");
                let ident = create_ident(&children[1], ctx);
                let binders = parse_binders(&children[2], ctx);
                assert_eq!(children[3].text(), "->");
                let expr = parse_expr(&children[4], ctx);
                let body = parse_expr(&children[5], ctx);
                StmtKind::Fn(ident, binders, Some(expr), body)
            } else if children.len() == 4 {
                assert_eq!(children[0].text(), "fn");
                let ident = create_ident(&children[1], ctx);
                let binders = parse_binders(&children[2], ctx);
                let body = parse_expr(&children[5], ctx);
                StmtKind::Fn(ident, binders, None, body)
            } else {
                panic!("the length of fn_stmt should be 6")
            }
        }
        SyntaxKind::IMPORT_STMT => {
            if children.len() == 2 {
                assert_eq!(children[0].text(), "import");
                let ident = create_ident(&children[1], ctx);
                todo!()
            } else {
                panic!("import_stmt have to be of the form import [import]")
            }
        }
        _ => panic!("parse_stmt can only parse a stmt"),
    };

    Stmt(stmt_kind, node.text_range())
}

fn parse_expr(node: &SyntaxNode, ctx: &mut BTreeMap<String, IdentIndex>) -> Expr {
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
            if children.len() == 2 {
                assert_eq!(children[1].text(), "!");
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
            if children.len() == 3 {
                assert_eq!(children[1].text(), "->");
                let expr_1 = parse_expr(&children[0], ctx);
                let expr_2 = parse_expr(&children[2], ctx);
                ExprKind::Fun(expr_1, expr_2)
            } else {
                panic!("Fun_expr should be of the form [expr_1] -> [expr_2]")
            }
        }
        SyntaxKind::LAMBDA_EXPR => {
            if children.len() == 3 {
                assert_eq!(children[0].text(), "lambda");
                let binders = parse_binders(&children[1], ctx);
                let expr = parse_expr(&children[2], ctx);
                ExprKind::Lam(binders, expr)
            } else {
                panic!("lambda_expr should be of the form lambda [binders] [expr]")
            }
        }
        SyntaxKind::PI_EXPR => {
            if children.len() == 3 {
                assert_eq!(children[0].text(), "pi");
                let binders = parse_binders(&children[1], ctx);
                let expr = parse_expr(&children[2], ctx);
                ExprKind::Pi(binders, expr)
            } else {
                panic!("pi_expr should be of the form pi [binders] [expr]")
            }
        }
        SyntaxKind::STMT_EXPR => {
            let mut stmt = vec![];
            for child in children {
                stmt.push(parse_stmt(&child, ctx))
            }
            ExprKind::Stmt(stmt)
        }
        SyntaxKind::MEMBER_EXPR => {
            if children.len() == 3 {
                assert_eq!(children[1].text(), ".");
                let lhs = parse_expr(&children[0], ctx);
                let rhs = parse_expr(&children[2], ctx);
                ExprKind::Member(lhs, rhs)
            } else {
                panic!("member_expr should be of the form [lhs].[rhs]")
            }
        }
        SyntaxKind::STRING_EXPR => {
            let actual_children = node.children_with_tokens();
            panic!("tried to parse a string");
            // for child in actual_children {
            //     match child {
            //         NodeOrToken::Node(child) => {
            //             if child.first_token().unwrap().text() == "\\" {
            //                 // assert!(child.)
            //                 string_component.push(child.last_token());
            //             } else {
            //                 string_component.push(child.first_token().push());
            //             }
            //         }
            //         NodeOrToken::Token(child) => {
            //             let text = child.text();
            //             for char in text.chars() {
            //                 string_component.push(StringTokenKind::Char(char));
            //             }
            //         }
            //     }
            // }
        }
        _ => panic!("parse_expr can only parse an expr"),
    };
    Expr(Box::new(expr_kind), node.text_range())
}

fn parse_binders(node: &SyntaxNode, ctx: &mut BTreeMap<String, IdentIndex>) -> Binders {
    if let SyntaxKind::BINDERS = node.kind() {
        let binders = vec![];
        // TODO!//

        Binders(binders, node.text_range())
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
        let text = "fn foo |x : Type| -> Type {val x }";
        let node = source_code_to_parse(text);
        dbg!(node);
    }
}
