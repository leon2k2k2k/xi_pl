use std::fmt::Display;

use rowan::GreenNodeBuilder;
use rowan::Language;
use rowan::SyntaxKind;
use rowan::{cursor::SyntaxNode, GreenNode};
use tree_sitter::Node;

use crate::to_tree;

/// mini goal, let str = "let x = 5" and make this into a Rowan Greennode.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
enum MySyntaxKind {
    SOURCE_FILE,  //"source_file"
    LINE_COMMENT, //"line_comment"
    WHITESPACE,   // "whitespace"
    NEWLINE,      // "newline"
    LET_STMT,     // "let_stmt"
    DO_STMT,      // "do_stmt"
    VAL_STMT,     // "val_stmt"
    FN_STMT,      // "fn_stmt"
    IDENT,        // "ident"
    TYPE_EXPR,    // "type_expr"
    BANG_EXPR,    // "bang_expr"
    APP_EXPR,     // "app_expr"
    FUN_EXPR,
    LAMBDA_EXPR,
    PI_EXPR,
    STMT_EXPR,
    PAREN_EXPR,
    BINDERS,
    BINDER_COMPONENT,
    ERROR,
    STRING,
}

impl From<MySyntaxKind> for rowan::SyntaxKind {
    fn from(kind: MySyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type Kind = MySyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= MySyntaxKind::STRING as u16);
        unsafe { std::mem::transmute::<u16, MySyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}
/// The parse results are stored as a "green tree".
struct Parse {
    green_node: GreenNode,
    #[allow(unused)]
    errors: Vec<String>,
}

/// take a string and give back a GreenNode and a list of errors.
fn parse(text: &str) -> GreenNode {
    use MySyntaxKind::*;

    let tree = to_tree(text.into());
    let root_node = tree.root_node();
    /// initiate our GreenNodeBuilder
    let mut builder = GreenNodeBuilder::new();
    parse_rec(text, root_node, &mut builder);

    builder.finish()
}

/// Take a string(the source code), the current node, the currnet builder, and list of errors
/// parse through the current node.
pub fn parse_rec(text: &str, node: Node, builder: &mut GreenNodeBuilder) {
    use MySyntaxKind::*;

    let node_kind = node.kind();
    let kind = match node_kind {
        "source_file" => SOURCE_FILE,
        "line_comment" => LINE_COMMENT,
        "whitespace" => WHITESPACE,
        "newline" => NEWLINE,
        "let_stmt" => LET_STMT,
        "do_stmt" => DO_STMT,
        "val_stmt" => VAL_STMT,
        "fn_stmt" => FN_STMT,
        "ident" => IDENT,
        "type_expr" => TYPE_EXPR,
        "bang_expr" => BANG_EXPR,
        "app_expr" => APP_EXPR,
        "fun_expr" => FUN_EXPR,
        "lambda_expr" => LAMBDA_EXPR,
        "pi_expr" => PI_EXPR,
        "stmt_expr" => STMT_EXPR,
        "paren_expr" => PAREN_EXPR,
        "binders" => BINDERS,
        "binder_component" => BINDER_COMPONENT,
        "ERROR" => ERROR,

        _ => STRING,
    };

    builder.start_node(kind.into());

    let child_count = node.child_count();
    if child_count == 0 {
        builder.token(kind.into(), &text[node.byte_range()])
    } else {
        for child in node.children(&mut node.walk()) {
            parse_rec(text, child, builder);
        }
    }

    builder.finish_node();
}

fn syntax(node: GreenNode) -> SyntaxNode {
    SyntaxNode::new_root(node.clone())
}

fn string_to_syntax(text: &str) -> SyntaxNode {
    SyntaxNode::new_root(parse(text))
}

fn syntax_node_to_string(node: SyntaxNode) -> String {
    let mut str: String = "".into();
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(node_child) => {
                str.push_str(&format!("{:?}(", Lang::kind_from_raw(node_child.kind())));
                str.push_str(syntax_node_to_string(node_child.clone()).as_str());
                str.push_str(")");
            }

            rowan::NodeOrToken::Token(node_token) => str.push_str(node_token.text()),
        }
    }
    str
}

fn geometric_realization(node: SyntaxNode) -> String {
    let mut str: String = "".into();
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(node_child) => {
                str.push_str(geometric_realization(node_child.clone()).as_str());
            }

            rowan::NodeOrToken::Token(node_token) => str.push_str(node_token.text()),
        }
    }
    str
}

#[test]
fn test_parser() {
    use super::*;
    let text = "fn f (x : int) -> int { val x + 1
    }";
    let syntax_node = string_to_syntax(text);
    println!("{:?}", syntax_node_to_string(syntax_node.clone()));
    println!("{:?}", geometric_realization(syntax_node));

    // println!("{:?}", syntax_node.first_token())
}
