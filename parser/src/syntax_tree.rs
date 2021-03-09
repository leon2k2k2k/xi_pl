use std::fmt::Display;

use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use rowan::Language;
use tree_sitter::Node;

use crate::to_tree;

/// mini goal, let str = "let x = 5" and make this into a Rowan Greennode.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    SOURCE_FILE,
    LINE_COMMENT,
    WHITESPACE,
    NEWLINE,
    LET_STMT,
    DO_STMT,
    VAL_STMT,
    FN_STMT,
    IMPORT_STMT,
    IDENT,
    IDENT_EXPR,
    TYPE_EXPR,
    BANG_EXPR,
    APP_EXPR,
    FUN_EXPR,
    LAMBDA_EXPR,
    PI_EXPR,
    STMT_EXPR,
    MEMBER_EXPR,
    STRING_EXPR,
    STRING_COMPONENT,
    BINDERS,
    BINDER_COMPONENT,
    ERROR,
    STRING,
}

impl SyntaxKind {
    pub fn is_extra(&self) -> bool {
        use SyntaxKind::*;
        match self {
            ERROR => panic!("error lol"),
            STRING | WHITESPACE | NEWLINE => true,
            _ => false,
        }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::STRING as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;
/// The parse results are stored as a "green tree".
struct Parse {
    green_node: GreenNode,
    #[allow(unused)]
    errors: Vec<String>,
}

/// take a string and give back a GreenNode and a list of errors.
fn parse(text: &str) -> GreenNode {
    use SyntaxKind::*;

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
    use SyntaxKind::*;

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
        "import_stmt" => IMPORT_STMT,
        "ident" => IDENT,
        "ident_expr" => IDENT_EXPR,
        "type_expr" => TYPE_EXPR,
        "bang_expr" => BANG_EXPR,
        "app_expr" => APP_EXPR,
        "fun_expr" => FUN_EXPR,
        "lambda_expr" => LAMBDA_EXPR,
        "pi_expr" => PI_EXPR,
        "stmt_expr" => STMT_EXPR,
        "member_expr" => MEMBER_EXPR,
        "string_expr" => STRING_EXPR,
        "string_component" => STRING_COMPONENT,
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

pub fn string_to_syntax(text: &str) -> SyntaxNode {
    SyntaxNode::new_root(parse(text))
}

fn syntax_node_to_string(node: SyntaxNode) -> String {
    let mut str: String = "".into();
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(node_child) => {
                str.push_str(&format!("{:?}(", node_child.kind()));
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

pub fn nonextra_children(node: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> {
    node.children().filter(|node| !node.kind().is_extra())
}

mod test {
    #[test]
    fn test_syntax_tree() {
        use super::*;
        let text = "fn foo |x : Nat| -> Nat { val x }";
        // let tree_sitter_node = to_tree(text).root_node();
        // println!("{:?}", tree_sitter_node);
        let syntax_node = string_to_syntax(text);
        println!("{:?}", syntax_node_to_string(syntax_node.clone()));
        println!("{:?}", geometric_realization(syntax_node));

        // println!("{:?}", syntax_node.first_token())
    }
}
