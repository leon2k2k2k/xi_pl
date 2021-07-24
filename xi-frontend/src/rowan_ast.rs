use std::collections::{BTreeMap, BTreeSet};

use lazy_static::lazy_static;
use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use tree_sitter::Node;
use tree_sitter::{Parser, Tree};

/// mini goal, let str = "let x = 5" and make this into a Rowan Greennode.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    SOURCE_FILE,
    LINE_COMMENT,
    WHITESPACE,
    NEWLINE,
    MUT,
    IDENT,

    DECORATOR_STMT,
    LET_STMT,
    DO_STMT,
    VAL_STMT,
    FN_STMT,
    IMPORT_STMT,
    FFI_STMT,
    REMOTE_STMT,
    IF_STMT,
    WITH_STMT,
    ENUM_STMT,
    STRUCT_STMT,

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
    NUMBER_EXPR,
    BINARY_EXPR,
    DICT_EXPR,
    TUPLE_EXPR,
    LIST_EXPR,
    IF_PHRASE,
    ELSE_IF_PHRASE,
    ELSE_PHRASE,

    BINDERS,
    BINDER_COMPONENT,
    STRING_COMPONENT,
    DICT_COMPONENT,
    ENUM_COMPONENT,
    ENUM_COMPONENTS,
    STRUCT_COMPONENT,
    STRUCT_COMPONENTS,
    IMPORT_COMPONENTS,
    // These need to be the last things
    ERROR,
    UNKNOWN,
}

impl SyntaxKind {
    pub fn is_extra(&self) -> bool {
        use SyntaxKind::*;
        match self {
            UNKNOWN | WHITESPACE | NEWLINE | LINE_COMMENT => true,
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
        assert!(raw.0 <= SyntaxKind::UNKNOWN as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;

/// take a string and give back a GreenNode and a list of errors.
fn parse(text: &str) -> GreenNode {
    let tree = to_tree(text.into());
    let root_node = tree.root_node();
    // initiate our GreenNodeBuilder
    let mut builder = GreenNodeBuilder::new();
    parse_rec(text, root_node, &mut builder);

    builder.finish()
}

/// Take a string(the source code), the current node, the current builder, and list of errors
/// parse through the current node.
pub fn parse_rec(text: &str, node: Node, builder: &mut GreenNodeBuilder) {
    fn get_kind(kind_str: &str) -> SyntaxKind {
        if kind_str == "ERROR" {
            return SyntaxKind::ERROR;
        }

        match LANGUAGE
            .node_names
            .get(&String::from(kind_str).to_ascii_uppercase())
        {
            None => SyntaxKind::UNKNOWN,
            Some(kind) => *kind,
        }
    }

    let kind = get_kind(node.kind());

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

// fn syntax(node: GreenNode) -> SyntaxNode {
//     SyntaxNode::new_root(node.clone())
// }

pub fn string_to_syntax(text: &str) -> SyntaxNode {
    SyntaxNode::new_root(parse(text))
}

pub fn nonextra_children(node: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> {
    node.children().filter(|node| !node.kind().is_extra())
}

extern "C" {
    fn tree_sitter_aplite() -> tree_sitter::Language;
}

struct Language {
    ts_lang: tree_sitter::Language,
    node_names: BTreeMap<String, SyntaxKind>,
}

lazy_static! {
    static ref LANGUAGE: Language = {
        let ts_lang = unsafe { tree_sitter_aplite() };
        let num_kinds = ts_lang.node_kind_count();

        let mut node_names_set: BTreeSet<String> = BTreeSet::new();
        for index in 0..num_kinds {
            if ts_lang.node_kind_is_named(index as u16) {
                let name = ts_lang.node_kind_for_id(index as u16).unwrap();
                if !name.starts_with("_") && name != "end" {
                    node_names_set.insert(name.into());
                }
            }
        }

        assert_eq!(node_names_set.len(), SyntaxKind::ERROR as usize);

        let mut node_names = BTreeMap::new();
        for kind_index in 0..node_names_set.len() {
            let kind: SyntaxKind = unsafe { std::mem::transmute(kind_index as u16) };
            let kind_str = format!("{:?}", kind);
            node_names.insert(kind_str, kind);
        }

        Language {
            ts_lang,
            node_names,
        }
    };
}

pub fn to_tree(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    parser.set_language(LANGUAGE.ts_lang).unwrap();
    parser.parse(source_code, None).unwrap()
}
