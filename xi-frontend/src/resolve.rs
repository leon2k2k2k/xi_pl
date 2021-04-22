use crate::{
    rowan_ast::{nonextra_children, SyntaxKind, SyntaxNode},
    type_inference::{UiMetadata, UiPrim},
};
use crate::{type_inference::UiBinaryOp as BinaryOp, Module};
use rowan::{TextRange, TextSize};
use std::collections::{BTreeMap, BTreeSet};
use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

// remember to change the prims() method below
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ResolvePrim {
    IOMonad,
    String,
    Int,
}
impl ResolvePrim {
    pub fn prims() -> Vec<ResolvePrim> {
        use ResolvePrim::*;
        vec![IOMonad, String, Int]
    }
}

impl std::fmt::Display for ResolvePrim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ResolvePrim::*;
        let res = match self {
            IOMonad => "IO",
            String => "String",
            Int => "Int",
        };
        write!(f, "{}", res)
    }
}

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
    Let(Var, Option<Expr>, Expr, BTreeSet<VarUuid>),
    // Do(Expr),
    Val(Expr),
    Ffi(String, Vec<(Var, Expr)>),
    Enum(Var, Option<Binders>, Var, Vec<(Var, Vec<Expr>)>),
    Struct(Var, Option<Binders>, Vec<(Var, Expr)>),
    // Fn(Ident, Binders, Option<Expr>, Expr), // Expr should be a stmt_expr.
    // Import(Var),
    // Error(StmtError)
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
    Number(String),
    Binary(BinaryOp, Expr, Expr),
}

#[derive(Clone, Debug)]
pub struct StringToken(pub StringTokenKind, pub Span);

#[derive(Clone, Debug)]
pub enum StringTokenKind {
    String(String),
    Escape(String),
}

#[derive(Clone, Debug)]
pub struct Var {
    pub index: VarUuid,
    pub name: String,
    pub span: Span,
    pub local_or_global: LocalOrGlobal,
}

#[derive(Clone, Debug)]
pub struct Binders(pub Vec<(Var, Option<Expr>)>, pub Span);

#[derive(Clone, Debug)]
pub enum LocalOrGlobal {
    Local,
    Global,
}
#[derive(Clone, Debug)]
pub struct ImportVar {
    pub index: VarUuid,
    pub var_type: Judgment<UiPrim, UiMetadata>,
    pub name: String,
    pub span: Span,
    // pub origin
}

#[derive(Clone, Debug)]
pub struct NameNotFoundError {
    name: String,
    span: Span,
}

pub type Span = rowan::TextRange;

#[derive(Clone, Debug)]
struct Context {
    local_ctx: BTreeMap<String, Var>,
    import_ctx: Module,
    dependencies: Vec<VarUuid>,
}

pub fn new_span() -> Span {
    Span::new(TextSize::from(0), TextSize::from(0))
}

impl Context {
    fn get_ctx(module: &mut Module) -> (Context, BTreeMap<VarUuid, ResolvePrim>) {
        let mut ident_map = BTreeMap::new();
        let mut resolve_map = BTreeMap::new();
        for prim in ResolvePrim::prims() {
            let var = Var {
                index: VarUuid::new(),
                name: prim.to_string(),
                span: Span::new(TextSize::from(0), TextSize::from(0)),
                local_or_global: LocalOrGlobal::Local,
            };

            ident_map.insert(prim.to_string(), var.clone());
            resolve_map.insert(var.index, prim.clone());
        }

        (
            Context {
                local_ctx: ident_map,
                import_ctx: module.clone(),
                dependencies: vec![],
            },
            resolve_map,
        )
    }

    fn create_var(&mut self, node: &SyntaxNode) -> Var {
        assert_eq!(node.kind(), SyntaxKind::IDENT);
        let index = VarUuid::new();
        let var_name: String = node
            .first_token()
            .expect("Expected a child for an var")
            .text()
            .into();

        let var = Var {
            index: index,
            name: var_name.clone(),
            span: node.text_range(),
            local_or_global: LocalOrGlobal::Local,
        };
        self.local_ctx.insert(var_name.clone(), var.clone());

        var
    }

    fn parse_var(&self, node: &SyntaxNode) -> Result<Var, NameNotFoundError> {
        assert!(node.kind() == SyntaxKind::IDENT);
        let var_name = node
            .first_token()
            .expect("Expected a child for an var")
            .text()
            .into();
        match self.local_ctx.get(&var_name) {
            Some(var) => Ok(Var {
                index: var.index,
                name: var_name,
                span: node.text_range(),
                local_or_global: LocalOrGlobal::Local,
            }),
            None => {
                if let Some(index) = self.import_ctx.str_to_index.get(&var_name) {
                    Ok(Var {
                        index: *index,
                        name: var_name,
                        span: node.text_range(),
                        local_or_global: LocalOrGlobal::Global,
                    })
                } else {
                    Err(NameNotFoundError {
                        name: var_name,
                        span: node.text_range(),
                    })
                }
            }
        }
    }

    fn parse_stmt(&mut self, node: &SyntaxNode) -> Stmt {
        // dbg!(node);
        let children = nonextra_children(node).collect::<Vec<_>>();

        let stmt_kind = match node.kind() {
            SyntaxKind::LET_STMT => {
                if children.len() == 3 {
                    let var_type = self.parse_expr(&children[1]);
                    let body = self.parse_expr(&children[2]);
                    let var = self.create_var(&children[0]);

                    StmtKind::Let(var, Some(var_type), body, BTreeSet::new())
                } else if children.len() == 2 {
                    let body = self.parse_expr(&children[1]);
                    let var = self.create_var(&children[0]);

                    StmtKind::Let(var, None, body, BTreeSet::new())
                } else {
                    panic!("The length of the let_stmt should be 2 or 4.")
                }
            }

            SyntaxKind::DO_STMT => {
                // do expr
                // becomes let _ = expr;

                if children.len() == 1 {
                    let expr = self.parse_expr(&children[0]);
                    let var_index = VarUuid::new();
                    let var = Var {
                        index: var_index,
                        name: "_".into(),
                        span: TextRange::empty(expr.1.start()),
                        local_or_global: LocalOrGlobal::Local,
                    };
                    StmtKind::Let(var, None, expr, BTreeSet::new())
                } else {
                    panic!("the length of do_stmt should be 1")
                }
            }
            SyntaxKind::VAL_STMT => {
                // dbg!(children.clone());
                if children.len() == 1 {
                    let expr = self.parse_expr(&children[0]);
                    StmtKind::Val(expr)
                } else {
                    panic!("the length of val_stmt should be 1")
                }
            }
            SyntaxKind::FN_STMT => {
                if children.len() == 4 {
                    // fn foo |binders| -> ret_type  body
                    // becomes let foo (: pi |binders| ret_type) = lambda |binders| body
                    let (binders, new_ctx) = self.parse_binders(&children[1]);
                    let ret_type = new_ctx.parse_expr(&children[2]);
                    let body = new_ctx.parse_expr(&children[3]);

                    let func_expr_kind = ExprKind::Pi(binders.clone(), ret_type.clone());
                    let func_span = TextRange::new(binders.1.start(), ret_type.1.end());
                    let func_expr = Expr(Box::new(func_expr_kind), func_span);
                    let var = self.create_var(&children[0]);

                    // StmtKind::Fn(var, binders, Some(expr), body);

                    let body_expr_kind = ExprKind::Lam(binders, body.clone());
                    let body_expr = Expr(Box::new(body_expr_kind), body.1);

                    StmtKind::Let(var.clone(), Some(func_expr), body_expr, BTreeSet::new())
                } else if children.len() == 3 {
                    let (binders, new_ctx) = self.parse_binders(&children[1]);
                    let body = new_ctx.parse_expr(&children[2]); // lamda |binders| body
                    let body_expr_kind = ExprKind::Lam(binders, body.clone());
                    let body_expr = Expr(Box::new(body_expr_kind), body.1);
                    let var = self.create_var(&children[0]);

                    StmtKind::Let(var.clone(), None, body_expr, BTreeSet::new())
                } else {
                    panic!("the length of fn_stmt should be 4 or 3")
                }
            }
            SyntaxKind::IMPORT_STMT => {
                todo!()
            }
            SyntaxKind::FFI_STMT => {
                if children.len() == 2 {
                    let file_name = children[0]
                        .first_token()
                        .unwrap()
                        .next_token()
                        .unwrap()
                        .text()
                        .into();
                    let dict = &children[1];
                    let result = self.parse_ffi_dict(&dict);

                    StmtKind::Ffi(file_name, result)
                } else {
                    panic!("the length of ffi_stmt should be 2")
                }
            }
            SyntaxKind::WITH_STMT => {
                if children.len() == 2 {
                    let with_var = self.parse_var(&children[0]).expect("expected var");
                    let inside_stmt = self.parse_stmt(&children[1]);
                    let new_inside_stmt = match inside_stmt.0 {
                        StmtKind::Let(var_name, var_type, body, with_list) => {
                            let mut new_with_list = with_list.clone();
                            new_with_list.insert(with_var.index);
                            StmtKind::Let(var_name, var_type, body, new_with_list)
                        }
                        _ => panic!("with only works with let"),
                    };

                    new_inside_stmt
                } else {
                    panic!("the length of with_stmt should be 2")
                }
            }
            SyntaxKind::ENUM_STMT => {
                if children.len() == 3 || children.len() == 2 {
                    let enum_var = self.create_var(&children[0]);
                    let (binders, mut new_ctx) = match children.len() {
                        3 => {
                            let (binders, new_ctx) = self.parse_binders(&children[1]);
                            (Some(binders), new_ctx)
                        }
                        2 => (None, self.clone()),
                        _ => panic!("the length of enum_stmt should be 2 or 3"),
                    };

                    let self_var = Var {
                        index: VarUuid::new(),
                        name: "Self".into(),
                        span: children[0].text_range(),
                        local_or_global: LocalOrGlobal::Local,
                    };
                    new_ctx.local_ctx.insert("Self".into(), self_var.clone());

                    let mut variants = vec![];
                    for child in nonextra_children(&children[children.len() - 1]) {
                        let grandchildren = nonextra_children(&child).collect::<Vec<_>>();

                        let variant_var = self.create_var(&grandchildren[0]);

                        let variant_type = if grandchildren.len() == 1 {
                            vec![]
                        } else {
                            new_ctx.parse_tuple(&grandchildren[1])
                        };

                        variants.push((variant_var, variant_type))
                    }

                    StmtKind::Enum(enum_var, binders, self_var, variants)
                } else {
                    panic!("the length of enum_stmt should be 2 or 3")
                }
            }
            SyntaxKind::STRUCT_STMT => {
                if children.len() == 3 || children.len() == 2 {
                    let type_u = Some(Expr(Box::new(ExprKind::Type), children[0].text_range()));
                    let struct_var = self.create_var(&children[0]);
                    let (binders, new_ctx) = match children.len() {
                        3 => {
                            let (binders, new_ctx) = self.parse_binders(&children[1]);
                            (Some(binders), new_ctx)
                        }
                        2 => (None, self.clone()),
                        _ => panic!("the length of enum_stmt should be 2 or 3"),
                    };

                    let mut fields = vec![];
                    for child in nonextra_children(&children[children.len() - 1]) {
                        let grandchildren = nonextra_children(&child).collect::<Vec<_>>();

                        let field_var = self.create_var(&grandchildren[0]);

                        let field_type = new_ctx.parse_expr(&grandchildren[1]);
                        fields.push((field_var, field_type))
                    }

                    StmtKind::Struct(struct_var, binders, fields)
                } else {
                    panic!("the length of enum_stmt should be 2 or 3")
                }
            }
            _ => panic!("parse_stmt can only parse an stmt, got {:?}", node.kind()),
        };

        Stmt(stmt_kind, node.text_range())
    }

    fn parse_ffi_dict(&mut self, node: &SyntaxNode) -> Vec<(Var, Expr)> {
        let children = nonextra_children(node).collect::<Vec<_>>();
        assert!(node.kind() == SyntaxKind::DICT_EXPR);
        let mut components = vec![];

        for child in children {
            let grandchildren = nonextra_children(&child).collect::<Vec<_>>();
            if grandchildren.len() == 2 {
                let ffi_type = self.parse_expr(&grandchildren[1]);

                let ffi_var = self.create_var(&grandchildren[0]);

                components.push((ffi_var, ffi_type));
            } else {
                panic!("dict component should have 3 things")
            }
        }

        components
    }

    fn parse_expr(&self, node: &SyntaxNode) -> Expr {
        // dbg!(node);
        let children = nonextra_children(node).collect::<Vec<_>>();

        let expr_kind = match node.kind() {
            SyntaxKind::IDENT_EXPR => {
                if children.len() == 1 {
                    ExprKind::Var(self.parse_var(&children[0]).unwrap())
                } else {
                    panic!("var_expr should just have an var")
                }
            }
            SyntaxKind::TYPE_EXPR => ExprKind::Type,
            SyntaxKind::BANG_EXPR => {
                if children.len() == 1 {
                    ExprKind::Bang(self.parse_expr(&children[0]))
                } else {
                    panic!("bang_expr should be of the form [expr] !")
                }
            }
            SyntaxKind::APP_EXPR => {
                if children.len() == 2 {
                    let func = self.parse_expr(&children[0]);
                    let elem = self.parse_expr(&children[1]);
                    ExprKind::App(func, elem)
                } else {
                    panic!("app_expr should be of the form [func] [elem]")
                }
            }
            SyntaxKind::FUN_EXPR => {
                if children.len() == 2 {
                    let expr_1 = self.parse_expr(&children[0]);
                    let expr_2 = self.parse_expr(&children[1]);
                    ExprKind::Fun(expr_1, expr_2)
                } else {
                    panic!("Fun_expr should be of the form [expr_1] -> [expr_2]")
                }
            }
            SyntaxKind::LAMBDA_EXPR => {
                if children.len() == 2 {
                    let (binders, new_ctx) = self.parse_binders(&children[0]);
                    let expr = new_ctx.parse_expr(&children[1]);
                    ExprKind::Lam(binders, expr)
                } else {
                    panic!("lambda_expr should be of the form lambda [binders] [expr]")
                }
            }
            SyntaxKind::PI_EXPR => {
                if children.len() == 2 {
                    let (binders, new_ctx) = self.parse_binders(&children[0]);
                    let expr = new_ctx.parse_expr(&children[1]);
                    ExprKind::Pi(binders, expr)
                } else {
                    panic!("pi_expr should be of the form pi [binders] [expr]")
                }
            }
            SyntaxKind::STMT_EXPR => {
                let mut stmt = vec![];
                let mut new_ctx = self.clone();
                for child in children {
                    stmt.push(new_ctx.parse_stmt(&child))
                }
                ExprKind::Stmt(stmt)
            }
            SyntaxKind::MEMBER_EXPR => {
                if children.len() == 2 {
                    let lhs = self.parse_expr(&children[0]);
                    let rhs = self.parse_expr(&children[1]);
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
                        let string_token_kind =
                            StringTokenKind::Escape(child_token.text()[1..].into());
                        string_component
                            .push(StringToken(string_token_kind, child_token.text_range()));
                    } else {
                        let string_token_kind = StringTokenKind::String(child_token.text().into());
                        string_component
                            .push(StringToken(string_token_kind, child_token.text_range()));
                    }
                }
                ExprKind::StringLit(string_component)
            }
            SyntaxKind::BINARY_EXPR => {
                if children.len() == 2 {
                    let operator = node
                        .children()
                        .filter(|node| node.kind() == SyntaxKind::UNKNOWN)
                        .next()
                        .unwrap();
                    let token = operator.first_token().unwrap();
                    let op_name = token.text();

                    use BinaryOp::*;
                    let operator = match op_name {
                        "&&" => Minus,
                        "//" => Or,
                        "==" => Equal,
                        "!=" => NotEqual,
                        "<" => LessThan,
                        "<=" => LessThanEqual,
                        ">" => GreaterThan,
                        ">=" => GreaterThanEqual,
                        "+" => Plus,
                        "-" => Minus,
                        "*" => Multiply,
                        "/" => Divide,
                        "%" => Modulo,
                        _ => panic!(format!("the binary operator {} is not defined", op_name)),
                    };

                    ExprKind::Binary(
                        operator,
                        self.parse_expr(&children[0]),
                        self.parse_expr(&children[1]),
                    )
                } else {
                    panic!("binary expression should have two children");
                }
            }

            SyntaxKind::NUMBER_EXPR => {
                let token = node.first_token().unwrap();
                let number = token.text();
                ExprKind::Number(number.into())
            }
            SyntaxKind::DICT_EXPR => todo!(),
            SyntaxKind::TUPLE_EXPR => todo!(),
            SyntaxKind::LIST_EXPR => todo!(),

            _ => panic!("parse_expr can only parse an expr, got {:?}", node.kind()),
        };
        Expr(Box::new(expr_kind), node.text_range())
    }

    fn parse_tuple(&self, node: &SyntaxNode) -> Vec<Expr> {
        assert_eq!(SyntaxKind::TUPLE_EXPR, node.kind());

        let mut exprs = vec![];
        for child in nonextra_children(node) {
            exprs.push(self.parse_expr(&child));
        }
        exprs
    }

    fn parse_binders(&self, node: &SyntaxNode) -> (Binders, Context) {
        if let SyntaxKind::BINDERS = node.kind() {
            let mut ctx = self.clone();

            let mut binders = vec![];
            for child in nonextra_children(node) {
                let grandchildren = nonextra_children(&child).collect::<Vec<_>>();
                if grandchildren.len() == 2 {
                    let expr = ctx.parse_expr(&grandchildren[1]);

                    let binder_name = ctx.create_var(&grandchildren[0]);
                    binders.push((binder_name, Some(expr)));
                } else if grandchildren.len() == 1 {
                    let binder_name = ctx.create_var(&grandchildren[0]);
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
}

pub fn parse_module_stmt(
    module: &mut Module,
    node: &SyntaxNode,
) -> (Stmt, BTreeMap<VarUuid, ResolvePrim>) {
    let (mut ctx, resolve_var) = Context::get_ctx(module);
    let stmt = ctx.parse_stmt(node);

    (stmt, resolve_var)
}

#[cfg(test)]
mod test {
    // use super::{parse_source_file, SourceFile};
    // use crate::rowan_ast::string_to_syntax;

    // fn source_code_to_parse(text: &str) -> SourceFile {
    //     let node = string_to_syntax(text);
    //     parse_source_file(&node)
    // }

    // #[test]
    // fn test_parser() {
    //     // let text = "fn foo |x : Type, y : Type| -> Type {
    //     //     val x}";
    //     // let node = source_code_to_parse(text);
    //     // dbg!(node);

    //     // // let text2 = "fn foo |x| {val x}
    //     // // val foo (Pi |y: Type| y)";
    //     // // let node2 = source_code_to_parse(text2);
    //     // // dbg!(node2);

    //     // let text = "val \"hello\"";
    //     // let node = source_code_to_parse(text);
    //     // dbg!(node);

    //     let text = "ffi \"some_file.js\" {
    //         add1: Type -> Type,
    //         add2: Type -> Type,
    //         Justin: Type,
    //     }
    //     val add1 add2 Justin";
    //     let node = source_code_to_parse(text);
    //     dbg!(node);
    // }
}
