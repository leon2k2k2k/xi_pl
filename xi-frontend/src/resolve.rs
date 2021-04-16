use crate::ModuleItem;
use crate::{rowan_ast::Lang, type_inference::UiBinaryOp as BinaryOp, Module};
use crate::{
    rowan_ast::{nonextra_children, SyntaxKind, SyntaxNode},
    type_inference::{UiMetadata, UiPrim},
};
use rowan::{TextRange, TextSize};
use std::collections::BTreeMap;
use xi_core::judgment::Judgment;
use xi_uuid::VarUuid;

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
    Let(VarBinder, Expr),
    // Do(Expr),
    Val(Expr),
    Ffi(String, Vec<VarBinder>),
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
pub struct Binders(pub Vec<VarBinder>, pub Span);

#[derive(Clone, Debug)]
pub struct VarBinder {
    pub index: VarUuid,
    pub var_type: Option<Expr>,
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub index: VarUuid,
    pub name: String,
    pub span: Span,
    pub local_or_global: LocalOrGlobal,
}

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
    local_ctx: BTreeMap<String, VarBinder>,
    import_ctx: Module,
    dependencies: Vec<VarUuid>,
}

impl Context {
    pub fn new(module: Module) -> Context {
        Context {
            local_ctx: BTreeMap::new(),
            import_ctx: module,
            dependencies: vec![],
        }
    }
    fn create_var(&mut self, node: &SyntaxNode, var_type: Option<Expr>) -> VarBinder {
        assert_eq!(node.kind(), SyntaxKind::IDENT);
        let index = VarUuid::new();
        let var_name: String = node
            .first_token()
            .expect("Expected a child for an var")
            .text()
            .into();
        let var = VarBinder {
            index: index,
            var_type: var_type,
            name: var_name.clone(),
            span: node.text_range(),
        };
        self.local_ctx.insert(var_name.clone(), var.clone());

        var
    }

    // check if the ident exists in the local context, then check if it exists in module, if it does, then check has it been used before, if not, then add it onto the dependencies vec.
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

                    let var = self.create_var(&children[0], Some(var_type));
                    let body = self.parse_expr(&children[2]);
                    StmtKind::Let(var, body)
                } else if children.len() == 2 {
                    let var = self.create_var(&children[0], None);
                    let body = self.parse_expr(&children[1]);
                    StmtKind::Let(var, body)
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
                    let var = VarBinder {
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
                    let var = self.create_var(&children[0], Some(func_expr));

                    // StmtKind::Fn(var, binders, Some(expr), body);

                    let body_expr_kind = ExprKind::Lam(binders, body.clone());
                    let body_expr = Expr(Box::new(body_expr_kind), body.1);
                    StmtKind::Let(var, body_expr)
                } else if children.len() == 3 {
                    let (binders, new_ctx) = self.parse_binders(&children[1]);
                    let body = new_ctx.parse_expr(&children[2]); // lamda |binders| body
                    let body_expr_kind = ExprKind::Lam(binders, body.clone());
                    let body_expr = Expr(Box::new(body_expr_kind), body.1);
                    let var = self.create_var(&children[0], None);

                    StmtKind::Let(var, body_expr)
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
                    panic!("ffi_stmt should only have two children")
                }
            }
            // SyntaxKind::ERROR => {
            //     let first_word = &children[0];
            //     todo!();
            // }
            _ => panic!("parse_stmt can only parse an stmt, got {:?}", node.kind()),
        };

        Stmt(stmt_kind, node.text_range())
    }

    fn parse_ffi_dict(&mut self, node: &SyntaxNode) -> Vec<VarBinder> {
        let children = nonextra_children(node).collect::<Vec<_>>();
        assert!(node.kind() == SyntaxKind::DICT_EXPR);
        let mut components = vec![];

        for child in children {
            let grandchildren = nonextra_children(&child).collect::<Vec<_>>();
            if grandchildren.len() == 2 {
                let ffi_type = self.parse_expr(&grandchildren[1]);

                let ffi_var = self.create_var(&grandchildren[0], Some(ffi_type));

                components.push(ffi_var);
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

    fn parse_binders(&self, node: &SyntaxNode) -> (Binders, Context) {
        if let SyntaxKind::BINDERS = node.kind() {
            let children = nonextra_children(node).collect::<Vec<_>>();

            let mut ctx = self.clone();

            let mut binders = vec![];

            for child in children {
                let grandchildren = nonextra_children(&child).collect::<Vec<_>>();
                if grandchildren.len() == 2 {
                    let expr = ctx.parse_expr(&grandchildren[1]);

                    let binder_name = ctx.create_var(&grandchildren[0], Some(expr));
                    binders.push(binder_name);
                } else if grandchildren.len() == 1 {
                    let binder_name = ctx.create_var(&grandchildren[0], None);
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
}

pub fn parse_module_stmt(module: &mut Module, node: &SyntaxNode) -> (String, ModuleStmt) {
    let mut ctx = Context::new(module.clone());
    let children = nonextra_children(node).collect::<Vec<_>>();

    let name: String = match node.kind() {
        SyntaxKind::LET_STMT => children[0].first_token().expect("a token").text().into(),
        SyntaxKind::FN_STMT => children[0].first_token().expect("a token").text().into(),
        SyntaxKind::IMPORT_STMT => todo!(),
        SyntaxKind::FFI_STMT => todo!(),
        _ => panic!("wrong stmt on the module level"),
    };

    (
        name,
        ModuleStmt {
            impl_: ctx.parse_stmt(node),
        },
    )
}

#[derive(Clone, Debug)]
pub struct ModuleStmt {
    pub impl_: Stmt,
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
