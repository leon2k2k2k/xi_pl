use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{braced, parenthesized, parse_macro_input, token, Expr, Ident, Result, Token};

#[proc_macro]
pub fn term(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let builder = parse_macro_input!(tokens as TermBuilder);
    (quote! { #builder }).into()
}

#[derive(Clone, Debug)]
enum TermBuilder {
    Type,
    Var(Ident),
    RustExpr(Expr),
    Lam(Ident, Box<TermBuilder>, Box<TermBuilder>),
    Pi(Ident, Box<TermBuilder>, Box<TermBuilder>),
    Fun(Box<TermBuilder>, Box<TermBuilder>),
    App(Box<TermBuilder>, Box<TermBuilder>),
}

impl ToTokens for TermBuilder {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            TermBuilder::Type => tokens.extend(quote! { U() }),
            TermBuilder::Var(ident) => tokens.extend(quote! { #ident }),
            TermBuilder::RustExpr(expr) => tokens.extend(quote! { #expr }),
            TermBuilder::Lam(ident, type_, body) => {
                let ident_as_str = format!("{}", ident);
                let lam_expr = quote! {{
                    let #ident = #type_.var(#ident_as_str);
                    #ident.lam(& #body)
                }};
                tokens.extend(lam_expr);
            }
            TermBuilder::Pi(ident, type_, body) => {
                let ident_as_str = format!("{}", ident);
                let pi_expr = quote! {{
                    let #ident = #type_.var(#ident_as_str);
                    #ident.pi(& #body)
                }};
                tokens.extend(pi_expr);
            }
            TermBuilder::Fun(domain, codomain) => {
                let fun_expr = quote! {
                    #domain.fun(& #codomain)
                };
                tokens.extend(fun_expr);
            }
            TermBuilder::App(fun, arg) => {
                let app_expr = quote! {
                    #fun.app(& #arg)
                };
                tokens.extend(app_expr);
            }
        }
    }
}

// parser implementation based off
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

impl Parse for TermBuilder {
    fn parse(tokens: ParseStream) -> Result<TermBuilder> {
        expr_bp(tokens, 0)
    }
}

#[derive(Clone, Debug)]
enum Op {
    App,
    RArrow,
}

fn expr_bp(tokens: ParseStream, min_bp: u8) -> Result<TermBuilder> {
    let mut lhs = if tokens.peek(Ident) {
        let ident_name = tokens.parse::<Ident>()?;
        if ident_name.to_string() == "Lam" {
            parse_lam(tokens)?
        } else if ident_name.to_string() == "Pi" {
            parse_pi(tokens)?
        } else if ident_name.to_string() == "U" {
            TermBuilder::Type
        } else {
            TermBuilder::Var(ident_name)
        }
    } else if tokens.peek(token::Paren) {
        parse_parens(tokens)?
    } else if tokens.peek(token::Brace) {
        parse_rust_expr(tokens)?
    } else {
        return Err(tokens.error("Unexpected token"));
    };

    loop {
        // Break if no more input
        if tokens.is_empty() {
            break;
        }

        // Compute binding power of next token
        if let Some((token, l_bp, r_bp)) = {
            if tokens.peek(token::RArrow) {
                Some((Op::RArrow, 2, 1))
            } else if tokens.peek(Ident) {
                Some((Op::App, 3, 4))
            } else if tokens.peek(token::Paren) {
                Some((Op::App, 3, 4))
            } else if tokens.peek(token::Tilde) {
                Some((Op::App, 3, 4))
            } else if tokens.peek(token::Brace) {
                Some((Op::App, 3, 4))
            } else {
                None
            }
        } {
            if l_bp < min_bp {
                // We are done parsing our subexpression, leave rest
                break;
            }

            // Consume arrow (if it's there)
            match token {
                Op::App => {}
                Op::RArrow => {
                    tokens.parse::<token::RArrow>()?;
                }
            }

            let rhs = expr_bp(tokens, r_bp)?;

            lhs = match token {
                Op::App => TermBuilder::App(Box::new(lhs), Box::new(rhs)),
                Op::RArrow => TermBuilder::Fun(Box::new(lhs), Box::new(rhs)),
            };

            continue;
        }

        break;
    }

    Ok(lhs)
}

// Lam |X: U, Y: U| X
fn parse_lam(tokens: ParseStream) -> Result<TermBuilder> {
    let mut binders = Vec::new();
    tokens.parse::<Token![|]>()?;

    while {
        let ident = tokens.parse::<Ident>()?;
        tokens.parse::<Token![:]>()?;
        let type_ = expr_bp(tokens, 0)?;

        binders.push((ident, type_));

        tokens.parse::<token::Comma>().is_ok()
    } {}

    tokens.parse::<Token![|]>()?;
    let body = expr_bp(tokens, 0)?;

    let mut expr = body;
    binders.reverse();

    for (ident, type_) in binders {
        expr = TermBuilder::Lam(ident, Box::new(type_), Box::new(expr));
    }
    Ok(expr)
}

fn parse_pi(tokens: ParseStream) -> Result<TermBuilder> {
    let mut binders = Vec::new();
    tokens.parse::<Token![|]>()?;

    while {
        let ident = tokens.parse::<Ident>()?;
        tokens.parse::<Token![:]>()?;
        let type_ = expr_bp(tokens, 0)?;

        binders.push((ident, type_));

        tokens.parse::<token::Comma>().is_ok()
    } {}

    tokens.parse::<Token![|]>()?;
    let body = expr_bp(tokens, 0)?;

    let mut expr = body;
    binders.reverse();

    for (ident, type_) in binders {
        expr = TermBuilder::Pi(ident, Box::new(type_), Box::new(expr));
    }
    Ok(expr)
}

fn parse_parens(tokens: ParseStream) -> Result<TermBuilder> {
    let content;
    parenthesized!(content in tokens);
    Ok(content.parse::<TermBuilder>()?)
}

fn parse_rust_expr(tokens: ParseStream) -> Result<TermBuilder> {
    let content;
    braced!(content in tokens);
    Ok(TermBuilder::RustExpr(content.parse::<Expr>()?))
}

mod test {
    use proc_macro2::TokenStream;
    use syn::parse2;

    use super::*;
    fn test_expand(text: &str, desired: TokenStream) {
        let tokens = text.parse::<TokenStream>().unwrap();
        let builder = parse2::<TermBuilder>(tokens).unwrap();
        let output = quote!( #builder );
        assert_eq!(output.to_string(), desired.to_string());
    }

    #[test]
    fn test_type() {
        test_expand(
            "U",
            quote! {
                U()
            },
        );
        test_expand(
            "Lam |T : U| T",
            quote! {{
                let T = U().var("T");
                T.lam(&T)
            }},
        );
        test_expand(
            "Pi |T : U| T",
            quote! {{
                let T = U().var("T");
                T.pi(&T)
            }},
        );
        test_expand(
            "Lam |T : U, t : T| t",
            quote! {{
                let T = U().var("T");
                T.lam( & {
                    let t = T.var("t");
                    t.lam(&t)
                })
            }},
        );
        test_expand(
            "Lam |A : U| A -> (A -> A) -> A",
            quote! {{
                let A = U().var("A");
                A.lam(& A.fun(&A.fun(&A).fun(&A)))
            }},
        );
        test_expand(
            "Lam |A : U, f : A -> A| f f f",
            quote! {{
                let A = U().var("A");
                A.lam(&{
                    let f = A.fun(&A).var("f");
                    f.lam(&f.app(&f).app(&f))
                })
            }},
        );
        test_expand(
            "Lam |A : U| A -> ~empty_type",
            quote! {{
                let A = U().var("A");
                A.lam(&A.fun(&empty_type()))
            }},
        );
        test_expand(
            "Lam |A : U| A -> empty_type",
            quote! {{
                let A = U().var("A");
                A.lam(&A.fun(&empty_type))
            }},
        );
        test_expand(
            "Lam |A : U| A -> U",
            quote! {{
                let A = U().var("A");
                A.lam(&A.fun(&U()))
            }},
        );
        test_expand(
            "Pi |A : U, P : A -> U| Lam |a : A| P a -> P a",
            quote! {{
                let A = U().var("A");
                A.pi(&{
                    let P = A.fun(&U()).var("P");
                    P.pi(&{
                        let a = A.var("a");
                        a.lam(&P.app(&a).fun(&P.app (& a)))
                    })
                })
            }},
        );
    }
}
