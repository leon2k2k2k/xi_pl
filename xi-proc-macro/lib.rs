use std::collections::BTreeMap;

use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{braced, bracketed, parenthesized, parse_macro_input, token, Expr, Ident, Result, Token};

#[proc_macro]
pub fn term(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let builder = parse_macro_input!(tokens as TermBuilderFree);
    (quote! {{ #[allow(non_snake_case)] { #builder } }}).into()
}

#[derive(Clone, Debug)]
struct TermBuilderFree {
    free_vars: Vec<(Ident, TermBuilder)>,
    term: TermBuilder,
}
#[derive(Clone, Debug)]
enum TermBuilder {
    Type,
    Var(Ident),
    RustExpr(Expr),
    Prim(Expr),
    Lam(Ident, Box<TermBuilder>, Box<TermBuilder>),
    Pi(Ident, Box<TermBuilder>, Box<TermBuilder>),
    Fun(Box<TermBuilder>, Box<TermBuilder>),
    App(Box<TermBuilder>, Box<TermBuilder>),
}

impl ToTokens for TermBuilderFree {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        fn to_tokens_rec(
            free_vars: &BTreeMap<Ident, TermBuilder>,
            term: &TermBuilder,
        ) -> proc_macro2::TokenStream {
            match term {
                TermBuilder::Type => quote! { Judgment::u(None) },
                TermBuilder::Var(ident) => {
                    let var_type = free_vars
                        .get(ident)
                        .expect(&format!("Variable {} not bound", ident));

                    let var_type_tokens = to_tokens_rec(free_vars, var_type);
                    quote! {
                        Judgment::free(#ident, #var_type_tokens, None)
                    }
                }
                TermBuilder::RustExpr(expr) => quote! { #expr.clone() },
                TermBuilder::Prim(prim) => quote! { Judgment::prim(#prim, None) },
                TermBuilder::Lam(ident, type_, body) => {
                    let type_tokens = to_tokens_rec(free_vars, type_);
                    let mut new_vars = free_vars.clone();
                    new_vars.insert(ident.clone(), *type_.clone());
                    let body_tokens = to_tokens_rec(&new_vars, body);
                    quote! {{
                        let #ident = VarUuid::new();
                        Judgment::lam(#type_tokens, Judgment::rebind(#body_tokens, #ident), None)
                    }}
                }
                TermBuilder::Pi(ident, type_, body) => {
                    let type_tokens = to_tokens_rec(free_vars, type_);
                    let mut new_vars = free_vars.clone();
                    new_vars.insert(ident.clone(), *type_.clone());
                    let body_tokens = to_tokens_rec(&new_vars, body);
                    quote! {{
                        let #ident = VarUuid::new();
                        Judgment::pi(#type_tokens, Judgment::rebind(#body_tokens, #ident), None)
                    }}
                }
                TermBuilder::Fun(domain, codomain) => {
                    let domain_tokens = to_tokens_rec(free_vars, domain);
                    let codomain_tokens = to_tokens_rec(free_vars, codomain);
                    quote! {Judgment::pi(#domain_tokens, #codomain_tokens, None)}
                }
                TermBuilder::App(fun, arg) => {
                    let func_tokens = to_tokens_rec(free_vars, fun);
                    let arg_tokens = to_tokens_rec(free_vars, arg);
                    quote! { Judgment::app(#func_tokens, #arg_tokens, None)}
                }
            }
        }
        let mut free_vars = BTreeMap::new();
        let mut free_var_bindings = vec![];

        for (bind_ident, bind_type) in &self.free_vars {
            free_vars.insert(bind_ident.clone(), bind_type.clone());
            free_var_bindings.push(quote! { let #bind_ident = VarUuid::new(); });
        }
        let body_tokens = to_tokens_rec(&free_vars, &self.term);

        if free_vars.is_empty() {
            tokens.extend(body_tokens);
        } else if free_vars.len() == 1 {
            let free_var_name = self.free_vars[0].0.clone();
            tokens.extend(quote! {{#(#free_var_bindings)* (#free_var_name, #body_tokens)}})
        } else {
            let free_var_names = self.free_vars.iter().map(|x| &x.0);
            tokens.extend(quote! {{#(#free_var_bindings)* ((#(#free_var_names),*) , #body_tokens)}})
        }
    }
}

// parser implementation based off
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

impl Parse for TermBuilderFree {
    fn parse(tokens: ParseStream) -> Result<TermBuilderFree> {
        let should_parse_binders = tokens.peek(Token![|]);
        let free_vars = if should_parse_binders {
            parse_binders(tokens)?
        } else {
            vec![]
        };
        let term = tokens.parse::<TermBuilder>()?;
        Ok(TermBuilderFree { free_vars, term })
    }
}

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
    } else if tokens.peek(token::Bracket) {
        parse_bracket(tokens)?
    } else if tokens.peek(token::Brace) {
        parse_rust_expr(tokens)?
    } else {
        return Err(tokens.error("Unexpected token {}"));
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
            } else if tokens.peek(token::Bracket) {
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

fn parse_binders(tokens: ParseStream) -> Result<Vec<(Ident, TermBuilder)>> {
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
    Ok(binders)
}

// lam |X: U, Y: U| X
fn parse_lam(tokens: ParseStream) -> Result<TermBuilder> {
    let mut binders = parse_binders(tokens)?;
    let mut body = expr_bp(tokens, 0)?;

    binders.reverse();

    for (ident, type_) in binders {
        body = TermBuilder::Lam(ident, Box::new(type_), Box::new(body));
    }
    Ok(body)
}

fn parse_pi(tokens: ParseStream) -> Result<TermBuilder> {
    let mut binders = parse_binders(tokens)?;
    let mut body = expr_bp(tokens, 0)?;

    binders.reverse();

    for (ident, type_) in binders {
        body = TermBuilder::Pi(ident, Box::new(type_), Box::new(body));
    }
    Ok(body)
}

fn parse_parens(tokens: ParseStream) -> Result<TermBuilder> {
    let content;
    parenthesized!(content in tokens);
    Ok(content.parse::<TermBuilder>()?)
}

fn parse_bracket(tokens: ParseStream) -> Result<TermBuilder> {
    let content;
    bracketed!(content in tokens);
    Ok(TermBuilder::Prim(content.parse::<Expr>()?))
}

fn parse_rust_expr(tokens: ParseStream) -> Result<TermBuilder> {
    let content;
    braced!(content in tokens);
    Ok(TermBuilder::RustExpr(content.parse::<Expr>()?))
}

mod test {

    #[cfg(test)]
    fn test_expand(text: &str, desired: proc_macro2::TokenStream) {
        use super::*;
        use syn::parse2;

        let tokens = text.parse::<proc_macro2::TokenStream>().unwrap();
        let builder = parse2::<TermBuilderFree>(tokens).unwrap();
        let output = quote!( #builder );
        assert_eq!(output.to_string(), desired.to_string());
    }

    #[test]
    fn test_type() {
        use super::*;

        test_expand(
            "U",
            quote! {
                Judgment::u()
            },
        );
        test_expand(
            "Lam |T : U| T",
            quote! {{
                let T = VarUuid::new();
                Judgment::lam(
                    Judgment::u(),
                    Judgment::rebind(Judgment::free(T, Judgment::u()), T)
                )
            }},
        );
        test_expand(
            "Pi |T : U| T",
            quote! {{
                let T = VarUuid::new();
                Judgment::pi(
                    Judgment::u(),
                    Judgment::rebind(Judgment::free(T, Judgment::u()), T)
                )
            }},
        );
        test_expand(
            "Lam |T : U, t : T| t",
            quote! {{
                let T = VarUuid::new();
                Judgment::lam(
                    Judgment::u(),
                    Judgment::rebind(
                        {
                            let t = VarUuid::new();
                            Judgment::lam(
                                Judgment::free(T, Judgment::u()),
                                Judgment::rebind(
                                    Judgment::free(t, Judgment::free(T, Judgment::u())),
                                    t
                                )
                            )
                        },
                        T
                    )
                )
            }},
        );
        test_expand(
            "Lam |A : U| A -> (A -> A) -> A",
            quote! {{
                let A = VarUuid::new();
                Judgment::lam(
                    Judgment::u(),
                    Judgment::rebind(
                        Judgment::pi(
                            Judgment::free(A, Judgment::u()),
                            Judgment::pi(
                                Judgment::pi(
                                    Judgment::free(A, Judgment::u()),
                                    Judgment::free(A, Judgment::u())
                                ),
                                Judgment::free(A, Judgment::u())
                            )
                        ),
                        A
                    )
                )
            }},
        );
        test_expand(
            "Lam |A : U, f : A -> A| f f f",
            quote! {{
                let A = VarUuid::new();
                Judgment::lam(
                    Judgment::u(),
                    Judgment::rebind(
                        {
                            let f = VarUuid::new();
                            Judgment::lam(
                                Judgment::pi(
                                    Judgment::free(A, Judgment::u()),
                                    Judgment::free(A, Judgment::u())
                                ),
                                Judgment::rebind(
                                    Judgment::app(
                                        Judgment::app(
                                            Judgment::free(
                                                f,
                                                Judgment::pi(
                                                    Judgment::free(A, Judgment::u()),
                                                    Judgment::free(A, Judgment::u())
                                                )
                                            ),
                                            Judgment::free(
                                                f,
                                                Judgment::pi(
                                                    Judgment::free(A, Judgment::u()),
                                                    Judgment::free(A, Judgment::u())
                                                )
                                            )
                                        ),
                                        Judgment::free(
                                            f,
                                            Judgment::pi(
                                                Judgment::free(A, Judgment::u()),
                                                Judgment::free(A, Judgment::u())
                                            )
                                        )
                                    ),
                                    f
                                )
                            )
                        },
                        A
                    )
                )
            }},
        );
        test_expand("Lam |A : U| A -> {empty_type}", {
            quote! {{
                let A = VarUuid::new();
                Judgment::lam(
                    Judgment::u(),
                    Judgment::rebind(
                        Judgment::pi(Judgment::free(A, Judgment::u()), empty_type.clone()),
                        A
                    )
                )
            }}
        });
        test_expand(
            "Lam |A : U| A -> U",
            quote! {{
                let A = VarUuid::new();
                Judgment::lam(
                    Judgment::u(),
                    Judgment::rebind(
                        Judgment::pi(Judgment::free(A, Judgment::u()), Judgment::u()),
                        A
                    )
                )
            }},
        );
        test_expand(
            "Pi |A : U, P : A -> U| Lam |a : A| P a -> P a",
            quote! {{
                let A = VarUuid::new();
                Judgment::pi(
                    Judgment::u(),
                    Judgment::rebind(
                        {
                            let P = VarUuid::new();
                            Judgment::pi(
                                Judgment::pi(Judgment::free(A, Judgment::u()), Judgment::u()),
                                Judgment::rebind(
                                    {
                                        let a = VarUuid::new();
                                        Judgment::lam(
                                            Judgment::free(A, Judgment::u()),
                                            Judgment::rebind(
                                                Judgment::pi(
                                                    Judgment::app(
                                                        Judgment::free(
                                                            P,
                                                            Judgment::pi(
                                                                Judgment::free(A, Judgment::u()),
                                                                Judgment::u()
                                                            )
                                                        ),
                                                        Judgment::free(
                                                            a,
                                                            Judgment::free(A, Judgment::u())
                                                        )
                                                    ),
                                                    Judgment::app(
                                                        Judgment::free(
                                                            P,
                                                            Judgment::pi(
                                                                Judgment::free(A, Judgment::u()),
                                                                Judgment::u()
                                                            )
                                                        ),
                                                        Judgment::free(
                                                            a,
                                                            Judgment::free(A, Judgment::u())
                                                        )
                                                    )
                                                ),
                                                a
                                            )
                                        )
                                    },
                                    P
                                )
                            )
                        },
                        A
                    )
                )
            }},
        );
        test_expand(
            "{a} {b}",
            quote! {
                Judgment::app(a.clone(), b.clone())
            },
        );
        test_expand(
            "[a] [b]",
            quote! {
                Judgment::app(Judgment::prim(a), Judgment::prim(b))
            },
        );
        test_expand(
            "|T : U| T",
            quote! {{
                let T = VarUuid::new();
                (T, Judgment::free(T, Judgment::u()))
            }},
        );
        test_expand(
            "|T : U, S : U| T -> U",
            quote! {{
                let T = VarUuid::new();
                let S = VarUuid::new();
                ((T, S), Judgment::pi(Judgment::Free(T, Judgment::u()), Judgment::u()))
            }},
        );
    }
}
