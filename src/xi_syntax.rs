/*  The syntax of Xi, which is calculus of construction, but based on judgments and not terms.
    We are going to construct 3 types of apply function, differ by their type checking:
    given func and elem, then we require is_type(func) is of the form Pi(var_type, expr), and that elem.is_type = var_type.
    The key point is that there are three ways to write equality: they are (extensionally) strictly equal, or their normalization is
    strictly equal, or a path between them, when viewed Judgment as a higher inductive type with internal equality imposed over
    beta-reduction and eta-conversion.
*/

use xi_semantics::syntax_to_semantics;

use crate::xi_semantics::{self, semantics_to_syntax};
#[derive(Clone, PartialEq, Eq, Debug)]

pub enum NatPrim {
    NatType,
    Nat(i32),
    Add,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Judgment {
    UInNone,
    Pi(Box<Judgment>, Box<Judgment>),
    Lam(Box<Judgment>, Box<Judgment>),
    BoundVar(u32, Box<Judgment>),
    Application(Box<Judgment>, Box<Judgment>), // we need to type check Application I think.
    Prim(NatPrim),
}

impl Judgment {
    /// This takes a Judgment and return its type. For example, (n : Nat).is_type = (Nat: U), (Nat:U).is_type = U:None
    pub fn type_of(&self) -> Option<Judgment> {
        match self {
            Judgment::UInNone => None,
            Judgment::Pi(_var_type, expr) => expr.type_of(),
            Judgment::Lam(var_type, expr) => {
                dbg!(expr);
                dbg!(expr.type_of());
                Some(Judgment::pi((**var_type).clone(), expr.type_of().unwrap()))
            }
            Judgment::BoundVar(_, var_type) => Some((**var_type).clone()),
            Judgment::Application(func, elem) => {
                let func_type = func.clone().type_of().unwrap().nbe();
                if let Judgment::Pi(func_arg_type, func_expr_type) = func_type {
                    // Some(Judgment::Application(
                    //     Box::new(Judgment::lam(*func_arg_type, *func_expr_type)),
                    //     elem.clone())),
                    return Some(Judgment::instantiate(*func_expr_type, *elem.clone()));
                // I am not sure if I want this or just instantiate(func_expr_type, elem).
                } else {
                    dbg!(func_type);
                    dbg!(self);
                    panic!("func.is_type is not a Pi type.")
                }
            }
            Judgment::Prim(prim) => Some(match prim {
                NatPrim::NatType => Judgment::u(),
                NatPrim::Nat(_) => Judgment::Prim(NatPrim::NatType),
                NatPrim::Add => Judgment::pi(
                    Judgment::Prim(NatPrim::NatType),
                    Judgment::pi(
                        Judgment::Prim(NatPrim::NatType),
                        Judgment::Prim(NatPrim::NatType),
                    ),
                ),
            }),
        }
    }

    /// Normalization to beta-reduced, eta-long form. beta-reduced means that app(lam(var_type,expr), elem) is beta-reduced to expr[BoundVar(?)\elem].
    pub fn nbe(self) -> Judgment {
        semantics_to_syntax(syntax_to_semantics(self, vec![]))
    }

    /// Pass through the normalization.
    fn is_normalized_type(self) -> Option<Judgment> {
        todo!()
    }

    /// U:None constructor
    pub fn u() -> Judgment {
        Judgment::UInNone
    }
    /// Pi constructor
    pub fn pi(var_type: Judgment, expr: Judgment) -> Judgment {
        let type_expr = expr.type_of();

        match type_expr {
            None => Judgment::Pi(Box::new(var_type), Box::new(expr)),
            Some(type_expr2) => match type_expr2.nbe() {
                Judgment::UInNone => Judgment::Pi(Box::new(var_type), Box::new(expr)),
                _ => panic!("pi type needs a type as expr"),
            },
        }
    }
    /// Lambda constructor
    pub fn lam(var_type: Judgment, expr: Judgment) -> Judgment {
        Judgment::Lam(Box::new(var_type), Box::new(expr))
    }
    /// BoundVar constructor
    pub fn boundvar(int: u32, var_type: Judgment) -> Judgment {
        Judgment::BoundVar(int, Box::new(var_type))
    }

    pub fn app_but_fucking_works(func: Judgment, elem: Judgment) -> Judgment {
        let func_type = func.clone().type_of().unwrap();
        if let Judgment::Pi(func_arg_type, _) = func_type {
            if *func_arg_type != elem.clone().type_of().unwrap() {
                panic!("elem and func's types doesn't match up")
            }
        } else {
            panic!("func is not a Pi type")
        }

        if let Judgment::Lam(_var_type, func_body) = func.clone() {
            let func_new_body = Judgment::instantiate(*func_body, elem.clone());
            if let Judgment::Application(new_func, new_arg) = func_new_body {
                Judgment::app_but_fucking_works(*new_func, *new_arg)
            } else {
                Judgment::Application(Box::new(func), Box::new(elem))
            }
        } else {
            Judgment::Application(Box::new(func), Box::new(elem))
        }
    }
    /// Application constructor with strict type check
    pub fn app_strict(func: Judgment, elem: Judgment) -> Judgment {
        Judgment::app_but_fucking_works(func, elem)
        // let func_type = func.clone().type_of().unwrap();
        // if let Judgment::Pi(func_arg_type, _) = func_type {
        //     if *func_arg_type == elem.clone().type_of().unwrap() {
        //         Judgment::Application(Box::new(func), Box::new(elem))
        //     } else {
        //         panic!("elem and func's types doesn't match up")
        //     }
        // } else {
        //     panic!("func is not a Pi type")
        // }
    }

    ///Application constructor with normalized type check
    pub fn app_normalize(func: Judgment, elem: Judgment) -> Judgment {
        Judgment::app_but_fucking_works(func, elem)
        // let func_type = dbg!(func.clone().type_of().unwrap().nbe());
        // if let Judgment::Pi(func_arg_type, _) = func_type {
        //     if (*func_arg_type).nbe() == elem.clone().type_of().unwrap().nbe() {
        //         Judgment::Application(Box::new(func), Box::new(elem))
        //     } else {
        //         panic!("elem and func's types doesn't match up")
        //     }
        // } else {
        //     panic!("func is not a Pi type")
        // }
    }

    pub fn app(func: Judgment, elem: Judgment) -> Judgment {
        Judgment::app_but_fucking_works(func, elem)

        // Judgment::Application(Box::new(func), Box::new(elem))
    }

    //Application constructor with a exhibit path to type check
    // fn app_normalize(func:Judgment, elem:Judgment, path: id(???, ???)) -> Judgment{

    // }

    ///Instantiate elem to expr by replacing the correct variable in expr with elem.
    pub fn instantiate(expr: Judgment, elem: Judgment) -> Judgment {
        fn instantiate_rec(expr: &Judgment, elem: &Judgment, depth: &u32) -> Judgment {
            match expr {
                Judgment::UInNone => Judgment::UInNone,
                Judgment::Pi(var_type, expr) => Judgment::pi(
                    instantiate_rec(var_type, elem, &(depth + &1)),
                    instantiate_rec(expr, elem, &(depth + &1)),
                ),
                Judgment::Lam(var_type, expr) => Judgment::lam(
                    instantiate_rec(var_type, elem, &(depth + &1)),
                    instantiate_rec(expr, elem, &(depth + &1)),
                ),

                Judgment::BoundVar(int, var_type) => {
                    if int < depth {
                        Judgment::boundvar(*int, instantiate_rec(var_type, elem, depth))
                    } else if int == depth {
                        if **var_type == elem.clone().type_of().unwrap() {
                            elem.clone()
                        } else {
                            panic!("The types of the elem and boundVar are wrong")
                        }
                    } else {
                        panic!("free variable present")
                    }
                }
                Judgment::Application(func, arg) => Judgment::Application(
                    Box::new(instantiate_rec(func, elem, depth)),
                    Box::new(instantiate_rec(arg, elem, depth)),
                ),
                Judgment::Prim(_) => expr.clone(),
            }
        }
        instantiate_rec(&expr, &elem, &0)
    }
}
