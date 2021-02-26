/*  The syntax of Xi, which is calculus of construction, but based on judgments and not terms.
    We are going to construct 3 types of apply function, differ by their type checking:
    given func and elem, then we require is_type(func) is of the form Pi(var_type, expr), and that elem.is_type = var_type.
    The key point is that there are three ways to write equality: they are (extensionally) strictly equal, or their normalization is
    strictly equal, or a path between them, when viewed Judgment as a higher inductive type with internal equality imposed over
    beta-reduction and eta-conversion.
*/

use crate::nbe::{SJudgment, Semantics};
use free_var::FreeVar;
use std::fmt::Debug;

// #[derive(Clone, Debug)]
// enum TypeCheckError {
//     location: Span,

// }

pub trait Primitive
where
    Self: Sized,
{
    fn type_of(&self) -> Judgment<Self>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Judgment<T> {
    UInNone,
    Prim(T),
    FreeVar(FreeVar, Box<Judgment<T>>),
    Pi(Box<Judgment<T>>, Box<Judgment<T>>),
    Lam(Box<Judgment<T>>, Box<Judgment<T>>),
    BoundVar(u32, Box<Judgment<T>>),
    Application(Box<Judgment<T>>, Box<Judgment<T>>),
    // Metadata(String, Judgment<T>),
}

impl<T: Primitive + Clone + PartialEq + Eq + 'static + std::fmt::Debug> Judgment<T> {
    /// Takes a judgment and returns its the judgment representing its type
    pub fn type_of(&self) -> Option<Judgment<T>> {
        match self {
            Judgment::UInNone => None,
            Judgment::Pi(_var_type, expr) => expr.type_of(),
            Judgment::Lam(var_type, expr) => {
                Some(Judgment::pi((**var_type).clone(), expr.type_of().unwrap()))
            }
            Judgment::BoundVar(_, var_type) => Some((**var_type).clone()),
            Judgment::Application(func, elem) => {
                if let Some(Judgment::Pi(_func_arg_type, func_expr_type)) = func.type_of() {
                    let new_result_type = Judgment::instantiate(*func_expr_type, *elem.clone());
                    Some(new_result_type.normalize())
                } else {
                    panic!("type of func should be a Pi")
                }
            }
            Judgment::Prim(prim) => Some(prim.type_of().into()),
            Judgment::FreeVar(_free_var, var_type) => Some((**var_type).clone()),
        }
    }

    pub fn normalize(self) -> Judgment<T> {
        self.nbe()
    }

    /// U:None constructor
    pub fn u() -> Judgment<T> {
        Judgment::UInNone
    }
    /// Pi constructor
    pub fn pi(var_type: Judgment<T>, expr: Judgment<T>) -> Judgment<T> {
        let type_expr = expr.type_of();

        match type_expr {
            None => Judgment::Pi(Box::new(var_type), Box::new(expr)),
            Some(type_expr2) => match type_expr2 {
                Judgment::UInNone => Judgment::Pi(Box::new(var_type), Box::new(expr)),
                _ => panic!("pi type needs a type as expr"),
            },
        }
    }
    /// Lambda constructor
    pub fn lam(var_type: Judgment<T>, expr: Judgment<T>) -> Judgment<T> {
        Judgment::Lam(Box::new(var_type), Box::new(expr))
    }
    /// BoundVar constructor
    pub fn var(int: u32, var_type: Judgment<T>) -> Judgment<T> {
        Judgment::BoundVar(int, Box::new(var_type))
    }

    pub fn free(int: FreeVar, var_type: Judgment<T>) -> Judgment<T> {
        Judgment::FreeVar(int, Box::new(var_type))
    }

    pub fn prim(prim: T) -> Judgment<T> {
        Judgment::Prim(prim)
    }

    pub fn app(func: Judgment<T>, elem: Judgment<T>) -> Judgment<T> {
        let func_type = func.clone().type_of().unwrap();
        if let Judgment::Pi(func_arg_type, _) = func_type {
            let elem_type = elem.clone().type_of().unwrap();

            if *func_arg_type != elem_type {
                panic!(
                    "elem and func's types doesn't match up\nfunc_arg_type: {:?}\nelem_type: {:?}
                    \nfunc: {:?} \nelem: {:?}",
                    func_arg_type, elem_type, func, elem,
                )
            }
        } else {
            panic!("func is not a Pi type")
        }

        if let Judgment::Lam(_var_type, func_body) = func.clone() {
            let new_body = Judgment::instantiate(*func_body, elem.clone());
            new_body.normalize()
        } else {
            Judgment::Application(Box::new(func), Box::new(elem))
        }
    }

    /// Replace the outermost bound variable in expr with elem.
    fn instantiate(expr: Judgment<T>, elem: Judgment<T>) -> Judgment<T> {
        fn instantiate_rec<T: Primitive + Clone + PartialEq + Eq + 'static + Debug>(
            expr: &Judgment<T>,
            elem: &Judgment<T>,
            depth: u32,
        ) -> Judgment<T> {
            match expr {
                Judgment::UInNone => Judgment::UInNone,
                Judgment::Pi(var_type, expr) => Judgment::pi(
                    instantiate_rec(var_type, elem, depth + 1),
                    instantiate_rec(expr, elem, depth + 1),
                ),
                Judgment::Lam(var_type, expr) => Judgment::lam(
                    instantiate_rec(var_type, elem, depth + 1),
                    instantiate_rec(expr, elem, depth + 1),
                ),

                Judgment::BoundVar(int, var_type) => {
                    if *int < depth {
                        Judgment::var(*int, instantiate_rec(var_type, elem, depth))
                    } else if *int == depth {
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
                Judgment::FreeVar(free_var, var_type) => {
                    Judgment::FreeVar(*free_var, var_type.clone())
                }
            }
        }
        instantiate_rec(&expr, &elem, 0)
    }

    pub fn rebind(s: Judgment<T>, free_var: FreeVar) -> Judgment<T> {
        fn rebind_rec<T: Primitive + Clone + PartialEq + Eq + 'static + Debug>(
            s: Judgment<T>,
            free_var: FreeVar,
            depth: u32,
        ) -> Judgment<T> {
            match s {
                Judgment::UInNone => Judgment::u(),
                Judgment::Pi(var_type, expr) => Judgment::pi(
                    rebind_rec(*var_type, free_var, depth + 1),
                    rebind_rec(*expr, free_var, depth + 1),
                ),
                Judgment::Lam(var_type, expr) => Judgment::lam(
                    rebind_rec(*var_type, free_var, depth + 1),
                    rebind_rec(*expr, free_var, depth + 1),
                ),
                Judgment::BoundVar(i, var_type) => {
                    Judgment::var(i, rebind_rec(*var_type, free_var, depth))
                }
                Judgment::Application(lhs, rhs) => Judgment::app(
                    rebind_rec(*lhs, free_var, depth),
                    rebind_rec(*rhs, free_var, depth),
                ),
                Judgment::Prim(_) => s,
                Judgment::FreeVar(i, var_type) => {
                    if i == free_var {
                        Judgment::var(depth, *var_type)
                    } else {
                        Judgment::free(i, rebind_rec(*var_type, free_var, depth))
                    }
                }
            }
        }
        rebind_rec(s, free_var, 0)
    }

    pub fn app_unchecked(func: Judgment<T>, arg: Judgment<T>) -> Judgment<T> {
        Judgment::Application(Box::new(func), Box::new(arg))
    }

    pub fn pi_unchecked(var_type: Judgment<T>, expr: Judgment<T>) -> Judgment<T> {
        Judgment::Pi(Box::new(var_type), Box::new(expr))
    }

    /// Normalization to beta-reduced, eta-long form. beta-reduced means that app(lam(var_type,expr), elem) is beta-reduced to expr[BoundVar(?)\elem].
    pub fn nbe<U: Semantics<T> + Primitive + Clone + PartialEq + Eq + 'static + Debug>(
        self,
    ) -> Judgment<U> {
        SJudgment::semantics_to_syntax(SJudgment::syntax_to_semantics(self, vec![]))
    }

    pub fn is_outermost_bound_var_used(self) -> bool {
        fn is_bound_var_used<T>(expr: Judgment<T>, depth: u32) -> bool {
            match expr {
                Judgment::UInNone => false,
                Judgment::Prim(_) => false,
                Judgment::FreeVar(_, expr) => is_bound_var_used(*expr, depth),
                Judgment::Pi(var_type, expr) => {
                    is_bound_var_used(*var_type, depth + 1) || is_bound_var_used(*expr, depth + 1)
                }
                Judgment::Lam(var_type, expr) => {
                    is_bound_var_used(*var_type, depth + 1) || is_bound_var_used(*expr, depth + 1)
                }
                Judgment::BoundVar(int, expr) => int == depth || is_bound_var_used(*expr, depth),
                Judgment::Application(func, elem) => {
                    is_bound_var_used(*func, depth) || is_bound_var_used(*elem, depth)
                }
            }
        }
        is_bound_var_used(self, 0)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum NatPrim {
    NatType,
    Nat(i32),
    Add,
}

impl Primitive for NatPrim {
    fn type_of(&self) -> Judgment<Self> {
        match self {
            NatPrim::NatType => Judgment::u(),
            NatPrim::Nat(_) => Judgment::Prim(NatPrim::NatType),
            NatPrim::Add => Judgment::pi(
                Judgment::Prim(NatPrim::NatType),
                Judgment::pi(
                    Judgment::Prim(NatPrim::NatType),
                    Judgment::Prim(NatPrim::NatType),
                ),
            ),
        }
    }
}

impl Primitive for () {
    fn type_of(&self) -> Judgment<Self> {
        Judgment::u()
    }
}
