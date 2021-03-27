/*  The syntax of Xi, which is calculus of construction, but based on judgments and not terms.
    We are going to construct 3 types of apply function, differ by their type checking:
    given func and elem, then we require is_type(func) is of the form Pi(var_type, expr), and that elem.is_type = var_type.
    The key point is that there are three ways to write equality: they are (extensionally) strictly equal, or their normalization is
    strictly equal, or a path between them, when viewed Judgment as a higher inductive type with internal equality imposed over
    beta-reduction and eta-conversion.
*/

use std::rc::Rc;

use crate::nbe::{SJudgment, Semantics};
use xi_uuid::VarUuid;
// #[derive(Clone, Debug)]
// enum TypeCheckError {
//     location: Span,

// }

#[derive(Clone)]
pub struct Judgment<T, S> {
    pub metadata: S,
    pub tree: JudgmentKind<T, S>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum JudgmentKind<T, S> {
    UInNone,
    Prim(T),
    FreeVar(VarUuid, Box<Judgment<T, S>>),
    Pi(Box<Judgment<T, S>>, Box<Judgment<T, S>>),
    Lam(Box<Judgment<T, S>>, Box<Judgment<T, S>>),
    BoundVar(u32, Box<Judgment<T, S>>),
    Application(Box<Judgment<T, S>>, Box<Judgment<T, S>>),
}

impl<T: PartialEq, S: PartialEq> PartialEq for Judgment<T, S> {
    fn eq(&self, other: &Judgment<T, S>) -> bool {
        self.tree == other.tree
    }
}

impl<T: PartialEq, S: PartialEq> Eq for Judgment<T, S> {}

pub trait Primitive: Clone + PartialEq + Eq + 'static + std::fmt::Debug
where
    Self: Sized,
{
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S>;
}
pub trait Metadata: Clone + PartialEq + Eq + 'static + std::fmt::Debug + Default {}

impl<T: Primitive, S: Metadata> Judgment<T, S> {
    /// Takes a judgment and returns its the judgment representing its type
    pub fn type_of(&self) -> Option<Judgment<T, S>> {
        match self.tree.clone() {
            JudgmentKind::UInNone => None,
            JudgmentKind::Pi(_var_type, expr) => expr.type_of(),
            JudgmentKind::Lam(var_type, expr) => Some(Judgment::pi(
                (*var_type).clone(),
                expr.type_of().unwrap(),
                None,
            )),
            JudgmentKind::BoundVar(_, var_type) => Some((*var_type).clone()),
            JudgmentKind::Application(func, elem) => {
                if let Some(Judgment {
                    tree: JudgmentKind::Pi(_func_arg_type, func_expr_type),
                    ..
                }) = func.type_of()
                {
                    let new_result_type = Judgment::instantiate(*func_expr_type, &*elem.clone());
                    Some(new_result_type)
                // Some(new_result_type.normalize())
                } else {
                    panic!("type of func should be a Pi")
                }
            }
            JudgmentKind::Prim(prim) => Some(prim.type_of().into()),
            JudgmentKind::FreeVar(_free_var, var_type) => Some((*var_type).clone()),
        }
    }

    pub fn normalize(self) -> Judgment<T, S> {
        self.nbe()
    }

    /// U:None constructor
    pub fn u(metadata: Option<S>) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::UInNone,
            metadata: metadata.unwrap_or_default(),
        }
    }
    /// Pi constructor
    pub fn pi(
        var_type: Judgment<T, S>,
        expr: Judgment<T, S>,
        metadata: Option<S>,
    ) -> Judgment<T, S> {
        let type_expr = expr.type_of();
        if let Some(type_expr_) = type_expr {
            if type_expr_.tree != JudgmentKind::UInNone {
                panic!("pi type needs a type as expr");
            }
        }

        Judgment {
            tree: JudgmentKind::Pi(Box::new(var_type), Box::new(expr)),
            metadata: metadata.unwrap_or_default(),
        }
    }
    /// Lambda constructor
    pub fn lam(
        var_type: Judgment<T, S>,
        expr: Judgment<T, S>,
        metadata: Option<S>,
    ) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::Lam(Box::new(var_type), Box::new(expr)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn bound_var(int: u32, var_type: Judgment<T, S>, metadata: Option<S>) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::BoundVar(int, Box::new(var_type)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn free(int: VarUuid, var_type: Judgment<T, S>, metadata: Option<S>) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::FreeVar(int, Box::new(var_type)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn prim(prim: T, metadata: Option<S>) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::Prim(prim),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn app(func: Judgment<T, S>, elem: Judgment<T, S>, metadata: Option<S>) -> Judgment<T, S> {
        let func_type = func.clone().type_of().unwrap().normalize();

        if let JudgmentKind::Pi(func_arg_type, _) = func_type.tree {
            let elem_type = elem.clone().type_of().unwrap().normalize();

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

        Judgment {
            tree: JudgmentKind::Application(Box::new(func), Box::new(elem)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    /// Replace the outermost bound variable in expr with elem.
    pub fn instantiate(expr: Judgment<T, S>, elem: &Judgment<T, S>) -> Judgment<T, S> {
        fn instantiate_rec<T: Primitive, S: Metadata>(
            expr: Judgment<T, S>,
            elem: &Judgment<T, S>,
            depth: u32,
        ) -> Judgment<T, S> {
            let result_tree = match expr.tree.clone() {
                JudgmentKind::UInNone => JudgmentKind::UInNone,
                JudgmentKind::Pi(var_type, expr) => JudgmentKind::Pi(
                    Box::new(instantiate_rec(*var_type, elem, depth + 1)),
                    Box::new(instantiate_rec(*expr, elem, depth + 1)),
                ),
                JudgmentKind::Lam(var_type, expr) => JudgmentKind::Lam(
                    Box::new(instantiate_rec(*var_type, elem, depth + 1)),
                    Box::new(instantiate_rec(*expr, elem, depth + 1)),
                ),

                JudgmentKind::BoundVar(index, var_type) => {
                    if index != depth {
                        JudgmentKind::BoundVar(
                            index,
                            Box::new(instantiate_rec(*var_type, elem, depth)),
                        )
                    } else {
                        if *var_type == elem.clone().type_of().unwrap() {
                            // Note: early return!
                            return elem.clone();
                        } else {
                            panic!("The types of the elem and boundVar are wrong")
                        }
                    }
                }
                JudgmentKind::Application(func, arg) => JudgmentKind::Application(
                    Box::new(instantiate_rec(*func, elem, depth)),
                    Box::new(instantiate_rec(*arg, elem, depth)),
                ),
                JudgmentKind::Prim(_) => expr.tree.clone(),
                JudgmentKind::FreeVar(free_var, var_type) => {
                    JudgmentKind::FreeVar(free_var, var_type.clone())
                }
            };

            Judgment {
                tree: result_tree,
                metadata: expr.metadata,
            }
        }
        instantiate_rec(expr, elem, 0)
    }

    pub fn rebind(s: Judgment<T, S>, free_var: VarUuid) -> Judgment<T, S> {
        fn rebind_rec<T: Primitive, S: Metadata>(
            s: Judgment<T, S>,
            free_var: VarUuid,
            depth: u32,
        ) -> Judgment<T, S> {
            let result_tree = match s.tree {
                JudgmentKind::UInNone => JudgmentKind::UInNone,
                JudgmentKind::Pi(var_type, expr) => JudgmentKind::Pi(
                    Box::new(rebind_rec(*var_type, free_var, depth + 1)),
                    Box::new(rebind_rec(*expr, free_var, depth + 1)),
                ),
                JudgmentKind::Lam(var_type, expr) => JudgmentKind::Lam(
                    Box::new(rebind_rec(*var_type, free_var, depth + 1)),
                    Box::new(rebind_rec(*expr, free_var, depth + 1)),
                ),
                JudgmentKind::BoundVar(i, var_type) => {
                    JudgmentKind::BoundVar(i, Box::new(rebind_rec(*var_type, free_var, depth)))
                }
                JudgmentKind::Application(lhs, rhs) => JudgmentKind::Application(
                    Box::new(rebind_rec(*lhs, free_var, depth)),
                    Box::new(rebind_rec(*rhs, free_var, depth)),
                ),
                JudgmentKind::Prim(_) => s.tree.clone(),
                JudgmentKind::FreeVar(i, var_type) => {
                    if i == free_var {
                        JudgmentKind::BoundVar(depth, Box::new(*var_type))
                    } else {
                        JudgmentKind::FreeVar(i, Box::new(rebind_rec(*var_type, free_var, depth)))
                    }
                }
            };

            Judgment {
                tree: result_tree,
                metadata: s.metadata,
            }
        }
        rebind_rec(s, free_var, 0)
    }

    pub fn app_unchecked(
        func: Judgment<T, S>,
        arg: Judgment<T, S>,
        metadata: Option<S>,
    ) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::Application(Box::new(func), Box::new(arg)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn app_unchecked_vec(
        func: Judgment<T, S>,
        args: &mut Vec<Judgment<T, S>>,
        metadata: Option<S>,
    ) -> Judgment<T, S> {
        if args.len() == 0 {
            panic!("empty arg")
        } else if args.len() == 1 {
            Judgment::app_unchecked(func, args.pop().unwrap(), None)
        } else {
            let arg = args.pop();
            Judgment::app_unchecked(
                Judgment::app_unchecked_vec(func, args, metadata),
                arg.unwrap(),
                None,
            )
        }
    }
    pub fn pi_unchecked(
        var_type: Judgment<T, S>,
        expr: Judgment<T, S>,
        metadata: Option<S>,
    ) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::Pi(Box::new(var_type), Box::new(expr)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    /// Normalization to beta-reduced, eta-long form. beta-reduced means that app(lam(var_type,expr), elem) is beta-reduced to expr[BoundVar(?)\elem].
    pub fn nbe<U: Primitive + Semantics<T>>(self) -> Judgment<U, S> {
        SJudgment::semantics_to_syntax(SJudgment::syntax_to_semantics(self, vec![]))
    }

    pub fn is_outermost_bound_var_used(self) -> bool {
        fn is_bound_var_used<T, S>(expr: Judgment<T, S>, depth: u32) -> bool {
            match expr.tree {
                JudgmentKind::UInNone => false,
                JudgmentKind::Prim(_) => false,
                JudgmentKind::FreeVar(_, expr) => is_bound_var_used(*expr, depth),
                JudgmentKind::Pi(var_type, expr) => {
                    is_bound_var_used(*var_type, depth + 1) || is_bound_var_used(*expr, depth + 1)
                }
                JudgmentKind::Lam(var_type, expr) => {
                    is_bound_var_used(*var_type, depth + 1) || is_bound_var_used(*expr, depth + 1)
                }
                JudgmentKind::BoundVar(int, expr) => {
                    int == depth || is_bound_var_used(*expr, depth)
                }
                JudgmentKind::Application(func, elem) => {
                    is_bound_var_used(*func, depth) || is_bound_var_used(*elem, depth)
                }
            }
        }
        is_bound_var_used(self, 0)
    }

    pub fn define_prim<U: Primitive>(
        &self,
        prim_meaning: Rc<dyn Fn(T) -> Judgment<U, S>>,
    ) -> Judgment<U, S> {
        let metadata = Some(self.metadata.clone());
        match &self.tree {
            JudgmentKind::UInNone => Judgment::u(metadata),
            JudgmentKind::Prim(prim) => prim_meaning(prim.clone()),
            JudgmentKind::FreeVar(index, var_type) => {
                Judgment::free(*index, var_type.define_prim(prim_meaning), metadata)
            }
            JudgmentKind::Pi(var_type, body) => Judgment::pi(
                var_type.define_prim(prim_meaning.clone()),
                body.define_prim(prim_meaning),
                metadata,
            ),
            JudgmentKind::Lam(var_type, body) => Judgment::lam(
                var_type.define_prim(prim_meaning.clone()),
                body.define_prim(prim_meaning),
                metadata,
            ),
            JudgmentKind::BoundVar(index, var_type) => {
                Judgment::bound_var(*index, var_type.define_prim(prim_meaning), metadata)
            }
            JudgmentKind::Application(lhs, rhs) => Judgment::app_unchecked(
                lhs.define_prim(prim_meaning.clone()),
                rhs.define_prim(prim_meaning),
                metadata,
            ),
        }
    }

    pub fn cast_metadata(expr: Judgment<T, ()>) -> Judgment<T, S> {
        match expr.tree {
            JudgmentKind::UInNone => Judgment::u(None),
            JudgmentKind::Prim(prim) => Judgment::prim(prim, None),
            JudgmentKind::FreeVar(index, var_type) => {
                Judgment::free(index, Judgment::cast_metadata(*var_type), None)
            }
            JudgmentKind::Pi(var_type, body) => Judgment::pi(
                Judgment::cast_metadata(*var_type),
                Judgment::cast_metadata(*body),
                None,
            ),
            JudgmentKind::Lam(var_type, body) => Judgment::lam(
                Judgment::cast_metadata(*var_type),
                Judgment::cast_metadata(*body),
                None,
            ),
            JudgmentKind::BoundVar(index, var_type) => {
                Judgment::bound_var(index, Judgment::cast_metadata(*var_type), None)
            }
            JudgmentKind::Application(lhs, rhs) => Judgment::app(
                Judgment::cast_metadata(*lhs),
                Judgment::cast_metadata(*rhs),
                None,
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum NatPrim {
    NatType,
    Nat(i32),
    Add,
}
impl Primitive for NatPrim {
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S> {
        use xi_proc_macro::term;
        match self {
            NatPrim::NatType => term!(U),
            NatPrim::Nat(_) => term!([NatPrim::NatType]),
            NatPrim::Add => term!([NatPrim::NatType] -> [NatPrim::NatType] -> [NatPrim::NatType]),
        }
    }
}

impl Primitive for () {
    fn type_of<S: Metadata>(&self) -> Judgment<Self, S> {
        Judgment::u(None)
    }
}

impl Metadata for () {}
