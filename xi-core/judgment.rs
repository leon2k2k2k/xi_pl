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
    Type,
    Prim(T, Box<Judgment<T, S>>),
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
    fn maybe_prim_type<S: Metadata>(&self) -> Option<Judgment<Self, S>>;
}
pub trait Metadata: Clone + PartialEq + Eq + 'static + std::fmt::Debug + Default {}

impl<T: Primitive, S: Metadata> Judgment<T, S> {
    /// Takes a judgment and returns its the judgment representing its type
    pub fn type_of(&self) -> Option<Judgment<T, S>> {
        match self.tree.clone() {
            JudgmentKind::Type => None,
            JudgmentKind::Pi(_var_type, expr) => expr.type_of(),
            JudgmentKind::Lam(var_type, expr) => Some(Judgment::pi(
                (*var_type).clone(),
                expr.type_of().unwrap(),
                None,
            )),
            JudgmentKind::BoundVar(index, var_type) => Some((*var_type).clone()),
            JudgmentKind::Application(func, elem) => {
                if let Some(Judgment {
                    tree: JudgmentKind::Pi(_func_arg_type, func_expr_type),
                    ..
                }) = func.type_of()
                {
                    let new_result_type = Judgment::instantiate(*func_expr_type, &*elem.clone());
                    Some(new_result_type.lower_outside_bv_index(0))
                // Some(new_result_type.normalize())
                } else {
                    panic!(
                        "type of func should be a Pi, func : {:?}. fimc_type : {:?}",
                        func,
                        func.type_of()
                    )
                }
            }
            JudgmentKind::FreeVar(_free_var, var_type) => Some((*var_type).clone()),
            JudgmentKind::Prim(t, prim_type) => Some(*prim_type),
        }
    }
    // this lowers the "bv" index which are scoped outside of the current judgment.
    pub fn lower_outside_bv_index(self, depth: u32) -> Judgment<T, S> {
        use JudgmentKind::*;
        let judgment_kind = match self.tree.clone() {
            Type => Type,
            Prim(prim, prim_type) => {
                JudgmentKind::Prim(prim, Box::new(prim_type.lower_outside_bv_index(depth)))
            }
            FreeVar(index, var_type) => {
                FreeVar(index, Box::new(var_type.lower_outside_bv_index(depth)))
            }
            Pi(var_type, expr) => Pi(
                Box::new(var_type.lower_outside_bv_index(depth)),
                Box::new(expr.lower_outside_bv_index(depth + 1)),
            ),
            Lam(var_type, expr) => Lam(
                Box::new(var_type.lower_outside_bv_index(depth)),
                Box::new(expr.lower_outside_bv_index(depth + 1)),
            ),
            BoundVar(index, var_type) => {
                if index < depth {
                    BoundVar(index, Box::new(var_type.lower_outside_bv_index(depth)))
                } else {
                    BoundVar(index - 1, Box::new(var_type.lower_outside_bv_index(depth)))
                }
            }
            Application(func, arg) => Application(
                Box::new(func.lower_outside_bv_index(depth)),
                Box::new(arg.lower_outside_bv_index(depth)),
            ),
        };
        Judgment {
            metadata: self.metadata,
            tree: judgment_kind,
        }
    }
    // note it shft bv(i) to bv(i-index)
    pub fn shift(self, index: u32) -> Judgment<T, S> {
        use JudgmentKind::*;
        let judgment_kind = match self.tree.clone() {
            BoundVar(int, var_type) => BoundVar(int - index, Box::new(var_type.shift(index))),
            Type => Type,

            FreeVar(int, var_type) => FreeVar(int, Box::new(var_type.shift(index))),
            Pi(var_type, expr) => Pi(Box::new(var_type.shift(index)), Box::new(expr.shift(index))),
            Lam(var_type, expr) => {
                Lam(Box::new(var_type.shift(index)), Box::new(expr.shift(index)))
            }
            Application(func, arg) => {
                Application(Box::new(func.shift(index)), Box::new(arg.shift(index)))
            }
            Prim(t, prim_type) => Prim(t, Box::new(prim_type.shift(index))),
        };
        Judgment {
            metadata: self.metadata,
            tree: judgment_kind,
        }
    }
    pub fn normalize(self) -> Judgment<T, S> {
        self.nbe()
    }

    /// U:None constructor
    pub fn u(metadata: Option<S>) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::Type,
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
            if type_expr_.tree != JudgmentKind::Type {
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

    pub fn prim(prim: T, prim_type: Judgment<T, S>, metadata: Option<S>) -> Judgment<T, S> {
        Judgment {
            tree: JudgmentKind::Prim(prim, Box::new(prim_type)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn prim_wo_prim_type(prim1: T, metadata: Option<S>) -> Judgment<T, S> {
        Judgment::prim(prim1.clone(), prim1.maybe_prim_type().unwrap(), metadata)
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
    // instaitiate elem = "hi" expr = (bv0, lambda bv1)
    // ("hi", lambda "hi")
    /// Replace the outermost bound variable in expr with elem.
    pub fn instantiate(expr: Judgment<T, S>, elem: &Judgment<T, S>) -> Judgment<T, S> {
        fn instantiate_rec<T: Primitive, S: Metadata>(
            expr: Judgment<T, S>,
            elem: &Judgment<T, S>,
            depth: u32,
        ) -> Judgment<T, S> {
            let result_tree = match expr.tree.clone() {
                JudgmentKind::Type => JudgmentKind::Type,
                JudgmentKind::Pi(var_type, expr) => JudgmentKind::Pi(
                    Box::new(instantiate_rec(*var_type, elem, depth)),
                    Box::new(instantiate_rec(*expr, elem, depth + 1)),
                ),
                JudgmentKind::Lam(var_type, expr) => JudgmentKind::Lam(
                    Box::new(instantiate_rec(*var_type, elem, depth)),
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
                            panic!("The types of the elem and boundVar are wrong, var_type is {:?}, elem is {:?}. elem_type is {:?}", var_type, elem.clone(), elem.clone().type_of());
                        }
                    }
                }
                JudgmentKind::Application(func, arg) => JudgmentKind::Application(
                    Box::new(instantiate_rec(*func, elem, depth)),
                    Box::new(instantiate_rec(*arg, elem, depth)),
                ),
                JudgmentKind::Prim(_, _) => expr.tree.clone(),
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
                JudgmentKind::Type => JudgmentKind::Type,
                JudgmentKind::Pi(var_type, expr) => JudgmentKind::Pi(
                    Box::new(rebind_rec(*var_type, free_var, depth)),
                    Box::new(rebind_rec(*expr, free_var, depth + 1)),
                ),
                JudgmentKind::Lam(var_type, expr) => JudgmentKind::Lam(
                    Box::new(rebind_rec(*var_type, free_var, depth)),
                    Box::new(rebind_rec(*expr, free_var, depth + 1)),
                ),
                JudgmentKind::BoundVar(i, var_type) => {
                    JudgmentKind::BoundVar(i, Box::new(rebind_rec(*var_type, free_var, depth)))
                }
                JudgmentKind::Application(lhs, rhs) => JudgmentKind::Application(
                    Box::new(rebind_rec(*lhs, free_var, depth)),
                    Box::new(rebind_rec(*rhs, free_var, depth)),
                ),
                JudgmentKind::Prim(_, _) => s.tree.clone(),
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
                JudgmentKind::Type => false,
                JudgmentKind::Prim(_, _) => false,
                JudgmentKind::FreeVar(_, expr) => is_bound_var_used(*expr, depth),
                JudgmentKind::Pi(var_type, expr) => {
                    is_bound_var_used(*var_type, depth) || is_bound_var_used(*expr, depth + 1)
                }
                JudgmentKind::Lam(var_type, expr) => {
                    is_bound_var_used(*var_type, depth) || is_bound_var_used(*expr, depth + 1)
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
        prim_meaning: Rc<
            dyn Fn(
                T,
                Judgment<T, S>,
                Rc<dyn Fn(Judgment<T, S>) -> Judgment<U, S>>,
            ) -> Judgment<U, S>,
        >,
    ) -> Judgment<U, S> {
        let prim_meaning_clone = prim_meaning.clone();
        let prim_fn = Rc::new(move |judgment: Judgment<T, S>| {
            judgment.define_prim(prim_meaning_clone.clone())
        });

        let metadata = Some(self.metadata.clone());
        match &self.tree {
            JudgmentKind::Type => Judgment::u(metadata),
            JudgmentKind::Prim(prim, prim_type) => {
                prim_meaning(prim.clone(), *prim_type.clone(), prim_fn)
            }
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
            JudgmentKind::Type => Judgment::u(None),
            JudgmentKind::Prim(prim, prim_type) => {
                Judgment::prim(prim, Judgment::cast_metadata(*prim_type), None)
            }
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

    pub fn contains_free_var(expr: &Judgment<T, S>, var: VarUuid) -> bool {
        match &expr.tree {
            JudgmentKind::Type => false,
            JudgmentKind::Prim(_, prim_type) => Judgment::contains_free_var(prim_type, var),
            JudgmentKind::FreeVar(var_index, var_type) => {
                if *var_index == var {
                    return true;
                } else {
                    return Judgment::contains_free_var(&*var_type, var);
                }
            }
            JudgmentKind::Pi(var_type, expr) => {
                Judgment::contains_free_var(&*var_type, var)
                    || Judgment::contains_free_var(&*expr, var)
            }
            JudgmentKind::Lam(var_type, expr) => {
                Judgment::contains_free_var(&*var_type, var)
                    || Judgment::contains_free_var(&*expr, var)
            }
            JudgmentKind::BoundVar(_, var_type) => Judgment::contains_free_var(&*var_type, var),
            JudgmentKind::Application(func, arg) => {
                Judgment::contains_free_var(&*func, var) || Judgment::contains_free_var(&*arg, var)
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum NatPrim {
    NatType,
    Nat(i32),
    Add,
    Add3,
}
impl Primitive for NatPrim {
    // Potentially the prim type is not given by the primitive itself, like in the case of ffi.
    fn maybe_prim_type<S: Metadata>(&self) -> Option<Judgment<Self, S>> {
        use xi_proc_macro::term;
        Some(match self {
            NatPrim::NatType => term!(U),
            NatPrim::Nat(_) => term!([NatPrim::NatType]),
            NatPrim::Add => term!([NatPrim::NatType] -> [NatPrim::NatType] -> [NatPrim::NatType]),
            NatPrim::Add3 => term!([NatPrim::NatType] -> [NatPrim::NatType]),
        })
    }
}

impl Primitive for () {
    fn maybe_prim_type<S: Metadata>(&self) -> Option<Judgment<Self, S>> {
        Some(Judgment::u(None))
    }
}

impl Metadata for () {}

mod test {
    use crate::judgment::Judgment;
    use xi_proc_macro::term;
    use xi_uuid::VarUuid;

    #[test]
    fn variable_binding_test() {
        // let id: Judgment<(), ()> = term!(Lam | T: U, t: T | t);
        // dbg!(id.type_of());

        // let pr1: Judgment<(), ()> = term!(Lam | T: U, t1: T, t2: T | t1);
        // dbg!(&pr1);
        // dbg!(&pr1.type_of());

        // let pr2: Judgment<(), ()> = term!(Lam | T: U, t1: T, t2: T | t2);
        // dbg!(&pr2);
        // dbg!(&pr2.type_of());

        // let zero: Judgment<(), ()> = term!(Lam |T : U, zero : T, succ: T -> T| zero);
        // dbg!(&zero);
        // dbg!(&zero.type_of());

        // let func: Judgment<(), ()> = term!(Lam | T: U, succ: T -> T| succ );
        // dbg!(&func);
        // dbg!(&func.type_of());

        let test: Judgment<(), ()> = term!(Lam |T : U, f : U -> T| f T);
        dbg!(&test);
        dbg!(&test.type_of());
    }
}
