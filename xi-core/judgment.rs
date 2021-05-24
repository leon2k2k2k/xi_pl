/*  The syntax of Xi, which is calculus of construction, but based on judgments and not terms.
    We are going to construct 3 types of apply function, differ by their type checking:
    given func and elem, then we require is_type(func) is of the form Pi(var_type, expr), and that elem.is_type = var_type.
    The key point is that there are three ways to write equality: they are (extensionally) strictly equal, or their normalization is
    strictly equal, or a path between them, when viewed Judgment as a higher inductive type with internal equality imposed over
    beta-reduction and eta-conversion.
*/

use std::{collections::BTreeMap, rc::Rc};

use crate::nbe::SJudgment;
use text_size::TextRange;
use xi_uuid::VarUuid;

#[derive(Clone)]
pub struct Judgment<T> {
    pub metadata: Metadata,
    pub tree: Box<JudgmentKind<T>>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum JudgmentKind<T> {
    Type,
    Prim(T, Judgment<T>),
    FreeVar(VarUuid, Judgment<T>),
    Pi(Judgment<T>, ScopedJudgment<T>),
    Lam(Judgment<T>, ScopedJudgment<T>),
    BoundVar(u32, Judgment<T>),
    App(Judgment<T>, Judgment<T>),
}
#[derive(Clone, PartialEq, Eq)]
pub struct ScopedJudgment<T>(pub(crate) Judgment<T>);

pub trait Primitive: Clone + PartialEq + Eq + 'static + std::fmt::Debug
where
    Self: Sized,
{
    fn maybe_prim_type(&self) -> Option<Judgment<Self>>;
}

pub trait MetadataTrait: 'static + std::fmt::Debug {
    fn span(&self) -> Option<TextRange>;
    fn var_name(&self) -> Option<String>;
}

#[derive(Clone, Debug)]
pub struct Metadata(Rc<dyn MetadataTrait>);

#[derive(Clone, Debug)]
pub struct Empty();

impl MetadataTrait for Empty {
    fn span(&self) -> Option<TextRange> {
        None
    }

    fn var_name(&self) -> Option<String> {
        None
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Metadata(Rc::new(Empty()))
    }
}

impl<T: Primitive> ScopedJudgment<T> {
    pub fn unbind(self) -> (VarUuid, Judgment<T>) {
        let index = VarUuid::new();
        fn unbind_rec<T: Primitive>(
            judgment: Judgment<T>,
            index: VarUuid,
            depth: u32,
        ) -> Judgment<T> {
            let judgmentkind = match *judgment.tree {
                JudgmentKind::Type => JudgmentKind::Type,
                JudgmentKind::Prim(prim, prim_type) => {
                    JudgmentKind::Prim(prim, unbind_rec(prim_type, index, depth))
                }
                JudgmentKind::FreeVar(i, var_type) => {
                    JudgmentKind::FreeVar(i, unbind_rec(var_type, index, depth))
                }
                JudgmentKind::Pi(var_type, sexpr) => JudgmentKind::Pi(
                    unbind_rec(var_type, index, depth),
                    ScopedJudgment(unbind_rec(sexpr.0, index, depth + 1)),
                ),
                JudgmentKind::Lam(var_type, sexpr) => JudgmentKind::Lam(
                    unbind_rec(var_type, index, depth),
                    ScopedJudgment(unbind_rec(sexpr.0, index, depth + 1)),
                ),
                JudgmentKind::BoundVar(i, var_type) => {
                    if i == depth {
                        JudgmentKind::FreeVar(index, var_type)
                    } else {
                        JudgmentKind::BoundVar(i, unbind_rec(var_type, index, depth))
                    }
                }
                JudgmentKind::App(func, arg) => JudgmentKind::App(
                    unbind_rec(func, index, depth),
                    unbind_rec(arg, index, depth),
                ),
            };
            Judgment {
                metadata: judgment.metadata,
                tree: Box::new(judgmentkind),
            }
        }
        (index, unbind_rec(self.0, index, 0))
    }

    pub fn instantiate(self, sub: &Judgment<T>) -> Judgment<T> {
        let (index, judgment) = self.unbind();
        fn instantiate_rec<T: Primitive>(
            judgment: Judgment<T>,
            index: VarUuid,
            sub: &Judgment<T>,
        ) -> Judgment<T> {
            let judgmentkind = match *judgment.tree {
                JudgmentKind::Type => JudgmentKind::Type,
                JudgmentKind::Prim(prim, prim_type) => {
                    JudgmentKind::Prim(prim, instantiate_rec(prim_type, index, sub))
                }
                JudgmentKind::FreeVar(i, var_type) => {
                    if i == index {
                        *sub.tree.clone()
                    } else {
                        JudgmentKind::FreeVar(i, instantiate_rec(var_type, index, sub))
                    }
                }
                JudgmentKind::Pi(var_type, sexpr) => {
                    let (i, expr) = sexpr.unbind();
                    let inst_expr = instantiate_rec(expr, index, sub);
                    let inst_sexpr = inst_expr.bind(i);
                    JudgmentKind::Pi(instantiate_rec(var_type, index, sub), inst_sexpr)
                }
                JudgmentKind::Lam(var_type, sexpr) => {
                    let (i, expr) = sexpr.unbind();
                    let inst_expr = instantiate_rec(expr, index, sub);
                    let inst_sexpr = inst_expr.bind(i);
                    JudgmentKind::Pi(instantiate_rec(var_type, index, sub), inst_sexpr)
                }
                JudgmentKind::BoundVar(_i, _var_type) => {
                    unreachable!("We should never see boundvar");
                }
                JudgmentKind::App(func, arg) => JudgmentKind::App(
                    instantiate_rec(func, index, sub),
                    instantiate_rec(arg, index, sub),
                ),
            };
            Judgment {
                metadata: judgment.metadata,
                tree: Box::new(judgmentkind),
            }
        }

        instantiate_rec(judgment, index, sub)
    }

    pub fn replace_free_var(self, index: VarUuid) -> Judgment<T> {
        pub fn replace_free_var_rec<T: Primitive>(
            expr: Judgment<T>,
            old_index: VarUuid,
            new_index: VarUuid,
        ) -> Judgment<T> {
            let judgmentkind = match *expr.tree {
                JudgmentKind::Type => JudgmentKind::Type,
                JudgmentKind::Prim(prim, prim_type) => {
                    JudgmentKind::Prim(prim, replace_free_var_rec(prim_type, old_index, new_index))
                }
                JudgmentKind::FreeVar(index, var_type) => {
                    if index == old_index {
                        JudgmentKind::FreeVar(new_index, var_type)
                    } else {
                        JudgmentKind::FreeVar(
                            index,
                            replace_free_var_rec(var_type, old_index, new_index),
                        )
                    }
                }
                JudgmentKind::Pi(var_type, sexpr) => {
                    let (index, expr) = sexpr.unbind();
                    JudgmentKind::Pi(
                        replace_free_var_rec(var_type, old_index, new_index),
                        replace_free_var_rec(expr, old_index, new_index).bind(index),
                    )
                }
                JudgmentKind::Lam(var_type, sexpr) => {
                    let (index, expr) = sexpr.unbind();
                    JudgmentKind::Lam(
                        replace_free_var_rec(var_type, old_index, new_index),
                        replace_free_var_rec(expr, old_index, new_index).bind(index),
                    )
                }
                JudgmentKind::BoundVar(_index, _var_type) => {
                    unreachable!()
                }
                JudgmentKind::App(func, arg) => JudgmentKind::App(
                    replace_free_var_rec(func, old_index, new_index),
                    replace_free_var_rec(arg, old_index, new_index),
                ),
            };
            Judgment {
                metadata: expr.metadata,
                tree: Box::new(judgmentkind),
            }
        }
        let (old_index, expr) = self.unbind();
        replace_free_var_rec(expr, old_index, index)
    }
}

impl<T: Primitive> Judgment<T> {
    pub fn bind(self, index: VarUuid) -> ScopedJudgment<T> {
        fn bind_rec<T: Primitive>(
            judgment: Judgment<T>,
            index: VarUuid,
            depth: u32,
        ) -> Judgment<T> {
            let judgmentkind = match *judgment.tree {
                JudgmentKind::Type => JudgmentKind::Type,
                JudgmentKind::Prim(prim, prim_type) => {
                    JudgmentKind::Prim(prim, bind_rec(prim_type, index, depth))
                }
                JudgmentKind::FreeVar(i, var_type) => {
                    if i == index {
                        JudgmentKind::BoundVar(depth, var_type)
                    } else {
                        JudgmentKind::FreeVar(i, bind_rec(var_type, index, depth))
                    }
                }
                JudgmentKind::Pi(var_type, sexpr) => JudgmentKind::Pi(
                    bind_rec(var_type, index, depth),
                    ScopedJudgment(bind_rec(sexpr.0, index, depth + 1)),
                ),
                JudgmentKind::Lam(var_type, sexpr) => JudgmentKind::Lam(
                    bind_rec(var_type, index, depth),
                    ScopedJudgment(bind_rec(sexpr.0, index, depth + 1)),
                ),
                JudgmentKind::BoundVar(i, var_type) => {
                    JudgmentKind::BoundVar(i, bind_rec(var_type, index, depth))
                }
                JudgmentKind::App(func, arg) => {
                    JudgmentKind::App(bind_rec(func, index, depth), bind_rec(arg, index, depth))
                }
            };
            Judgment {
                metadata: judgment.metadata,
                tree: Box::new(judgmentkind),
            }
        }
        ScopedJudgment(bind_rec(self, index, 0))
    }

    pub fn to_scoped(self) -> ScopedJudgment<T> {
        let var = VarUuid::new();
        self.bind(var)
    }
}

impl<T: PartialEq> PartialEq for Judgment<T> {
    fn eq(&self, other: &Judgment<T>) -> bool {
        self.tree == other.tree
    }
}

impl<T: PartialEq> Eq for Judgment<T> {}

impl<T: Primitive> Judgment<T> {
    pub fn has_type(&self) -> bool {
        match *self.tree.clone() {
            JudgmentKind::Type => false,
            JudgmentKind::Pi(_var_type, sexpr) => {
                let (_index, expr) = sexpr.unbind();
                expr.has_type()
            }
            JudgmentKind::Lam(_var_type, _sexpr) => true,
            JudgmentKind::BoundVar(_, _) => unreachable!("Bound Var"),
            JudgmentKind::App(_func, _elem) => false,
            JudgmentKind::FreeVar(_free_var, _var_type) => true,
            JudgmentKind::Prim(_t, _prim_type) => true,
        }
    }
    /// Takes a judgment and returns its the judgment representing its type
    /// Panics if it has no type
    pub fn type_of(&self) -> Judgment<T> {
        match *self.tree.clone() {
            JudgmentKind::Type => unreachable!("expression has no type"),
            JudgmentKind::Pi(_var_type, sexpr) => {
                let (_index, expr) = sexpr.unbind();
                expr.type_of()
            }
            JudgmentKind::Lam(var_type, sexpr) => {
                let (index, expr) = sexpr.unbind();

                Judgment::pi(
                    var_type.clone(),
                    expr.type_of().nbe().bind(index),
                    Some(self.metadata.clone()),
                )
            }
            JudgmentKind::BoundVar(_, _) => unreachable!("Bound Var"),
            JudgmentKind::App(func, elem) => {
                let func_type = func.type_of();

                if let JudgmentKind::Pi(_var_type, sexpr) = *func_type.tree {
                    sexpr.instantiate(&elem).nbe()
                } else {
                    panic!("Expected type of a function to be a Pi")
                }
            }
            JudgmentKind::FreeVar(_free_var, var_type) => var_type.nbe(),
            JudgmentKind::Prim(_t, prim_type) => prim_type.nbe(),
        }
    }

    /// U:None constructor
    pub fn u(metadata: Option<Metadata>) -> Judgment<T> {
        Judgment {
            tree: Box::new(JudgmentKind::Type),
            metadata: metadata.unwrap_or_default(),
        }
    }
    /// Pi constructor
    pub fn pi(
        var_type: Judgment<T>,
        sexpr: ScopedJudgment<T>,
        metadata: Option<Metadata>,
    ) -> Judgment<T> {
        let (index, expr) = sexpr.unbind();

        if expr.has_type() && *expr.type_of().tree != JudgmentKind::Type {
            panic!("pi type needs a type as expr");
        }

        Judgment {
            tree: Box::new(JudgmentKind::Pi(var_type, expr.bind(index))),
            metadata: metadata.unwrap_or_default(),
        }
    }
    /// Lambda constructor
    pub fn lam(
        var_type: Judgment<T>,
        sexpr: ScopedJudgment<T>,
        metadata: Option<Metadata>,
    ) -> Judgment<T> {
        Judgment {
            tree: Box::new(JudgmentKind::Lam(var_type, sexpr)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn free(index: VarUuid, var_type: Judgment<T>, metadata: Option<Metadata>) -> Judgment<T> {
        Judgment {
            tree: Box::new(JudgmentKind::FreeVar(index, var_type)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn prim(prim: T, prim_type: Judgment<T>, metadata: Option<Metadata>) -> Judgment<T> {
        Judgment {
            tree: Box::new(JudgmentKind::Prim(prim, prim_type)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn prim_wo_prim_type(prim1: T, metadata: Option<Metadata>) -> Judgment<T> {
        Judgment::prim(prim1.clone(), prim1.maybe_prim_type().unwrap(), metadata)
    }
    pub fn app(func: Judgment<T>, elem: Judgment<T>, metadata: Option<Metadata>) -> Judgment<T> {
        let func_type = func.clone().type_of().nbe();
        if let JudgmentKind::Pi(func_arg_type, _) = *func_type.tree {
            let elem_type = elem.clone().type_of().nbe();

            if func_arg_type != elem_type {
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
            tree: Box::new(JudgmentKind::App(func, elem)),
            metadata: metadata.unwrap_or_default(),
        }
    }
    // instaitiate elem = "hi" expr = (bv0, lambda bv1)
    // ("hi", lambda "hi")
    /// Replace the outermost bound variable in expr with elem.

    pub fn app_unchecked(
        func: Judgment<T>,
        arg: Judgment<T>,
        metadata: Option<Metadata>,
    ) -> Judgment<T> {
        if let JudgmentKind::Pi(_, _) = *func.tree {
            panic!();
        }
        Judgment {
            tree: Box::new(JudgmentKind::App(func, arg)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    pub fn app_unchecked_vec(
        func: Judgment<T>,
        args: Vec<Judgment<T>>,
        metadata: Option<u32>,
    ) -> Judgment<T> {
        let mut args = args;
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
        var_type: Judgment<T>,
        expr: ScopedJudgment<T>,
        metadata: Option<Metadata>,
    ) -> Judgment<T> {
        Judgment {
            tree: Box::new(JudgmentKind::Pi(var_type, expr)),
            metadata: metadata.unwrap_or_default(),
        }
    }

    /// Normalization to beta-reduced, eta-long form. beta-reduced means that app(lam(var_type,expr), elem) is beta-reduced to expr[BoundVar(?)\elem].
    pub fn nbe(self) -> Judgment<T> {
        SJudgment::semantics_to_syntax(SJudgment::syntax_to_semantics(self, BTreeMap::new()))
    }

    pub fn define_prim<U: Primitive>(
        &self,
        prim_meaning: Rc<
            dyn Fn(T, Judgment<T>, Rc<dyn Fn(Judgment<T>) -> Judgment<U>>) -> Judgment<U>,
        >,
    ) -> Judgment<U> {
        let prim_meaning_clone = prim_meaning.clone();
        let prim_fn =
            Rc::new(move |judgment: Judgment<T>| judgment.define_prim(prim_meaning_clone.clone()));

        let metadata = Some(self.metadata.clone());
        match *self.tree.clone() {
            JudgmentKind::Type => Judgment::u(metadata),
            JudgmentKind::Prim(prim, prim_type) => {
                prim_meaning(prim.clone(), prim_type.clone(), prim_fn)
            }
            JudgmentKind::FreeVar(index, var_type) => {
                Judgment::free(index, var_type.define_prim(prim_meaning), metadata)
            }
            JudgmentKind::Pi(var_type, sexpr) => {
                let (index, expr) = sexpr.unbind();
                Judgment::pi_unchecked(
                    var_type.define_prim(prim_meaning.clone()),
                    expr.define_prim(prim_meaning).bind(index),
                    metadata,
                )
            }
            JudgmentKind::Lam(var_type, sexpr) => {
                let (index, expr) = sexpr.unbind();
                Judgment::lam(
                    var_type.define_prim(prim_meaning.clone()),
                    expr.define_prim(prim_meaning).bind(index),
                    metadata,
                )
            }
            JudgmentKind::BoundVar(_index, _var_type) => {
                unreachable!()
            }
            JudgmentKind::App(lhs, rhs) => Judgment::app(
                lhs.define_prim(prim_meaning.clone()),
                rhs.define_prim(prim_meaning),
                metadata,
            ),
        }
    }

    pub fn define_prim_unchecked<U: Primitive>(
        &self,
        prim_meaning: Rc<
            dyn Fn(T, Judgment<T>, Rc<dyn Fn(Judgment<T>) -> Judgment<U>>) -> Judgment<U>,
        >,
    ) -> Judgment<U> {
        let prim_meaning_clone = prim_meaning.clone();
        let prim_fn =
            Rc::new(move |judgment: Judgment<T>| judgment.define_prim(prim_meaning_clone.clone()));

        let metadata = Some(self.metadata.clone());
        match *self.tree.clone() {
            JudgmentKind::Type => Judgment::u(metadata),
            JudgmentKind::Prim(prim, prim_type) => {
                prim_meaning(prim.clone(), prim_type.clone(), prim_fn)
            }
            JudgmentKind::FreeVar(index, var_type) => Judgment::free(
                index,
                var_type.define_prim_unchecked(prim_meaning),
                metadata,
            ),
            JudgmentKind::Pi(var_type, sexpr) => {
                let (index, expr) = sexpr.unbind();
                Judgment::pi_unchecked(
                    var_type.define_prim_unchecked(prim_meaning.clone()),
                    expr.define_prim_unchecked(prim_meaning).bind(index),
                    metadata,
                )
            }
            JudgmentKind::Lam(var_type, sexpr) => {
                let (index, expr) = sexpr.unbind();
                Judgment::lam(
                    var_type.define_prim_unchecked(prim_meaning.clone()),
                    expr.define_prim_unchecked(prim_meaning).bind(index),
                    metadata,
                )
            }
            JudgmentKind::BoundVar(_index, _var_type) => {
                unreachable!()
            }
            JudgmentKind::App(lhs, rhs) => Judgment::app_unchecked(
                lhs.define_prim_unchecked(prim_meaning.clone()),
                rhs.define_prim_unchecked(prim_meaning),
                metadata,
            ),
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
    fn maybe_prim_type(&self) -> Option<Judgment<Self>> {
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
    fn maybe_prim_type(&self) -> Option<Judgment<Self>> {
        Some(Judgment::u(None))
    }
}

impl MetadataTrait for () {
    fn span(&self) -> Option<TextRange> {
        None
    }

    fn var_name(&self) -> Option<String> {
        None
    }
}

mod test {
    #[test]
    fn variable_binding_test() {
        use crate::judgment::Judgment;
        use xi_proc_macro::term;
        use xi_uuid::VarUuid;

        let unit_type: Judgment<()> = term!(Pi |T : U| T -> T);
        let id: Judgment<()> = term!(Lam | T: U, t: T | t);
        assert_eq!(id.type_of(), unit_type);
        // let x: u32 = 5;

        // let pr1: Judgment<(), ()> = term!(Lam | T: U, t1: T, t2: T | t1);
        // dbg!(&pr1);
        // dbg!(&pr1.type_of());

        // let pr2: Judgment<(), ()> = term!(Lam | T: U, t1: T, t2: T | t2);
        // dbg!(&pr2);
        // dbg!(&pr2.type_of());

        // let zero: Judgment<(), ()> = term!(Lam |T : U, zero : T, succ: T -> T| zero);
        // dbg!(&zero);
        // dbg!(&zero.type_of());

        // let one: Judgment<(), ()> = term!(Lam |T : U, zero : T, succ: T -> T| succ zero);
        // dbg!(&one);
        // dbg!(&one.type_of());

        // let func: Judgment<(), ()> = term!(Lam | T: U, succ: T -> T| succ );
        // dbg!(&func);
        // dbg!(&func.type_of());

        // let test: Judgment<(), ()> = term!(Lam |T : U, f : U -> T| f T);
        // dbg!(&test);
        // dbg!(&test.type_of());
    }

    #[test]
    fn tedst() {
        use super::*;
        use xi_proc_macro::term;
        use xi_uuid::VarUuid;
        let ((_a, _b), t): (_, Judgment<()>) = term!(|fv9 : U, fv12 : Pi |bv0: U -> U| bv0 fv9 -> bv0 (Pi |bv2: U| bv2 -> bv2)| fv12 (Lam |bv0: U| bv0 -> fv9) (Lam |bv0 : fv9| bv0));
        dbg!(t.type_of());
    }
}
