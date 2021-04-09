// Define a map from Judgment<T,S> to term macros so we can read better.
use crate::judgment::*;
use xi_uuid::VarUuid;

#[derive(Clone, PartialEq, Eq, Debug)]

pub enum JudgmentTree<T> {
    Type,
    Prim(T, Box<JudgmentTree<T>>),
    FreeVar(VarUuid, Box<JudgmentTree<T>>),
    BoundVar(u32, Box<JudgmentTree<T>>),
    // I should add metadata here.
    Fun(Vec<JudgmentTree<T>>),
    Pi(Vec<JudgmentTree<T>>, Box<JudgmentTree<T>>),
    Lam(Vec<JudgmentTree<T>>, Box<JudgmentTree<T>>),
    App(Vec<JudgmentTree<T>>),
    Metadata(Box<JudgmentTree<T>>, String),
}

fn is_outermost_bound_var_used<T, S>(expr: Judgment<T, S>) -> bool {
    fn is_bound_var_used<T, S>(expr: Judgment<T, S>, depth: u32) -> bool {
        match *expr.tree {
            JudgmentKind::Type => false,
            JudgmentKind::Prim(_, _) => false,
            JudgmentKind::FreeVar(_, expr) => is_bound_var_used(expr, depth),
            JudgmentKind::Pi(var_type, expr) => {
                is_bound_var_used(var_type, depth) || is_bound_var_used(expr.0, depth + 1)
            }
            JudgmentKind::Lam(var_type, expr) => {
                is_bound_var_used(var_type, depth) || is_bound_var_used(expr.0, depth + 1)
            }
            JudgmentKind::BoundVar(int, expr) => int == depth || is_bound_var_used(expr, depth),
            JudgmentKind::App(func, elem) => {
                is_bound_var_used(func, depth) || is_bound_var_used(elem, depth)
            }
        }
    }
    is_bound_var_used(expr, 0)
}

pub fn judgment_to_tree<T: Primitive, S: Metadata>(judgment: Judgment<T, S>) -> JudgmentTree<T> {
    use JudgmentTree::*;
    let judgment_tree = match *judgment.tree.clone() {
        JudgmentKind::Type => JudgmentTree::Type,
        JudgmentKind::Prim(prim, prim_type) => {
            JudgmentTree::Prim(prim, Box::new(judgment_to_tree(prim_type)))
        }
        JudgmentKind::FreeVar(index, var_type) => {
            JudgmentTree::FreeVar(index, Box::new(judgment_to_tree(var_type)))
        }
        JudgmentKind::Pi(var_type, sexpr) => {
            let expr_tree = judgment_to_tree(sexpr.0.clone());
            let var_type_tree = judgment_to_tree(var_type);

            if is_outermost_bound_var_used(sexpr.0) {
                if let Pi(mut vec, expr) = expr_tree {
                    vec.insert(0, var_type_tree);
                    Pi(vec, expr)
                } else {
                    Pi(vec![var_type_tree], Box::new(expr_tree.clone()))
                }
            } else {
                if let Fun(mut vec) = expr_tree {
                    vec.insert(0, var_type_tree);
                    Fun(vec)
                } else {
                    Fun(vec![var_type_tree, expr_tree])
                }
            }
        }
        JudgmentKind::Lam(var_type, expr) => {
            let expr_tree = judgment_to_tree(expr.0);
            let var_type_tree = judgment_to_tree(var_type);
            if let Lam(mut vec, expr) = expr_tree {
                vec.insert(0, var_type_tree);
                Lam(vec, expr)
            } else {
                Lam(vec![var_type_tree], Box::new(expr_tree.clone()))
            }
        }
        JudgmentKind::BoundVar(index, expr) => {
            JudgmentTree::BoundVar(index, Box::new(judgment_to_tree(expr)))
        }
        JudgmentKind::App(func, elem) => {
            let func_tree = judgment_to_tree(func);
            let elem_tree = judgment_to_tree(elem);
            if let App(mut vec) = func_tree {
                vec.push(elem_tree);
                App(vec)
            } else {
                App(vec![func_tree, elem_tree])
            }
        }
    };
    if judgment.metadata == S::default() {
        judgment_tree
    } else {
        JudgmentTree::Metadata(Box::new(judgment_tree), format!("{:?}", judgment.metadata))
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Precedence {
    Top,
    Fun,
    App,
    Var,
}

pub fn tree_to_string<T: Primitive>(judg_tree: &JudgmentTree<T>) -> String {
    fn tts_rec<T: Primitive>(
        judg_tree: &JudgmentTree<T>,
        free_vars: &mut Vec<String>,
        precedence: Precedence,
        depth: u32,
    ) -> String {
        let body_str = match judg_tree {
            JudgmentTree::Type => "U".into(),
            JudgmentTree::Prim(prim, prim_type) => match prim.maybe_prim_type::<()>() {
                None => format!(
                    "({:?} : {})",
                    prim,
                    tts_rec(prim_type, free_vars, Precedence::Top, depth),
                ),
                Some(_) => format!("{:?}", prim),
            },
            JudgmentTree::FreeVar(var_index, var_type) => {
                format!(
                    "(fv{} : {})",
                    var_index.index(),
                    tts_rec(&*var_type, free_vars, Precedence::Top, depth)
                )
                // let free_var_index = free_vars.len();
                // free_vars
                //     .clone()
                //     .push(tts_rec(&*var_type, free_vars, Precedence::Top, depth));
                // format!("fv{}", free_var_index)
            }
            // we put VarUuid in the beginning.
            JudgmentTree::BoundVar(index, var_type) => match depth.checked_sub(1 + index) {
                // _ => format!("(v{} : {})", index, tts_rec(&*var_type, free_vars, Precedence::Top, depth))
                Some(value) => format!("bv{}", value),
                None => format!(
                    "(v_not_bound{}: {})",
                    1 + index - depth,
                    tts_rec(&*var_type, free_vars, Precedence::Top, depth)
                ),
            },
            JudgmentTree::Fun(vec) => {
                let mut str_vec: Vec<String> = vec[0..vec.len() - 1]
                    .iter()
                    .zip(depth..)
                    .map(|(s, new_depth)| tts_rec(&*s, free_vars, Precedence::App, new_depth))
                    .collect();
                str_vec.push(tts_rec(
                    &vec[vec.len() - 1],
                    free_vars,
                    Precedence::Top,
                    depth + vec.len() as u32 - 1,
                ));
                str_vec.join(" -> ")
            }
            JudgmentTree::Pi(var_type_vec, expr) => {
                let var_types = var_type_vec
                    .iter()
                    .zip(depth..)
                    .map(|(s, new_depth)| tts_rec(&*s, free_vars, Precedence::Top, new_depth));
                let binding_str = var_types
                    .zip(depth..)
                    .map(|(binding, new_depth)| format!("bv{}: {}", new_depth, binding))
                    .collect::<Vec<String>>()
                    .join(", ");

                let expr_str = tts_rec(
                    &*expr,
                    free_vars,
                    Precedence::Top,
                    depth + var_type_vec.len() as u32,
                );
                format!("Pi |{}| {}", binding_str, expr_str)
            }
            JudgmentTree::Lam(var_type_vec, expr) => {
                let var_types = var_type_vec
                    .iter()
                    .zip(depth..)
                    .map(|(s, new_depth)| tts_rec(&*s, free_vars, Precedence::Top, new_depth));
                let binding_str = var_types
                    .zip(depth..)
                    .map(|(binding, new_depth)| format!("bv{}: {}", new_depth, binding))
                    .collect::<Vec<String>>()
                    .join(", ");

                let expr_str = tts_rec(
                    &*expr,
                    free_vars,
                    Precedence::Top,
                    depth + var_type_vec.len() as u32,
                );
                format!("Lam |{}| {}", binding_str, expr_str)
            }
            JudgmentTree::App(vec) => {
                let str_vec: Vec<String> = vec
                    .iter()
                    .map(|s| tts_rec(&*s, free_vars, Precedence::Var, depth))
                    .collect();
                str_vec.join(" ")
            }
            JudgmentTree::Metadata(tree, metadata) => {
                format!(
                    "Metadata({}, {})",
                    tts_rec(&*tree, free_vars, Precedence::Top, depth),
                    metadata
                )
            }
        };

        let my_precedence = match judg_tree {
            JudgmentTree::Type => Precedence::Var,
            JudgmentTree::Prim(_, _) => Precedence::Var,
            JudgmentTree::FreeVar(_, _) => Precedence::Var,
            JudgmentTree::BoundVar(_, _) => Precedence::Var,
            JudgmentTree::Fun(_) => Precedence::Fun,
            JudgmentTree::Pi(_, _) => Precedence::Top,
            JudgmentTree::Lam(_, _) => Precedence::Top,
            JudgmentTree::App(_) => Precedence::App,
            JudgmentTree::Metadata(_, _) => Precedence::Top,
        };

        if precedence > my_precedence {
            format!("({})", body_str)
        } else {
            format!("{}", body_str)
        }
    }
    let mut free_vars = vec![];
    let expr_part = tts_rec(&judg_tree, &mut free_vars, Precedence::Top, 0);
    let binding_str = free_vars
        .iter()
        .enumerate()
        .map(|(binding, num)| format!("fv{} : {}", num, binding))
        .collect::<Vec<String>>()
        .join(", ");

    if binding_str != "" {
        format!("|{}| {}", binding_str, expr_part)
    } else {
        format!("{}", expr_part)
    }
}

impl<T: Primitive, S: Metadata> std::fmt::Debug for Judgment<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&tree_to_string(&judgment_to_tree(self.clone())))?;
        Ok(())
    }
}
mod test {
    #[test]
    fn judgment_to_tree_test() {
        use super::*;
        use xi_proc_macro::term;

        let test0: Judgment<(), ()> = term!(Pi | A: U | A);
        assert_eq!(tree_to_string(&judgment_to_tree(test0)), "Pi |v0 : U| v0");

        let test1: Judgment<(), ()> = term!(Pi |A : U, B : U, C : U| A -> B -> C);
        assert_eq!(
            tree_to_string(&judgment_to_tree(test1)),
            "Pi |v0 : U, v1 : U, v2 : U| v0 -> v1 -> v2"
        );

        let test2: Judgment<(), ()> = term!(Pi |T : U| T -> (T -> T) -> T);
        assert_eq!(
            tree_to_string(&judgment_to_tree(test2)),
            "Pi |v0 : U| v0 -> (v0 -> v0) -> v0"
        );

        let test3: Judgment<(), ()> = term!(Pi |T : U, P : U -> U| (Pi |S : U| S) -> P T -> P T);
        assert_eq!(
            tree_to_string(&judgment_to_tree(test3)),
            "Pi |v0 : U, v1 : U -> U| (Pi |v2 : U| v2) -> v1 v0 -> v1 v0"
        );

        let test4: Judgment<(), ()> = term!(Lam | T: U, t: T | t);
        assert_eq!(
            tree_to_string(&judgment_to_tree(test4)),
            "Lam |v0 : U, v1 : v0| v1"
        );
    }
}
