/// Define a map from Judgment<T,S> to term macros so we can read better.
use crate::judgment::*;
use xi_uuid::VarUuid;

#[derive(Clone, PartialEq, Eq, Debug)]

pub enum JudgmentTree<T> {
    UinNone,
    Prim(T),
    VarUuid(VarUuid, Box<JudgmentTree<T>>),
    BoundVar(u32, Box<JudgmentTree<T>>),
    /// I should add metadata here.
    Fun(Vec<JudgmentTree<T>>),
    Pi(Vec<JudgmentTree<T>>, Box<JudgmentTree<T>>),
    Lam(Vec<JudgmentTree<T>>, Box<JudgmentTree<T>>),
    App(Vec<JudgmentTree<T>>),
}

pub fn judgment_to_tree<T: Primitive, S: Metadata>(judgment: Judgment<T, S>) -> JudgmentTree<T> {
    use JudgmentTree::*;
    match judgment.tree {
        JudgmentKind::UInNone => JudgmentTree::UinNone,
        JudgmentKind::Prim(prim) => JudgmentTree::Prim(prim),
        JudgmentKind::FreeVar(free_var, expr) => {
            JudgmentTree::VarUuid(free_var, Box::new(judgment_to_tree(*expr)))
        }
        JudgmentKind::Pi(var_type, expr) => {
            let expr_tree = judgment_to_tree(*expr.clone());
            let var_type_tree = judgment_to_tree(*var_type);
            if expr.is_outermost_bound_var_used() {
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
            let expr_tree = judgment_to_tree(*expr);
            let var_type_tree = judgment_to_tree(*var_type);
            if let Lam(mut vec, expr) = expr_tree {
                vec.insert(0, var_type_tree);
                Lam(vec, expr)
            } else {
                Lam(vec![var_type_tree], Box::new(expr_tree.clone()))
            }
        }
        JudgmentKind::BoundVar(u32, expr) => {
            JudgmentTree::BoundVar(u32, Box::new(judgment_to_tree(*expr)))
        }
        JudgmentKind::Application(func, elem) => {
            let func_tree = judgment_to_tree(*func);
            let elem_tree = judgment_to_tree(*elem);
            if let App(mut vec) = func_tree {
                vec.push(elem_tree);
                App(vec)
            } else {
                App(vec![func_tree, elem_tree])
            }
        }
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
            JudgmentTree::UinNone => "U".into(),
            JudgmentTree::Prim(prim) => format!("{:?}", prim),
            JudgmentTree::VarUuid(_free_var, var_type) => {
                let free_var_index = free_vars.len();
                free_vars
                    .clone()
                    .push(tts_rec(&*var_type, free_vars, Precedence::Top, depth));
                format!("fv{}", free_var_index)
            }
            // we put VarUuid in the beginning.
            JudgmentTree::BoundVar(index, _var_type) => match depth.checked_sub(1 + index) {
                Some(value) => format!("v{}", value),
                None => format!("f{}", 1 + index - depth),
            },
            JudgmentTree::Fun(vec) => {
                let mut str_vec: Vec<String> = vec[0..vec.len() - 1]
                    .iter()
                    .zip(depth + 1..)
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
                    .zip(depth + 1..)
                    .map(|(s, new_depth)| tts_rec(&*s, free_vars, Precedence::Top, new_depth));
                let binding_str = var_types
                    .zip(depth..)
                    .map(|(binding, new_depth)| format!("v{} : {}", new_depth, binding))
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
                    .zip(depth + 1..)
                    .map(|(s, new_depth)| tts_rec(&*s, free_vars, Precedence::Top, new_depth));
                let binding_str = var_types
                    .zip(depth..)
                    .map(|(binding, new_depth)| format!("v{} : {}", new_depth, binding))
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
        };

        let my_precedence = match judg_tree {
            JudgmentTree::UinNone => Precedence::Var,
            JudgmentTree::Prim(_) => Precedence::Var,
            JudgmentTree::VarUuid(_, _) => Precedence::Var,
            JudgmentTree::BoundVar(_, _) => Precedence::Var,
            JudgmentTree::Fun(_) => Precedence::Fun,
            JudgmentTree::Pi(_, _) => Precedence::Top,
            JudgmentTree::Lam(_, _) => Precedence::Top,
            JudgmentTree::App(_) => Precedence::App,
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
            "Pi |v0 : U, v1 : U -> U| (Pi |v3 : U| v3) -> v1 v0 -> v1 v0"
        );

        let test4: Judgment<(), ()> = term!(Lam | T: U, t: T | t);
        assert_eq!(
            tree_to_string(&judgment_to_tree(test4)),
            "Lam |v0 : U, v1 : v0| v1"
        );
    }
}
