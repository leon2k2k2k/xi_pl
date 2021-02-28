// ///Want to define brackets in type theory
// use crate::judgment::*;
// use free_var::FreeVar;
// use term_macro::term;

// ///Parametricity bracket:
// pub fn para_bra(judgment: Judgment<(), ()>) -> Judgment<(), ()> {
//     match judgment {
//         Judgment::UInNone => term!(Lam |A : U| A -> U),
//         Judgment::Prim(_) => panic!("not too sure what to do with primitives"),
//         Judgment::FreeVar(_, _) => panic!("not too sure what to do with FreeVar"),
//         Judgment::Pi(var_type, expr) => {
//             term!(Lam |f : {Judgment::Pi(var_type.clone(), expr)}|
//             (Pi | x : {*var_type.clone()}, xr : {para_bra(*var_type)} x | {para_bra(*expr)} (f x)))
//         }
//         Judgment::Lam(var_type, expr) => {
//             term!(Lam |x : {*var_type}, xr : {para_bra(*var_type)} x| {para_bra(*expr)})
//         }
//         Judgment::BoundVar(int, expr) => todo!("we need some sort of global memory here!!!"),
//         Judgment::Application(func, elem) => {
//             term!({para_bra(*func)} {*elem} {para_bra(*elem)} )
//         }
//         Judgment::Metadata(_, _) => todo!("not too sure"),
//     }
// }
