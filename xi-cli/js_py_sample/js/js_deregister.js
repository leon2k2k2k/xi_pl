import { Server, pi, prim, u, freevar } from "./../../js_server.ts";
export let server = new Server(5000, 8080);

// this usage of things on this side should be the same as the 
// js_register side.

let int = prim("Int", u());
let int_to_int = pi(int, int, 0);
let int_to_int______to_int = pi(int_to_int, int, 1);
let int_to______int_to_int = pi(int, int_to_int, 2);


//  100
export let var_0 = Promise.resolve(await server.deregister_top_level("var_0", int));

// plus_5
export let var_1 = Promise.resolve(await server.deregister_top_level("var_1",
    int_to_int));

// // apply_20
export let var_2 = Promise.resolve(await server.deregister_top_level("var_2",
    int_to_int______to_int));

// plus
export let var_3 = Promise.resolve(await server.deregister_top_level("var_3",
    int_to______int_to_int));



// test0
// 100
console.log(await var_0)

// test1
// 15
console.log(await (await var_1)(10n))

// 105
export const var_210 = (await var_1)(await var_0);
console.log(await var_210)

// test2
// 25
export const var_232 = (await var_2)(await var_1);
console.log(await var_232)

// test3
// 99
export const var_329 = (await (await var_3)(95n))(4n);
console.log(await var_329)
