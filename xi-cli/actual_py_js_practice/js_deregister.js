import { Server, pi_to_json, json_kind } from "./js_server.ts";
export let server = new Server(8080, 5000);

// this usage of things on this side should be the same as the 
// js_register side.

//  100
export let var_0 = Promise.resolve(await server.deregister_top_level("var_0", json_kind("Int")));

// // plus_5
export let var_1 =  Promise.resolve(await server.deregister_top_level("var_1", pi_to_json(json_kind("Int"), json_kind("Int"))));

// // apply_20
export let var_2 =  Promise.resolve(await server.deregister_top_level("var_2", pi_to_json(pi_to_json(json_kind("Int"), json_kind("Int")), json_kind("Int"))));

// plus
export let var_3 =  Promise.resolve(await server.deregister_top_level("var_3", pi_to_json(json_kind("Int"), pi_to_json(json_kind("Int"), json_kind("Int")))));



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
