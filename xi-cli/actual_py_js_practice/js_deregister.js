import { Server, pi_to_json, json_kind } from "./js_server.ts";
export let server = new Server(8080, 5000);

// this usage of things on this side should be the same as the 
// js_register side.

//  5 case
export let var_0 = Promise.resolve(await server.deregister_top_level("var_0", json_kind("Int")));

// // plus_3
// export let var_1 =  Promise.resolve(await server.deregister_top_level("var_1", pi_to_json(json_kind("Int"), json_kind("Int"))));

// // apply_23
// export let var_2 =  Promise.resolve(await server.deregister_top_level("var_2", pi_to_json(pi_to_json(json_kind("Int"), json_kind("Int")), json_kind("Int"))));



// should be a promise object
console.log(var_0)
// this shouold porints 5n
console.log(await var_0)

var_0 = Promise.resolve(await server.deregister_top_level("var_0", json_kind("Int")));
console.log(await var_0)

// console.log(var_1)

// // how we should use var_1 
// export const var_210 = (await var_1)(await var_0);
// // prints out 8
// console.log(await var_210)



// //usage
// export const var_232 = (await var_2)(await var_1);
// console.log(await var_232)
