import { Server, pi_to_json, json_kind } from "./js_server.ts";
export let server = new Server(5000, 8080);
export const plus = Promise.resolve(async (a) =>
  async (b) => Promise.resolve(a + b)
);

// 5 case
export const var_0 = Promise.resolve(5n);
server.register_top_level(await var_0, "var_0", json_kind("Int"));


// plus_3
export const var_1 = Promise.resolve(async (var_4659)=>(await (await plus)(3n))(var_4659)
);
server.register_top_level(await var_1, "var_1", pi_to_json(json_kind("Int"), json_kind("Int")));


// apply_23 case:
export const var_2 = Promise.resolve(async (var_3)=>(var_3)(23n)
);
server.register_top_level(await var_2, "var_2", pi_to_json(pi_to_json(json_kind("Int"), json_kind("Int")), json_kind("Int")));


// tests//////////////////////////
// should be a promise object
console.log(var_0)
// how we should use var_0, prints 5n
console.log(await var_0)

//should print out a promise object
console.log(var_1)
// how we should use var_1 
export const var_210 = (await var_1)(await var_0);
// prints out 8
console.log(await var_210)



//26
export const var_232 = (await var_2)(await var_1);
console.log(await var_232)
