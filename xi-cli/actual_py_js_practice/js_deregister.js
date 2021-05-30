import { Server, pi_to_json, json_kind } from "./js_server.ts";
export let server = new Server(8080, 5000);


// // // this works!!
// export let var_0 = await server.deregister_top_level("var_0", json_kind("Int"));
// console.log(var_0)

export let var_1 = await server.deregister_top_level("var_1", pi_to_json(json_kind("Int"), json_kind("Int")));

export const var_294 = await (await var_1)(100n);

console.log( var_294)
