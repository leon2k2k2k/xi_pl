import { json_kind, pi_to_json, Server } from "../server.ts";

let server = new Server("js");

let int_type = json_kind("Int");

let var_0 = Promise.resolve(6n);

// let var_0 = await server.deregister_top_level(
//   "var_2",
//   pi_to_json(int_type, int_type),
// );

// console.log(await (await var_0)(5));
