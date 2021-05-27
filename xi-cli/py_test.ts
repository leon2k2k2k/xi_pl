import { json_kind, pi_to_json, Server } from "./server.ts";
export let server = new Server("py");

export let var_3 = await server.deregister_top_level("var_3", json_kind("Str"));

console.log(await var_3);
