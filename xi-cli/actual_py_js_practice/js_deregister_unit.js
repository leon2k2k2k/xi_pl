import { Server, pi, prim, u, freevar } from "./js_server_beta.ts";
export let server = new Server(5000, 8080);
export const plus = Promise.resolve(async (a) =>
    async (b) => Promise.resolve(a + b)
);

// this is Pi|T: U| T -> T
let UnitType = pi(u(), pi(freevar(0, u()), freevar(0, u()), 1), 0);

// now we want to apply this to T = String
let StrType = prim("Str", u());

export let var_0 = Promise.resolve(await server.deregister_top_level("var_0", UnitType));

console.log(await (await (await var_0)(StrType))("hello"));
