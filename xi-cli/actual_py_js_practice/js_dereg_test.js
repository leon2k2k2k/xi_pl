import { Server, pi, prim, u, freevar } from "./js_server_beta.ts";
export let server = new Server(8080, 5000);
export const plus = Promise.resolve(async (a) =>
    async (b) => Promise.resolve(a + b)
);


export let var_313 = Promise.resolve(await server.deregister_top_level("var_313", pi(u(), pi(freevar(375n, u()), freevar(375n, u()), 376n), 375n)));
export const var_335 = (await (await var_313)(prim("Str")))("hello");

console.log(await var_335)
