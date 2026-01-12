import { Server, pi, prim, u, freevar } from "./../../xi-cli/js_server.ts";
export let server = new Server(5000, 8080);
export let var_1 = Promise.resolve(await server.deregister_top_level("var_1", prim("Int")));
