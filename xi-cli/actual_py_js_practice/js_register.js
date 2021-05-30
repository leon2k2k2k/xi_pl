import { Server, pi_to_json, json_kind } from "./js_server.ts";
export let server = new Server(5000, 8080);
export const plus = Promise.resolve(async (a) =>
  async (b) => Promise.resolve(a + b)
);
// // this works!
// export const var_0 = Promise.resolve(5n);
// server.register_top_level(await var_0, "var_0", json_kind("Int"));

export const var_1 = Promise.resolve(async (var_4659)=>(await (await plus)(3n))(var_4659)
);

server.register_top_level(await var_1, "var_1", pi_to_json(json_kind("Int"), json_kind("Int")));
