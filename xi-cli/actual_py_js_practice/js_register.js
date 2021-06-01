import { Server, pi_to_json, json_kind } from "./js_server.ts";
export let server = new Server(5000, 8080);
export const plus = Promise.resolve(async (a) =>
  async (b) => Promise.resolve(a + b)
);

// 100
export const var_0 = Promise.resolve(100n);
server.register_top_level(await var_0, "var_0", json_kind("Int"));


// pluts_5
export const var_1 = Promise.resolve(async (var_4659)=>(await (await plus)(5n))(var_4659)
);
server.register_top_level(await var_1, "var_1", pi_to_json(json_kind("Int"), json_kind("Int")));


// apply_20
export const var_2 = Promise.resolve(async (var_3)=>(var_3)(20n)
);
server.register_top_level(await var_2, "var_2", pi_to_json(pi_to_json(json_kind("Int"), json_kind("Int")), json_kind("Int")));

// plus
export const var_3= Promise.resolve(async (var_371)=>Promise.resolve(async (var_372)=>(await (await plus)(var_371))(var_372)
    )
);
server.register_top_level(await var_3, "var_3", pi_to_json(json_kind("Int"), pi_to_json(json_kind("Int"), json_kind("Int"))));


// tests


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
