import { Server, pi, prim, u, freevar } from "./js_server_beta.ts";
export let server = new Server(5000, 8080);
export const plus = Promise.resolve(async (a) =>
  async (b) => Promise.resolve(a + b)
);

let int = prim("Int", u());
let int_to_int = pi(int, int, 0);
let int_to_int______to_int = pi(int_to_int, int, 1);
let int_to______int_to_int = pi(int, int_to_int, 2);


// console.log(int_to_int);

// console.log(int_to______int_to_int);

// 100
export const var_0 = Promise.resolve(100n);
server.register_top_level(await var_0, "var_0", int);


// plus_5
export const var_1 = Promise.resolve(async (var_4659) => (await (await plus)(5n))(var_4659)
);
server.register_top_level(await var_1, "var_1", int_to_int);


// apply_20
export const var_2 = Promise.resolve(async (var_3) => (var_3)(20n)
);
server.register_top_level(await var_2, "var_2", int_to_int______to_int);

// plus
export const var_3 = Promise.resolve(async (var_371) => Promise.resolve(async (var_372) => (await (await plus)(var_371))(var_372)
)
);
server.register_top_level(await var_3, "var_3", int_to______int_to_int);


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
