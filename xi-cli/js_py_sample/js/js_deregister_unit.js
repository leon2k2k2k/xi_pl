import { Server, pi, prim, u, freevar } from "./../../js_server.ts";
export let server = new Server(5000, 8080);
export const plus = Promise.resolve(async (a) =>
    async (b) => Promise.resolve(a + b)
);

// this is Pi|T: U| T -> T
let UnitType = pi(u(), pi(freevar(0), freevar(0), 1), 0);

// now we want to apply this to T = String
let StrType = prim("Str");
let int = prim("Int");

let int_to_int = pi(int, int, 0);

export let var_0 = Promise.resolve(await server.deregister_top_level("var_0", UnitType));

// export const var_1 = Promise.resolve(async (var_4659) => (await (await plus)(5n))(var_4659)
// );
// let plus_5 = (await (await var_0)(int_to_int))(await var_1);

// console.log(await (await plus_5)(6n))

let T_to_T = pi(freevar(0), freevar(0), 1);
let T_to_T_to_T = pi(freevar(0), T_to_T, 2);
let BoolType = pi(u(), T_to_T_to_T, 0);

export let var_1 = Promise.resolve(await server.deregister_top_level("var_1", BoolType))

// expect "hello"
console.log(await (await (await (await var_1)(StrType))("hello"))("world"))

export let var_2 = Promise.resolve(await server.deregister_top_level("var_2", BoolType))

// expect "world"
console.log(await (await (await (await var_2)(StrType))("hello"))("world"))
