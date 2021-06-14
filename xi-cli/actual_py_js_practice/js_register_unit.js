import { Server, pi, prim, u, freevar, instantiate } from "./../js_server.ts";
export let server = new Server(8080, 5000);
export const plus = Promise.resolve(async (a) =>
    async (b) => Promise.resolve(a + b)
);

// this is Pi|T: U| T -> T
let UnitType = pi(u(), pi(freevar(0), freevar(0), 1), 0);

// now we want to apply this to T = String
let StrType = prim("StrType");

let StrTypeToStrType = instantiate(UnitType.right, StrType, 0);

export const var_0 = Promise.resolve(async (var_32) => Promise.resolve(async (var_33) => Promise.resolve(var_33)
));
server.register_top_level(await var_0, "var_0", UnitType);

console.log(await (await (await var_0)(StrType))("hello"));


// now let's try true and false.
// they have type Pi |T : Type, t : T| T -> T -> T
export const var_1 = Promise.resolve(async (var_55) => Promise.resolve(async (var_56) => Promise.resolve(async (var_57) => Promise.resolve(var_56)
)
)
);

let T_to_T = pi(freevar(0), freevar(0), 1);
let T_to_T_to_T = pi(freevar(0), T_to_T, 2);
let BoolType = pi(u(), T_to_T_to_T, 0);

server.register_top_level(await var_1, "var_1", BoolType);

console.log(await (await (await (await var_1)(StrType))("hello"))("world"))


export const var_2 = Promise.resolve(async (var_55) => Promise.resolve(async (var_56) => Promise.resolve(async (var_57) => Promise.resolve(var_57)
)
)
);

server.register_top_level(await var_2, "var_2", BoolType);

console.log(await (await (await (await var_2)(StrType))("hello"))("world"))
