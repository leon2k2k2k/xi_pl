import { Server, pi, prim, u, freevar } from "./../js_server.ts";
export let server = new Server(8080, 5000);

// export let var_5 = Promise.resolve(await server.deregister_top_level("var_5", pi(u(), pi(freevar(261), freevar(261), 262), 261)));
// export const var_28 = Promise.resolve(async (var_263) => Promise.resolve(var_263)
// );
// export const var_39 = (await (await var_5)(pi(prim("Int"), prim("Int"), 264)))(await var_28);
// export const var_214 = (await var_39)(5n);

// console.log(await var_214);


export let var_6 = Promise.resolve(await server.deregister_top_level("var_6", pi(u(), pi(freevar(578), pi(freevar(578), freevar(578), 580), 579), 578)));
export let var_51 = Promise.resolve(await server.deregister_top_level("var_51", pi(u(), pi(freevar(581), pi(freevar(581), freevar(581), 583), 582), 581)));
export const var_93 = (await (await (await var_6)(prim("Str")))("hello"))("world");
export const var_320 = (await (await (await var_51)(prim("Str")))("hello"))("world");

console.log(await var_93);
console.log(await var_320);
