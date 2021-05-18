import { pi_to_json, Server } from "./server.ts";
let server = new Server("py");

let json_int = { kind: "Int" };
let json_int_to_int = pi_to_json(json_int, json_int);

let json_int_to____int_to_int = pi_to_json(json_int, json_int_to_int);
let json_int_to_int_____to_int = pi_to_json(json_int_to_int, json_int);

// let five = await server.deserialize("0", json_int);
// console.log(five);
let thing = { kind: "function", value: "0" };
// let plus_3 = server.deserialize(thing, json_int_to_int);
// console.log(await plus_3(10000n));

// let plus = await server.deserialize(thing, json_int_to____int_to_int);
// console.log(await (await plus(3))(5));

// let plus = <any> await server.request("0", "int->int->int")
// let eight = await (await plus(3))(5);
let apply3 = await server.deserialize(thing, json_int_to_int_____to_int);

console.log(await apply3((x: any) => x + 5n));
