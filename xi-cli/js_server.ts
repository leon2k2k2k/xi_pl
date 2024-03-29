// we need to take a type in Judgment into a javascript object.
// we can represent it as a json object.

// examples:
// U -> {value: U} // I don't think they will appear
// Int -> {Kind: prim, value: Int} or just {kind: Int}
// Pi|x : A| B -> {Kind: Pi, var_id: x, {left: A, right: B}}
// F A -> {Kind: App, {left: F, right: A}}

// so the rhs are generated by primitives, Pi and application.
// let's not worry about app and lam right now.
// let's not use scoped and unscoped.

// I guess want to get
// Pi |T : U| T -> T working.

export function u(): any {
  return { kind: "U", value: "U" };
}
export function prim(x: string): any {
  return { kind: "prim", value: x };
}

export function pi(left: any, right: any, var_id: any): any {
  return { kind: "pi", left: left, right: right, var_id: var_id };
}

// let's not worry about app right now.
// function app(left: any, right: any): any {
//   return { kind: "app", left: left, right: right };
// }

export function freevar(index: any): any {
  return { kind: "free_var", index: index };
}
export function instantiate(sexpr: any, expr: any, var_index: any): any {
  // console.log("in instantiate, the sexpr is", sexpr, "expr is ", expr);
  if (sexpr.kind === "prim") {
    return sexpr;
  } else if (sexpr.kind === "U") {
    return sexpr;
  } else if (sexpr.kind === "pi") {
    return pi(
      instantiate(sexpr.left, expr, var_index),
      instantiate(sexpr.right, expr, var_index),
      sexpr.var_id,
    );
  } else if (sexpr.kind === "free_var") {
    if (sexpr.index == var_index) {
      return expr;
    } else {
      return sexpr;
    }
  }
}

export function value_and_type(value: any, type: any): any {
  return { value: value, type: type };
}
// this is Pi|T: U| T -> T
let UnitType = pi(u(), pi(freevar(0), freevar(0), 1), 0);

// now we want to apply this to T = String
let StrType = prim("Str");

// let StrTypeToStrType = instantiate(UnitType.right, StrType, 0);

///////////////////////////////////////////////////////////////////////////////////////////
// This is the server.
////////////////////////
export class Server {
  port: number;
  other_port: number;
  register_id: number;
  registrations: Map<number, any>;
  var_registrations: Map<string, any>;
  constructor(port: number, other_port: number) {
    this.port = port;
    this.other_port = other_port;

    this.register_id = 0;
    this.registrations = new Map();
    this.var_registrations = new Map();
    // start new server in background
    this.new_server();
  }

  // the behavior is governed by the type!!
  serialize(value: any, type: any) {
    if (type.kind === "prim") {
      if (type.value === "Int") {
        return this.register_new(value);
      } else if (type.value === "Str") {
        return this.register_new(value);
      } else {
        Error;
      }
    } else if (type.kind === "pi") {
      let var_type = type.left;
      let return_type = type.right;
      let var_id = type.var_id;
      // s is only going to be the term, not the type, we don't need the type of s!.
      let new_func = Promise.resolve(async (s: any) => {
        // console.log("serialized: s is", s, "and var_type is", var_type);
        let x = await this.deserialize(s, var_type);
        // let x_type = (await value_and_type).type;
        // console.log("serialized: x is", x);
        // console.log("in serialize, return_type is", return_type);
        // console.log("also we have value", value, "and type", type);
        // let instantiated = instantiate(return_type, x, var_id);
        // console.log("this is the instantiated:", instantiated);
        return this.serialize(
          await value(await x),
          instantiate(return_type, x, var_id),
        );
      });
      return this.register_new(new_func);
    } else if (type.kind === "free_var") {
      Error;
    } else if (type.kind === "U") {
      return value;
    }
  }

  // serialize(value: any, type: any) {
  //   if (type.kind == "Int") {
  //     return this.register_new(value);
  //   } else if (type.kind === "Str") {
  //     return this.register_new(value);
  //   } else {
  //     let arg_type = type.kind.left;
  //     let return_type = type.kind.right;
  //     let new_func = Promise.resolve(async (s: any) => {
  //       let x = await this.deserialize(s, arg_type);
  //       return this.serialize(await value(await x), return_type);
  //     });
  //     return this.register_new(new_func);
  //   }
  // }

  // register a new var in var_registrations with the register_id this.register_id, then increase this.register_id by 1.
  register_new(value: any) {
    const current_register_id = this.register_id;
    this.registrations.set(current_register_id, value);
    console.log(`SERVER: registered id ${current_register_id}`);

    this.register_id += 1;
    return current_register_id;
  }

  // this is when the value is actually a exported var, and other would like to access it by its name.
  // so we log the var_name, register_id pair into var_registration.
  register_top_level(value: any, var_name: string, var_type: any) {
    let register_id = this.serialize(value, var_type);
    console.log(
      "SERVER: registered top level " + var_name + " with id " + register_id,
    );
    this.var_registrations.set(var_name, register_id);
  }

  // deregister_top_level is the opposite of register_top_level, it is just get the register_id, then deserialize that:
  async deregister_top_level(value: any, var_type: any): Promise<any> {
    let request = JSON.stringify({
      request_type: "reg_id",
      var_name: value,
    });
    let register_id = JSON.parse(await this.post(request));
    return await this.deserialize(register_id, var_type);
  }

  // deseralize need to send the type of x across, and it should also
  // return x and x_type.
  // only need to send the type of arg over.

  async deserialize(value: any, type: any): Promise<any> {
    // console.log("in deserialize, the type is", type);
    if (type.kind === "prim") {
      if (type.value === "Int") {
        let request = JSON.stringify({
          remote_ident: value,
        });
        return Promise.resolve(BigInt(await this.post(request)));
      } else if (type.value === "Str") {
        let request = JSON.stringify({
          remote_ident: value,
        });
        return Promise.resolve(await this.post(request));
      }
    } else if (type.kind === "U") {
      return value;
    } else if (type.kind === "pi") {
      let var_type = type.left;
      let return_type = type.right;
      let var_id = type.var_id;
      return async (x: any) => {
        // console.log("in deserialized, x is", x);
        // console.log("in deserialized, var_type is", var_type);
        let serialized_x = this.serialize(x, var_type);
        // console.log(serialized_x);
        let request = JSON.stringify({
          remote_ident: value,
          arg: serialized_x,
        });
        // console.log("x is :::::::::", x);
        // console.log("return_type is :::::::", return_type);
        let return_id = JSON.parse(await this.post(request));
        return Promise.resolve(
          await this.deserialize(
            return_id,
            instantiate(return_type, x, var_id),
          ),
        );
      };
    }
  }
  // // need to return a pair of x and x_type
  // async deserialize_ol(value: any, type: any): Promise<any> {
  //   if (type.kind === "Int") {
  //     let request = JSON.stringify({
  //       js_ident: value,
  //     });
  //     return Promise.resolve(BigInt(await this.post(request)));
  //   } else if (type.kind === "Str") {
  //     let request = JSON.stringify({
  //       js_ident: value,
  //     });
  //     return Promise.resolve(await this.post(request));
  //   } else {
  //     let arg_type = type.kind.left;
  //     let return_type = type.kind.right;
  //     return async (x: any) => {
  //       let serialized_x = this.serialize(x, arg_type);
  //       let request = JSON.stringify({
  //         js_ident: value,
  //         value: serialized_x,
  //       });
  //       let return_id = JSON.parse(await this.post(request));
  //       return Promise.resolve(await this.deserialize(return_id, return_type));
  //     };
  //   }
  // }

  async post(value: string) {
    console.log("CLIENT: posting with " + value);
    let url = `http://localhost:${this.other_port}`;
    let resp = await fetch(url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: value,
    });
    let text = await (await resp).text();
    console.log("CLIENT: received " + text + " from post");
    return text;
  }

  async new_server() {
    // Start listening on port 8080/8081 of localhost.
    const server = Deno.listen({ port: this.port });
    console.log(
      `SERVER: server running at http://localhost:${this.port}/`,
    );

    // Connections to the server will be yielded up as an async iterable.
    for await (const conn of server) {
      // This "upgrades" a network connection into an HTTP connection.
      (async () => {
        const httpConn = Deno.serveHttp(conn);
        for await (const requestEvent of httpConn) {
          let response;

          if (requestEvent.request.method === "POST") {
            let str = await requestEvent.request.text();
            console.log(`SERVER: got request with contents ${str}`);
            let body = JSON.parse(str);

            if (body.request_type === "reg_id") {
              let reg_id = this.var_registrations.get(body.var_name);
              response = JSON.stringify(reg_id);
            } else {
              // // I need to look up the name of the function:
              let result_ident = this.registrations.get(body.remote_ident);
              if (result_ident === undefined) {
                throw new Error("ident no found");
              }
              if (body.arg === undefined) {
                response = await result_ident;
              } else {
                response = JSON.stringify(
                  await (await result_ident)(body.arg),
                );
              }
            }

            console.log(`SERVER: responded with ${response}`);
            requestEvent.respondWith(
              new Response(response, { status: 200 }),
            );
          } else {
            requestEvent.respondWith(
              new Response(response, { status: 404 }),
            );
          }
        }
      })();
    }
  }
}
