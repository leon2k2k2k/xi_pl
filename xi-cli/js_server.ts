// this is the generated code for the server

// this helps encode Aplite Types as JSON object.
export function pi_to_json(left: any, right: any) {
  return { kind: { left: left, right: right } };
}

export function json_kind(type: any) {
  return { kind: type };
}

export class Server {
  port: number;
  other_port: number;
  register_id: number;
  registrations: Map<number, any>;
  var_registrations: Map<string, number>;
  constructor(port: number, other_port: number) {
    this.port = port;
    this.other_port = other_port;

    this.register_id = 0;
    this.registrations = new Map();
    this.var_registrations = new Map();
    // start new server in background
    this.new_server();
  }

  serialize(value: any, type: any) {
    if (type.kind == "Int") {
      return this.register_new(value);
    } else if (type.kind === "Str") {
      return this.register_new(value);
    } else {
      let arg_type = type.kind.left;
      let return_type = type.kind.right;
      let new_func = Promise.resolve(async (s: any) => {
        let x = await this.deserialize(s, arg_type);
        return this.serialize(await value(await x), return_type);
      });
      return this.register_new(new_func);
    }
  }

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

  async deserialize(value: any, type: any): Promise<any> {
    if (type.kind === "Int") {
      let request = JSON.stringify({
        js_ident: value,
      });
      return Promise.resolve(BigInt(await this.post(request)));
    } else if (type.kind === "Str") {
      let request = JSON.stringify({
        js_ident: value,
      });
      return Promise.resolve(await this.post(request));
    } else {
      let arg_type = type.kind.left;
      let return_type = type.kind.right;
      return async (x: any) => {
        let serialized_x = this.serialize(x, arg_type);
        let request = JSON.stringify({
          js_ident: value,
          value: serialized_x,
        });
        let return_id = JSON.parse(await this.post(request));
        return Promise.resolve(await this.deserialize(return_id, return_type));
      };
    }
  }

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
              let result_ident = this.registrations.get(body.js_ident);
              if (result_ident === undefined) {
                throw new Error("ident no found");
              }
              if (body.value === undefined) {
                response = await result_ident;
              } else {
                response = JSON.stringify(
                  await (await result_ident)(body.value),
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
