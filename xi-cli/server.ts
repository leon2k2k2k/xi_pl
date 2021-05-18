// this is the generated code for the server

// this helps encode Aplite Types as JSON object.
export function pi_to_json(left: any, right: any) {
  return { kind: { left: left, right: right } };
}

export function json_kind(type: any) {
  return { kind: type };
}

export function thing(id: any) {
  return { kind: "function", value: id.toString() };
}

export class Server {
  type: string;
  port: number;
  other_port: number;
  register_id: number;
  registrations: Map<string, any>;

  constructor(type: string) {
    this.type = type;
    if (type === "py") {
      this.port = 8081;
      this.other_port = 8080;
    } else if (type === "js") {
      this.port = 8080;
      this.other_port = 8081;
    } else {
      throw "Invalid server type";
    }

    this.register_id = 0;
    this.registrations = new Map();
    // start new server in background
    this.new_server();
  }

  serialize(value: any, type: any) {
    if (type.kind === "Int") {
      return { kind: "Int", value: value.toString() };
    } else {
      let arg_type = type.kind.left;
      let return_type = type.kind.right;
      let new_func = async (s: any) => {
        let x = this.deserialize(s, arg_type);
        console.log("aldfja;sldf" + x);
        return this.serialize(await value(x), return_type);
      };
      return { type: "function", value: this.register_new(new_func) };
    }
  }

  // serialize1(value: any, type: string): string {
  //   if (type === "int") {
  //     return this.register_new(value);
  //   } else if (type === "int->int") {
  //     let new_function = (s: string) => {
  //       let x = parseInt(s);
  //       return value(x).toString();
  //     };

  //     return this.register_new(new_function);
  //   } else if (type === "int->int->int") {
  //     let new_function = (s: string) => {
  //       let x = parseInt(s);
  //       return this.serialize1(value(x), "int->int");
  //     };

  //     return this.register_new(new_function);
  //   } else if (type === "(int->int)->int") {
  //     let new_function = (s: string) => {
  //       let x = this.deserialize1(s, "int->int");
  //       return this.serialize1(value(x), "int");
  //     };

  //     return this.register_new(new_function);
  //   }
  //   throw "unexpceted type";
  // }

  register_new(value: any) {
    let name = this.register_id.toString();

    this.register(name, value);
    this.register_id += 1;
    return name;
  }

  register(name: string, value: any) {
    console.assert(typeof name === "string", "should be register string");
    console.log("registering " + name);
    this.registrations.set(name, value);
  }

  deserialize(thing: any, type: any): any {
    if (type.kind === "Int") {
      return BigInt(parseInt(thing.value));
    } else {
      let arg_type = type.kind.left;
      let return_type = type.kind.right;
      return async (x: any) => {
        let serialized_x = this.serialize(x, arg_type);
        let request = JSON.stringify({
          js_ident: thing.value,
          value: serialized_x,
        });
        let return_id = JSON.parse(await this.post(request));
        return this.deserialize(return_id, return_type);
      };
    }
  }

  // async deserialize1(name: string, type: string): Promise<any> {
  //   if (type === "int") {
  //     return await parseInt(await this.post(name));
  //   } else if (type === "int->int") {
  //     return async (x: number) => {
  //       let serialized_x = this.serialize(x, "int");
  //       let request = JSON.stringify({ ident: name, value: serialized_x });
  //       let return_id = await this.post(request);
  //       return this.deserialize1(return_id, "int");
  //     };
  //   } else if (type === "int->int->int") {
  //     return async (x: any) => {
  //       let serialized_x = this.serialize(x, "int");
  //       let request = JSON.stringify({ ident: name, value: serialized_x });
  //       let return_id = await this.post(request);
  //       return this.deserialize1(return_id, "int->int");
  //     };
  //   } else if (type === "(int->int)->int") {
  //     return async (x: any) => {
  //       let serialized_x = this.serialize(x, "int->int");
  //       let request = JSON.stringify({ ident: name, value: serialized_x });
  //       let return_id = await this.post(request);
  //       return this.deserialize1(return_id, "int");
  //     };
  //   }
  // }

  async post(value: string) {
    console.log("posting with " + value);
    let url = `http://localhost:${this.other_port}`;
    let resp = await fetch(url, {
      method: "POST",
      body: value,
    });
    console.log("ok I finished the fetch");
    let text = await (await resp).text();
    return text;
  }

  async new_server() {
    // Start listening on port 8080/8081 of localhost.
    const server = Deno.listen({ port: this.port });
    console.log(
      `server running.  Access it at:  http://localhost:${this.port}/`,
    );

    // Connections to the server will be yielded up as an async iterable.
    for await (const conn of server) {
      // This "upgrades" a network connection into an HTTP connection.
      const httpConn = Deno.serveHttp(conn);
      for await (const requestEvent of httpConn) {
        let response;

        if (requestEvent.request.method === "POST") {
          let str = await requestEvent.request.text();
          console.log(`I got a request for ${str}`);
          let body = JSON.parse(str);
          console.log(body);
          // // I need to look up the name of the function:
          let result_ident = this.registrations.get(body.js_ident);
          if (result_ident === undefined) {
            throw new Error("ident no found");
          }

          response = JSON.stringify(await result_ident(body.value));

          console.log(`I responded with ${response}`);
          requestEvent.respondWith(
            new Response(response, { status: 200 }),
          );
        } else {
          requestEvent.respondWith(
            new Response(response, { status: 404 }),
          );
        }
      }
    }
  }
}
