// this is the generated code for the server

// this helps encode Aplite Types as JSON object.
export function pi_to_json(left: any, right: any) {
  return { kind: { left: left, right: right } };
}

export function json_kind(type: any) {
  return { kind: type };
}



export class Server {
  type: string;
  port: number;
  other_port: number;
  register_id: number;
  registrations: Map<number, any>;
  var_registrations: Map<string, number>;
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
      this.var_registrations = new Map();
      // start new server in background
      this.new_server();

  }

  

  serialize(value: any, type: any) {
      if (type.kind == "Int") {
          return this.register_new(value);
      }
      else {
          let arg_type = type.kind.left;
          let return_type = type.kind.right;
          let new_func = async (s: any) => {
              let x = this.deserialize(s, arg_type);
              return this.serialize(await (await value)(x), return_type);
          };
          return this.register_new(new_func);
      }
  }


  // register a new var in var_registrations with the register_id this.register_id, then increase this.register_id by 1.
  register_new(value: any) {
      let current_register_id = this.register_id
      this.register(current_register_id, value);
      this.register_id += 1;
      return current_register_id;
  }

  // this is when the value is actually a exported var, and other would like to access it by its name.
  // so we log the var_name, register_id pair into var_registration.
  register_var(value: any, var_name: string, var_type: any) {
      let register_id = this.serialize(value, var_type);
      console.log("registering " + var_name + " with registration number " + register_id)
      this.var_registrations.set(var_name, register_id);

  }

  register(register_id: number, value: any) {
      this.registrations.set(register_id, value);
  }


  // deregister_var is the opposite of register_var, it is just get the register_id, then deserialize that:
  async deregister_var(value: any, var_type: any) {
      let request = JSON.stringify({
          request_type: "reg_id",
          var_name: value,
      });
      let register_id = JSON.parse(await this.post(request));
      return this.deserialize(register_id, var_type)
  }

  async deserialize(value: any, type: any) {
      if (type.kind === "Int") {
          let request = JSON.stringify({
              js_ident: value,
          });
          return JSON.parse(await this.post(request));
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

                  if (body.request_type === "reg_id") {
                      let reg_id = this.var_registrations.get(body.var_name)
                      response = JSON.stringify(reg_id);
                  } else {
                  // // I need to look up the name of the function:
                  let result_ident = this.registrations.get(body.js_ident);
                  if (result_ident === undefined) {
                      throw new Error("ident no found");
                  }
                  if (body.value === undefined) {
                      let big_int = await result_ident;                      
                      response = big_int;
                  } else {
                  response = JSON.stringify(await result_ident(body.value));
              }}
      

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
