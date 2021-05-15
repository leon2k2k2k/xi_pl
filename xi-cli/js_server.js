// this is the thing that registers the names of the vars in backend as well as
// variables that was created.
var server_registrations = new Map();
// this function is getting called on the backend.js file, to keep the
// server_registrations of the two files the same.
export async function update_server(name, func) {
  server_registrations.set(name, func);
  console.log("registering func" + server_registrations);
}

// each client function that needs a server function would send a HTTP request
// to this server and with the body
// JSON.stringify({
// ident: func
// value: gives the list of all the varibles the frontend is going to need.
//})

export async function run_server() {
  // Start listening on port 8080 of localhost.
  const server = Deno.listen({ port: 8080 });
  console.log(`server running.  Access it at:  http://localhost:8080/`);

  // Connections to the server will be yielded up as an async iterable.
  for await (const conn of server) {
    // In order to not be blocking, we need to handle each connection individually
    // in its own async function.
    (async () => {
      // This "upgrades" a network connection into an HTTP connection.
      const httpConn = Deno.serveHttp(conn);
      for await (const requestEvent of httpConn) {
        let response;

        if (requestEvent.request.method === "POST") {
          let str = await requestEvent.request.text();
          console.log(str);
          let body = JSON.parse(str);
          console.log(server_registrations);
          // I need to look up the name of the function:
          let ident = body.ident;
          let result_ident = server_registrations.get(ident);
          if (result_ident === undefined) {
            throw new Error("ident no found");
          }
          console.log(result_ident);
          // now we check that have they send any values over too:
          let value = body.value;
          console.log(value);
          if (value === undefined) {
            response = result_ident;
          } else {
            // we also also send a http request:
            let new_body = JSON.stringify({
              ident: body.value,
            });
            let resp = fetch(
              "http://localhost:8081",
              {
                method: "POST",
                body: new_body,
              },
            );
            let a = await resp;
            console.log(a);
            let value = await a.text();
            response = result_ident(value);
            console.log(response);
          }
          requestEvent.respondWith(
            new Response(response, { status: 200 }),
          );
        } else {
          requestEvent.respondWith(
            new Response(response, { status: 404, message: "error" }),
          );
        }
      }
    })();
  }
}
