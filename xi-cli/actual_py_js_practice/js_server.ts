// Start listening on port 8080 of localhost.
const server = Deno.listen({ port: 8080 });
console.log(`HTTP webserver running.  Access it at:  http://localhost:8080/`);

// Connections to the server will be yielded up as an async iterable.
for await (const conn of server) {
  // In order to not be blocking, we need to handle each connection individually
  // in its own async function.
  (async () => {
    // This "upgrades" a network connection into an HTTP connection.
    const httpConn = Deno.serveHttp(conn);
    // Each request sent over the HTTP connection will be yielded as an async
    // iterator from the HTTP connection.
    for await (const requestEvent of httpConn) {
      let response;

      if (requestEvent.request.method === "POST") {
        let str = await requestEvent.request.text();
        console.log(`SERVER: got request with contents ${str}`);
        requestEvent.respondWith(
          new Response(response, {
            status: 200,
          }),
        );
      } else {
        requestEvent.respondWith(
          new Response(response, {
            status: 404,
          }),
        );
      }
    }
  })();
}
