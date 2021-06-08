export async function tictactoe(play: any, init_board: any, init_square: any) {
  // console.log("hey wer're playing tictactoe");
  let board = init_board;
  let square = init_square;
  // Start listening on port 8080 of localhost.
  const server = Deno.listen({ port: 8080 });
  console.log(
    `HTTP webserver running.  Access it at:  http://localhost:8080/`,
  );

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
        // The native HTTP server uses the web standard `Request` and `Response`
        // objects.
        if (requestEvent.request.method === "POST") {
          const body = await requestEvent.request.text();
          let pos = BigInt(JSON.parse(body));

          let play_result =
            await ((await (await (await (await play)(board))(pos))(square))());
          console.log(play_result);
          let response;
          if (play_result.message === "invalid") {
            response = JSON.stringify({ message: "invalid" });
          } else {
            board = play_result.board;
            square = play_result.square;
            let resp = {
              message: "move",
              square: play_result.square,
              pos: Number(pos),
              won: play_result.won,
            };
            response = JSON.stringify(resp);
          }
          requestEvent.respondWith(
            new Response(response, {
              status: 200,
            }),
          );
        } else if (requestEvent.request.method === "GET") {
          board = init_board;
          square = init_square;
          const body = await Deno.readTextFile(
            "./programs/tictactoe/tictactoe.html",
          );

          let body2 = new TextEncoder().encode(body);
          requestEvent.respondWith(
            new Response(body2, {
              status: 200,
            }),
          );
        } else {
          requestEvent.respondWith(
            new Response("File not found", {
              status: 404,
            }),
          );
        }
      }
    })();
  }
}
