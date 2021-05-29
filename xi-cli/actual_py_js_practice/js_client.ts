// this is to test if the py_server and handle http requests from js:

let value = 5;
let request_body = JSON.stringify({
  js_ident: value,
  value: 2,
});

let url = "http://localhost:5000";
let resp = await fetch(url, {
  method: "POST",
  headers: {
    "Content-Type": "application/json",
  },
  body: request_body,
});

// let resp = await fetch("http://localhost:5000/2/3");
// let text = await resp;
// console.log("CLIENT: received " + text + " from post");

// async post(value: string) {
//     console.log("CLIENT: posting with " + value);
//     let url = `http://localhost:${this.other_port}`;
//     let resp = await fetch(url, {
//       method: "POST",
//       body: value,
//     });
//     let text = await (await resp).text();
//     console.log("CLIENT: received " + text + " from post");
//     return text;
//   }
