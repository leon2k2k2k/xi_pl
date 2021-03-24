function io_bind2<T, U>(arg: () => T) {
  return (func: ((_: T) => (() => U))) => {
    const value = arg();
    return func(value);
  };
}

export function io_bind(_: any) {
  return (_: any) => io_bind2;
}

export function app<T, U>(arg: T) {
  return (func: ((_: T) => U)) => {
    return func(arg);
  };
}

function io_pure2<T>(val: T) {
  return () => val;
}

export function io_pure(_: any) {
  return io_pure2;
}

export function console_input(): string {
  const input = [];
  const buf = new Uint8Array(1);
  while (buf[0] != 0x0a /* \n */) {
    const num_read = Deno.readSync(Deno.stdin.rid, buf);
    if (num_read != 1) {
      throw "Failed to read from stdin";
    }
    input.push(buf[0]);
  }
  input.pop();
  return new TextDecoder().decode(new Uint8Array(input));
}

export function console_output(out_str: string): () => void {
  return () => {
    Deno.writeAllSync(Deno.stdout, new TextEncoder().encode(out_str));
    Deno.writeAllSync(Deno.stdout, new Uint8Array([0x0a])); // newline
  };
}

function promise_bind2<T, U>(arg: Promise<T>) {
  return (func: ((_: T) => Promise<U>)) => arg.then(func);
}

export function promise_bind(_: any) {
  return (_: any) => promise_bind2;
}

function promise_pure2<T>(val: T) {
  return Promise.resolve(val);
}

export function promise_pure(_: any) {
  return promise_pure;
}

export function id(val: any) {
  return val;
}

function add1(x: number, y: number) {
  return x + y;
}

/*
let add1: int -> int = js.add1 
foo.bar

let two = js.add1 1








a = [1, 2, 3]
a.append(4)

vec![1, 2, 3]

a = 
val add1 5
 */
