export const Int = "Int";

export const five = 5;

export const six = 6;

export function add(a) {
  return (b) => a + b;
}

export function int_to_string(a) {
  return a.toString();
}

export function concat_hello(a) {
  return a + "hello";
}

export function console_input() {
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

export function console_output(out_str) {
  return () => {
    Deno.writeAllSync(Deno.stdout, new TextEncoder().encode(out_str));
    Deno.writeAllSync(Deno.stdout, new Uint8Array([0x0a])); // newline
  };
}
