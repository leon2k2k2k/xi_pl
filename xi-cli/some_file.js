export const Int = "Int";
export const UnitType = "UnitType";
export const String2 = "String2";
export const unit = "unit";

export const five = 5;

export const six = 6;

export function add(a) {
  return (b) => a + b;
}

function io_pure2(val) {
  return async () => val;
}

export function io_pure(_) {
  return io_pure2;
}

export function equals(a) {
  return (b) => {
    if (a === b) {
      return ((var_80056) => ((var_80057) => ((var_80058) => var_80057)));
    } else {
      return ((var_80056) => ((var_80057) => ((var_80058) => var_80058)));
    }
  };
}

export function int_to_string(a) {
  return a.toString();
}

export function concat_hello(a) {
  return a + "hello";
}

export async function console_input() {
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
  return async () => {
    Deno.writeAllSync(Deno.stdout, new TextEncoder().encode(out_str));
    Deno.writeAllSync(Deno.stdout, new Uint8Array([0x0a])); // newline
  };
}

export async function panic(out_str) {
  return () => {
    console_output(out_str)();
    throw "exception";
  };
}

export function YCombinator_please_accept_us(fn) {
  return async () =>
    ((x) => x(x))(
      (maker) => (...args) => fn(maker(maker))(...args),
    );
}
