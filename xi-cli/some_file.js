export const Int = "Int";
export const UnitType = "UnitType";
export const String2 = "String2";
export const unit = "unit";

export const five = Promise.resolve(5);

export const six = Promise.resolve(6);

export const add = Promise.resolve(async (a) =>
  async (b) => Promise.resolve(a + b)
);

export const io_pure = Promise.resolve(async (_) =>
  async (val) => async () => val
);

export const equals = Promise.resolve(async (a) =>
  async (b) => {
    if (a === b) {
      return Promise.resolve(
        (async (T) => (async (t) => (async (f) => t))),
      );
    } else {
      return Promise.resolve(
        (async (T) => (async (t) => (async (f) => t))),
      );
    }
  }
);

export const int_to_string = Promise.resolve(async (a) =>
  Promise.resolve(a.toString())
);

export const concat_hello = async (a) => Promise.resolve(a + "hello");

export const console_input = Promise.resolve(async () => {
  const input = [];
  const buf = new Uint8Array(1);
  while (buf[0] != 0x0a /* \n */) {
    const num_read = await Deno.read(Deno.stdin.rid, buf);
    if (num_read != 1) {
      throw "Failed to read from stdin";
    }
    input.push(buf[0]);
  }
  input.pop();
  return new TextDecoder().decode(new Uint8Array(input));
});

export const console_output = Promise.resolve(async (out_str) =>
  async () => {
    await Deno.writeAll(Deno.stdout, new TextEncoder().encode(out_str));
    await Deno.writeAll(Deno.stdout, new Uint8Array([0x0a])); // newline
  }
);

export async function panic(out_str) {
  return () => {
    console_output(out_str)();
    throw "exception";
  };
}

export const panic = Promise.resolve(async (out_str) =>
  async () => {
    await (await (await console_output)(out_str))();
    throw "exception";
  }
);

// probably doesn't work
export const YCombinator_please_accept_us = Promise.resolve(async (fn) =>
  async () =>
    (async (x) => x(x))(
      async (maker) => async (...args) => fn(maker(maker))(...args),
    );
)
