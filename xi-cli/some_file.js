export const Int = "Int";
export const UnitType = "UnitType";
export const String2 = "String2";
export const unit = "unit";

export const five = Promise.resolve(5);

export const six = Promise.resolve(6);

function pi_to_json(left, right) {
  return { kind: { left: left, right: right } };
}

function json_kind(type) {
  return { kind: type };
}

let int = json_kind("Int");
let str = json_kind("Str");
// we need a context because we have to call all the variables at once
function js_to_aplite(var_, var_type) {
  function js_to_aplite_helper(var_, var_type, ctx) {
    // console.log(ctx);
    if (var_type.kind === "Int") {
      if (ctx.length == 0) {
        return Promise.resolve(var_);
      } else {
        return Promise.resolve(var_.apply(null, ctx));
      }
    } else if (var_type.kind === "Str") {
      if (ctx.length == 0) {
        return Promise.resolve(var_);
      } else {
        return Promise.resolve(var_.apply(null, ctx));
      }
    } else {
      let return_type = var_type.kind.right;
      // console.log(return_type);
      return Promise.resolve(async (arg) => {
        ctx.push(arg);
        let ans = js_to_aplite_helper(var_, return_type, ctx);
        ctx = [];
        return ans;
      });
    }
  }
  return js_to_aplite_helper(var_, var_type, []);
}

///////////////////////////////////////////

function int_to_string_helper(a) {
  return a.toString();
}
function concat_helper(a, b) {
  return a + b;
}


export const int_to_string = js_to_aplite(int_to_string_helper, pi_to_json(int, str));

export const concat = js_to_aplite(concat_helper, pi_to_json(str, pi_to_json(str, str)));






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
        (async (T) => (async (t) => (async (f) => f))),
      );
    }
  }
);
// export const int_to_string = Promise.resolve(async (a) =>
//   Promise.resolve(a.toString())
// );


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

export const panic = Promise.resolve(async (out_str) =>
  async () => {
    await (await (await console_output)(out_str))();
    throw "exception";
  }
);

// export const concat = Promise.resolve(async (a) =>
//   async (b) => Promise.resolve(a + b)
// );

export const YCombinator_please_accept_us = Promise.resolve(async (fn) =>
  async () =>
    (async (x) => x(x))(
      async (maker) => async (...args) => (await (await fn)(await (await maker)(await maker)))(...args),
    )
)
