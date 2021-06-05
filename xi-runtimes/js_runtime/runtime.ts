// we use json object to encode types as before

function pi_to_json(left: any, right: any) {
  return { kind: { left: left, right: right } };
}

function json_kind(type: any) {
  return { kind: type };
}

let int = json_kind("Int");

// we need a context because we have to call all the variables at once
function js_to_aplite(var_: any, var_type: any) {
  function js_to_aplite_helper(var_: any, var_type: any, ctx: any) {
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
      return Promise.resolve(async (arg: any) => {
        ctx.push(arg);
        let ans = js_to_aplite_helper(var_, return_type, ctx);
        ctx = [];
        return ans;
      });
    }
  }
  return js_to_aplite_helper(var_, var_type, []);
}

export const IO = async (_: any) => "IO";

export const io_bind = Promise.resolve(async (_: any) =>
  async (_: any) =>
    async (arg: any) =>
      async (func: any) => {
        return async () => {
          const value = await arg();
          return await (await func(value))();
        };
      }
);

export const io_pure = Promise.resolve(async (_: any) =>
  async (val: any) => async () => val
);
// plus
export function plus_helper(x: any, y: any) {
  return x + y;
}

export const plus = js_to_aplite(
  plus_helper,
  pi_to_json(int, pi_to_json(int, int)),
);
export const minus = Promise.resolve(async (a: any) =>
  async (b: any) => Promise.resolve(a - b)
);
export const multiply = Promise.resolve(async (a: any) =>
  async (b: any) => Promise.resolve(a * b)
);
export const divide = Promise.resolve(async (a: any) =>
  async (b: any) => {
    if (b == 0) return 0;
    return Promise.resolve(a / b);
  }
);
export const modulo = Promise.resolve(async (a: any) =>
  async (b: any) => {
    if (b == 0) return 0;
    return Promise.resolve(a % b);
  }
);
