export const IO = async (_: any) => "IO";

export const io_bind = Promise.resolve(async (_: any) =>
  async (_: any) =>
    async (arg: any) =>
      async (func: any) => {
        return async () => {
          const value = await arg();
          return await (await (await func)(value))();
        };
      }
);

export const io_pure = Promise.resolve(async (_: any) =>
  async (val: any) => async () => val
);

export const plus = Promise.resolve(async (a: any) =>
  async (b: any) => Promise.resolve(a + b)
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
