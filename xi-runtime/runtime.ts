export const IO = (_: any) => "IO";

function io_bind2<T, U>(arg: () => Promise<T>) {
  return (func: ((_: T) => (() => Promise<U>))) =>
    async () => {
      const value = await arg();
      return await func(value)();
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
  return async () => val;
}

export function io_pure(_: any) {
  return io_pure2;
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
  return promise_pure2;
}

export function id(val: any) {
  return val;
}

export function plus(a: any) {
  return (b: any) => a + b;
}

export function minus(a: any) {
  return (b: any) => a - b;
}

export function multiply(a: any) {
  return (b: any) => a * b;
}

export function divide(a: any) {
  return (b: any) => {
    if (b == 0) {
      return 0;
    }
    return a / b;
  };
}

export function modulo(a: any) {
  return (b: any) => {
    if (b == 0) {
      return 0;
    }
    return a % b;
  };
}
