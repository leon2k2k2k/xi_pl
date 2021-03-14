import { readLines } from "https://deno.land/std@0.88.0/io/bufio.ts";
const stdinReader = readLines(Deno.stdin);

function io_bind2<T, U>(arg: () => T) {
    return (func: ((_: T) => (() => U))) => {
        const value = arg();
        return func(value);
    };
}

export function io_bind(_: any) {
    return (_: any) => io_bind2
}

function io_pure2<T>(val: T) {
    return () => val;
}

export function io_pure(_: any) {
    return io_pure2;
}

export function console_input() {
    return stdinReader.next().then(
        ({ value }) => {
            return value;
        },
    );
}

export function console_output() {
    return (out_str: string) => Deno.writeAll(Deno.stdout, new TextEncoder().encode(out_str));
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
