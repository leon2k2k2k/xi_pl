def promise_resolve(x):
    async def helper():
        return x

    return helper


import types


class Runtime(types.ModuleType):
    def __getattr__(self, s):
        if s == "__path__":
            return ""
        return globals()[s]


import sys

runtime = Runtime("runtime")
sys.modules["runtime"] = runtime

UnitType = promise_resolve("UnitType")
unit = promise_resolve("unit")


async def int_to_string():
    async def int_to_string_helper(x):
        return promise_resolve(x)

    return int_to_string_helper


async def plus():
    async def plus_helper1(x):
        async def plus_helper2(y):
            return promise_resolve(x + y)

        return promise_resolve(plus_helper2)

    return plus_helper1


async def minus():
    async def minus_helper1(x):
        async def minus_helper2(y):
            return promise_resolve(x - y)

        return promise_resolve(minus_helper2)

    return minus_helper1


async def multiply():
    async def helper1(x):
        async def helper2(y):
            return promise_resolve(x * y)

        return promise_resolve(helper2)

    return helper1


async def divide():
    async def helper1(x):
        async def helper2(y):
            if y == 0:
                return promise_resolve(0)
            else:
                return promise_resolve(x / y)

        return promise_resolve(helper2)

    return helper1


async def modulo():
    async def helper1(x):
        async def helper2(y):
            if y == 0:
                return promise_resolve(0)
            else:
                return promise_resolve(x % y)

        return promise_resolve(helper2)

    return helper1


async def concat():
    async def helper1(x):
        async def helper2(y):
            return promise_resolve(x + y)

        return promise_resolve(helper2)

    return helper1


# arg: IO A
# arg: AFn() -> A
# func: A -> IO B
# func: AFn(A) -> (AFn() -> (AFn() -> B))


async def io_bind():
    async def helper1(_):
        async def helper2(_):
            async def helper3(arg):
                async def helper4(func):
                    # we need to return an AFn() -> (AFn() -> B) from here

                    # This has type AFn() -> (AFn() -> B)
                    # return promise_resolve(helper5)
                    async def helper5():
                        # value : A
                        value = await arg()
                        # result: AFn() -> (AFn() -> B)
                        result = await (await func(value))()

                        return await result()

                    return promise_resolve(helper5)

                return promise_resolve(helper4)

            return promise_resolve(helper3)

        return promise_resolve(helper2)

    return helper1


# val : A
async def io_pure():
    async def helper1(_):
        async def helper2(val):
            async def helper3():
                return val

            return promise_resolve(helper3)

        return promise_resolve(helper2)

    return helper1


# IO A
# AsyncFn() -> (AsyncFn () -> A)

# Int -> IO A
# AsyncFn() -> (AsyncFn(Int) -> (AsyncFn () -> (AsyncFn() -> A)))
async def console_output():
    async def console_output_helper(s):
        async def console_output_helper2():
            print(s)
            return 5

        return promise_resolve(console_output_helper2)

    return console_output_helper


# AsyncFn() -> (AsyncFn() -> Int)
async def console_input():
    async def input_helper():
        return input()

    return input_helper
