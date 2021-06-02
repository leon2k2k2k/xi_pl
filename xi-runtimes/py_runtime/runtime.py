def promise_resolve(x):
    async def helper():
        return x

    return helper

class Runtime():
    def __getattr__(self, s):
        return globals(s)

runtime = Runtime()
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
        return promise_resolve(elper2)
    return helper1

async def divide():
    async def helper1(x):
        async def helper2(y):
            if y == 0 : 
                return promise_resolve(0)
            else:
                return promise_resolve(x / y)
        return promise_resolve(helper2)
    return helper1

async def modulo():
    async def helper1(x):
        async def helper2(y):
            if y == 0 : 
                return promise_resolve(0)
            else:
                return promise_resolve(x % y)
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
                    async def helper5():
                        # value : A
                        value = await arg()
                        # result: AFn() -> (AFn() -> B)
                        result = await func(value)

                        return await result()
                    return helper5
                return promise_resolve(helper4)
            return promise_resolve(helper3)
        return promise_resolve(helper2)
    return helper1

# val : A
async def io_pure():
    async def helper1(_):
        async def helper2(val):
            async def helper3():
                return promise_resolve(val)
            return helper3
        return promise_resolve(helper2)
    return helper1

# IO A 
# AsyncFn() -> (AsyncFn () -> A)

# Int -> IO A
# AsyncFn() -> (AsyncFn(Int) -> (AsyncFn () -> (AsyncFn() -> A)))
async def console_output():
    async def console_output_helper(s):
        async def console_output_helper2():
            async def console_output_helper3():
                print(s)
                return 5
            return console_output_helper3
        return console_output_helper2
    return console_output_helper

# AsyncFn() -> (AsyncFn() -> Int)
async def console_input():
    async def input_helper():
        return 500
    return input_helper



# async def main():
#     io_5 = await (await io_pure()) (promise_resolve(5))

#     io_bind_fn = await io_bind()
#     console_output_fn = await console_output()
#     five = await io_5()
#     fn_value = await io_bind_fn(five)
#     partially_applied_fn = (await fn_value())
#     s = await partially_applied_fn(console_output_fn)

#     await (await s())()

# import asyncio
# asyncio.run(main())
import asyncio
from runtime import console_input as ffi3177
from runtime import console_output as ffi3175
from runtime import io_bind as ffi3178
from runtime import io_pure as ffi3181

async def main():

    async def var_3174(var_3174):
        return promise_resolve(var_3174)


    var_3 = await var_3174(await ffi3175())


    async def var_3176(var_3176):
        return promise_resolve(var_3176)


    var_4 = await var_3176(await ffi3177())


    async def var_3179(var_3179):

        async def var_3180(var_3180):
            return await (await (await (await ffi3181())('Number'))())(10)

        return await (await (await (await (await (await (await (await ffi3178()
            )('Number'))())('Number'))())(await (await (await var_3())(var_3179
            ))()))())(var_3180)


    var_329 = await (await (await (await (await (await (await (await ffi3178())
        ('Number'))())('Number'))())(await var_4()))())(var_3179)
    await var_329()


import asyncio
asyncio.run(main())
