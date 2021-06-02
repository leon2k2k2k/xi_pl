import asyncio

def promise_resolve(x):
    async def helper():
        return x

    return helper


async def main():
    async def plus():
        async def plus_helper1(x):
            async def plus_helper2(y):
                return promise_resolve(x + y)
            return promise_resolve(plus_helper2)
        return plus_helper1

    var_3 = await (await (await (await plus())(2))())(4)

    print(await var_3())
asyncio.run(main())
