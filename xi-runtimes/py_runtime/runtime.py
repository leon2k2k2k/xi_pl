def promise_resolve(x):
    async def helper():
        return x

    return helper
