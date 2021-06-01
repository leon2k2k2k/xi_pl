from py_server_fix import json_kind, pi_to_json, Server
import asyncio

loop = asyncio.get_event_loop()


async def main():

    # let's try to experiment with async(() -> Promise....)
    # the type of this is T -> (() - Promise(T))
    def promise_resolve(x):
        async def helper():
            return x

        return helper

    async def async_print(x):
        print(x)

    var_0 = promise_resolve(100)
    print(await var_0())


loop.run_until_complete(main())
