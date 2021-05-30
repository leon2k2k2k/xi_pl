from py_server_fix import json_kind, pi_to_json, Server
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()


# we are going to shove the outputted py file in main.

str_type = json_kind("Str")
int_type = json_kind("Int")
var_3_type = pi_to_json(str_type, pi_to_json(str_type, str_type))

# this is going to set up the server and run it.
server = Server("5000", "8080", loop)


async def main():
    def promise_resolve(x):
        async def helper():
            return x

        return helper

    # # 5

    var_0 = promise_resolve(100)

    # this is how to recover the int.
    print(await (var_0)())
    print(await (var_0)())
    server.register_top_level(var_0, "var_0", json_kind("Int"))

    # # plus_3
    # async def plus_3(x):
    #     return x + 3

    # var_1 = promise_resolve(plus_3)
    # server.register_top_level(await var_1, "var_1", pi_to_json(int_type, int_type))

    # # apply_23
    # async def apply_23(x):
    #     return await x(23)

    # var_2 = promise_resolve(apply_23)
    # server.register_top_level(
    #     await var_2,
    #     "var_2",
    #     pi_to_json(pi_to_json(json_kind("Int"), json_kind("Int")), json_kind("Int")),
    # )

    # # testssssss: note we can't reuse corountine in  Python
    # # so we just have to redefine it everytime lol.

    # # 5
    # var_0 = promise_resolve(5)
    # print(await var_0)

    # # 8
    # var_0 = promise_resolve(5)

    # async def plus_3(x):
    #     return x + 3

    # var_1 = promise_resolve(plus_3)

    # var_210 = (await var_1)(await var_0)
    # print(await var_210)

    # # # 26
    # async def apply_23(x):
    #     return await x(23)

    # var_2 = promise_resolve(apply_23)

    # async def plus_3(x):
    #     return x + 3

    # var_1 = promise_resolve(plus_3)
    # var_232 = (await var_2)(await var_1)
    # print(await var_232)


loop.run_until_complete(main())


loop.run_forever()
