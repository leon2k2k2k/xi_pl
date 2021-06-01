from py_server_fix import json_kind, pi_to_json, Server, promise_resolve
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

    # # 100

    var_0 = promise_resolve(100)
    server.register_top_level(var_0, "var_0", json_kind("Int"))

    print((await (var_0)()))
    server.register_top_level(var_0, "var_0", json_kind("Int"))

    # plus_5

    async def plus_5(x):
        return promise_resolve(x + 5)

    var_1 = promise_resolve(plus_5)
    server.register_top_level(var_1, "var_1", pi_to_json(int_type, int_type))

    # apply_20
    async def apply_20(x):
        return await x(20)

    var_2 = promise_resolve(apply_20)
    server.register_top_level(
        var_2,
        "var_2",
        pi_to_json(pi_to_json(json_kind("Int"), json_kind("Int")), json_kind("Int")),
    )

    # tests:

    # test0:
    # expects 100
    print(await var_0())

    # test1:
    # expects 15
    fiften = await (await var_1())(10)
    print(await fiften())

    # expectes 105
    hundred_and_five = await (await var_1())(await var_0())
    print(await hundred_and_five())

    # test2:
    # expects 25
    twenty_five = await (await var_2())(await var_1())
    print(await twenty_five())


loop.run_until_complete(main())


loop.run_forever()