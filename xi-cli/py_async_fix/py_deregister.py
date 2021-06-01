from py_server_fix import json_kind, pi_to_json, Server, promise_resolve
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()


# we are going to shove the outputted py file in main.

str_type = json_kind("Str")
int_type = json_kind("Int")
var_3_type = pi_to_json(str_type, pi_to_json(str_type, str_type))

# this is going to set up the server and run it.
server = Server("8080", "5000", loop)


async def main():

    # 5
    var_0 = promise_resolve(await server.deregister_top_level("var_0", int_type))

    var_1 = promise_resolve(
        await server.deregister_top_level(
            "var_1", pi_to_json(json_kind("Int"), json_kind("Int"))
        )
    )

    var_2 = promise_resolve(
        await server.deregister_top_level(
            "var_2",
            pi_to_json(
                pi_to_json(json_kind("Int"), json_kind("Int")), json_kind("Int")
            ),
        )
    )

    ans_28 = await (await var_2())(await var_1())
    print(await ans_28())

    # tests:

    # test0:
    # expects 100
    print("#############TEST RESULT#############")
    print(await var_0())

    # test1:
    # expects 15

    fiften = await (await var_1())(10)
    print("#############TEST RESULT#############")

    print(await fiften())

    # expectes 105

    hundred_and_five = await (await var_1())(await var_0())
    print("#############TEST RESULT#############")
    print(await hundred_and_five())

    # test2:
    # expects 25

    twenty_five = await (await var_2())(await var_1())
    print("#############TEST RESULT#############")
    print(await twenty_five())


loop.run_until_complete(main())


loop.run_forever()
