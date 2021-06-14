from py_server_beta import u, prim, pi, freevar, instantiate, Server, promise_resolve
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()


# we are going to shove the outputted py file in main.
int = prim("Int")
int_to_int = pi(int, int, 0)
int_to_int____to_int = pi(int_to_int, int, 1)
int_to______int_to_int = pi(int, int_to_int, 2)

# this is going to set up the server and run it.
server = Server("8080", "5000", loop)


async def main():

    # 5
    var_0 = promise_resolve(await server.deregister_top_level("var_0", int))

    var_1 = promise_resolve(await server.deregister_top_level("var_1", int_to_int))

    var_2 = promise_resolve(
        await server.deregister_top_level("var_2", int_to_int____to_int),
    )

    var_3 = promise_resolve(
        await server.deregister_top_level("var_3", int_to______int_to_int)
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

    # test3:
    # expects 99

    ninty_nine = await (await (await (await var_3())(94))())(5)
    print(await ninty_nine())


loop.run_until_complete(main())


loop.run_forever()
