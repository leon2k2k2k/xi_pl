from py_server_beta import u, prim, pi, freevar, instantiate, Server, promise_resolve
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()


UnitType = pi(u(), pi(freevar(0), freevar(0), 1), 0)
StrType = prim("Str")
T_to_T = pi(freevar(0), freevar(0), 1)
T_to_T_to_T = pi(freevar(0), T_to_T, 2)
BoolType = pi(u(), T_to_T_to_T, 0)

# this is going to set up the server and run it.
server = Server("8080", "5000", loop)


async def main():

    # id
    var_0 = promise_resolve(await server.deregister_top_level("var_0", UnitType))

    id_string = await (await var_0())(StrType)
    hello = await (await id_string())("hello")
    print(await hello())

    # # first
    # var_1 = promise_resolve(await server.deregister_top_level("var_1", BoolType))

    # partial = await (await (await (await var_1())(StrType))())("hello")
    # aplite_string = await (await partial())("world")
    # print((await aplite_string()))

    # # second
    # var_2 = promise_resolve(await server.deregister_top_level("var_2", BoolType))

    # partial = await (await (await (await var_2())(StrType))())("hello")
    # aplite_string = await (await partial())("world")
    # print((await aplite_string()))


loop.run_until_complete(main())


loop.run_forever()
