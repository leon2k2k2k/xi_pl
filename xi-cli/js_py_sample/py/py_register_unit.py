from py_server_beta import u, prim, pi, freevar, instantiate, Server, promise_resolve
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()

UnitType = pi(u(), pi(freevar(0), freevar(0), 1), 0)
StrType = prim("Str")


# this is going to set up the server and run it.
server = Server("8080", "5000", loop)


async def main():

    # lambda|T : Type, t : T| t
    async def var_32(var_32):
        async def var_33(var_33):
            return promise_resolve(var_33)

        return promise_resolve(var_33)

    var_0 = promise_resolve(var_32)
    server.register_top_level(var_0, "var_0", UnitType)
    id_string = await (await var_0())(StrType)
    hello = await (await id_string())("hello")
    print(await hello())

    T_to_T = pi(freevar(0), freevar(0), 1)
    T_to_T_to_T = pi(freevar(0), T_to_T, 2)
    BoolType = pi(u(), T_to_T_to_T, 0)

    # true = lambda|T: Type, t1: T, t2: T| t1
    async def var_578(var_578):
        async def var_579(var_579):
            async def var_580(var_580):
                return promise_resolve(var_579)

            return promise_resolve(var_580)

        return promise_resolve(var_579)

    var_1 = promise_resolve(var_578)
    server.register_top_level(var_1, "var_1", BoolType)

    partial = await (await (await (await var_1())(StrType))())("hello")
    aplite_string = await (await partial())("world")
    print((await aplite_string()))

    # false = lambda|T : Type, t1: T, t2: T| t2
    async def var_581(var_581):
        async def var_582(var_582):
            async def var_583(var_583):
                return promise_resolve(var_583)

            return promise_resolve(var_583)

        return promise_resolve(var_582)

    var_2 = promise_resolve(var_581)
    server.register_top_level(var_2, "var_2", BoolType)
    partial = await (await (await (await var_2())(StrType))())("hello")
    aplite_string = await (await partial())("world")
    print((await aplite_string()))


loop.run_until_complete(main())


loop.run_forever()
