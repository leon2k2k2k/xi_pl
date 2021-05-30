from py_server import json_kind, pi_to_json, Server, promise_resolve
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()


# we are going to shove the outputted py file in main.

str_type = json_kind("Str")
int_type = json_kind("Int")
var_3_type = pi_to_json(str_type, pi_to_json(str_type, str_type))

# this is going to set up the server and run it.
server = Server("5000", "8080", loop)


async def promise_resolve(x):
    return x


async def main():

    var_0 = promise_resolve(5)
    server.register_top_level(await var_0, "var_0", json_kind("Int"))

    async def var_1(x):
        return promise_resolve(x + 3)

    var_2 = promise_resolve(var_1)
    server.register_top_level(await var_2, "var_2", pi_to_json(int_type, int_type))

    # var_4 = await server.deregister_top_level("var_2", pi_to_json(int_type, int_type))
    # print(await (var_4)(5))

    # var_3 = await server.deregister_top_level("var_3", int_type)
    # print(var_3)
    # var_3 = await server.deregister_top_level("var_8", pi_to_json(int_type, int_type))
    # return (await (var_3)(5))

loop.run_until_complete(main())


loop.run_forever()
