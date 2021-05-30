from py_server import json_kind, pi_to_json, Server, promise_resolve
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()


# we are going to shove the outputted py file in main.

str_type = json_kind("Str")
int_type = json_kind("Int")
var_3_type = pi_to_json(str_type, pi_to_json(str_type, str_type))

# this is going to set up the server and run it.
server = Server("8080", "5000", loop)


async def promise_resolve(x):
    return x


async def main():
    # # this works! Let's go!
    # var_0 = await server.deregister_top_level("var_0", int_type)
    # print(var_0)

    var_1 = await server.deregister_top_level("var_1", pi_to_json(json_kind("Int"), json_kind("Int")))
    var_294 = await (var_1)(1000)
    print(var_294)
    # var_4 = await server.deregister_top_level("var_2", pi_to_json(int_type, int_type))
    # print(await (var_4)(5))

    # var_3 = await server.deregister_top_level("var_3", int_type)
    # print(var_3)
    # var_3 = await server.deregister_top_level("var_8", pi_to_json(int_type, int_type))
    # return (await (var_3)(5))

loop.run_until_complete(main())


loop.run_forever()
