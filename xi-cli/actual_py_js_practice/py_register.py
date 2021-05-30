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


async def main():
    # # # works!! let's go!
    # var_0 = promise_resolve(5)
    # server.register_top_level(var_0, "var_0", json_kind("Int"))

    async def plus_3(x):
        return x + 100

    var_1 = promise_resolve(plus_3)
    server.register_top_level(await var_1, "var_1", pi_to_json(int_type, int_type))


loop.run_until_complete(main())


loop.run_forever()
