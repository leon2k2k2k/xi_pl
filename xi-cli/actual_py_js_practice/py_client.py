from py_server import json_kind, pi_to_json, Server
import asyncio

# we need to create a loop for the async stuff
loop = asyncio.get_event_loop()

# this is going to set up the server and run it.
server = Server("5000", "8080", loop)

# we are going to shove the outputted py file in main.

print("hello")
str_type = json_kind("Str")
int_type = json_kind("Int")
var_3_type = pi_to_json(str_type, pi_to_json(str_type, str_type))


async def main():
    var_3 = await server.deregister_top_level("var_8", pi_to_json(int_type, int_type))
    return (await (var_3)(5))

loop.run_until_complete(main())
