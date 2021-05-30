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
    async def plus_3(x):
        return x + 3
    promise_8 = (await promise_resolve(plus_3))(5)
    print(await promise_8)

loop.run_until_complete(main())


loop.run_forever()
