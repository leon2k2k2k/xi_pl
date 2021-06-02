def promise_resolve(x):
    async def helper():
        return x

    return helper


import types


class Runtime(types.ModuleType):
    def __getattr__(self, s):
        if s == "__path__":
            return ""
        return globals()[s]


import sys

runtime = Runtime("runtime")
sys.modules["runtime"] = runtime


async def plus():
    async def plus_helper1(x):
        async def plus_helper2(y):
            return promise_resolve(x + y)

        return promise_resolve(plus_helper2)

    return plus_helper1


async def minus():
    async def minus_helper1(x):
        async def minus_helper2(y):
            return promise_resolve(x - y)

        return promise_resolve(minus_helper2)

    return minus_helper1


async def multiply():
    async def helper1(x):
        async def helper2(y):
            return promise_resolve(x * y)

        return promise_resolve(elper2)

    return helper1


async def divide():
    async def helper1(x):
        async def helper2(y):
            if y == 0:
                return promise_resolve(0)
            else:
                return promise_resolve(x / y)

        return promise_resolve(helper2)

    return helper1


async def modulo():
    async def helper1(x):
        async def helper2(y):
            if y == 0:
                return promise_resolve(0)
            else:
                return promise_resolve(x % y)

        return promise_resolve(helper2)

    return helper1


# arg: IO A
# arg: AFn() -> A
# func: A -> IO B
# func: AFn(A) -> (AFn() -> (AFn() -> B))


async def io_bind():
    async def helper1(_):
        async def helper2(_):
            async def helper3(arg):
                async def helper4(func):
                    # we need to return an AFn() -> (AFn() -> B) from here

                    # This has type AFn() -> (AFn() -> B)
                    # return promise_resolve(helper5)
                    async def helper5():
                        # value : A
                        value = await arg()
                        # result: AFn() -> (AFn() -> B)
                        result = await (await func(value))()

                        return await result()

                    return promise_resolve(helper5)

                return promise_resolve(helper4)

            return promise_resolve(helper3)

        return promise_resolve(helper2)

    return helper1


# val : A
async def io_pure():
    async def helper1(_):
        async def helper2(val):
            async def helper3():
                return val

            return promise_resolve(helper3)

        return promise_resolve(helper2)

    return helper1


# IO A
# AsyncFn() -> (AsyncFn () -> A)

# Int -> IO A
# AsyncFn() -> (AsyncFn(Int) -> (AsyncFn () -> (AsyncFn() -> A)))
async def console_output():
    async def console_output_helper(s):
        async def console_output_helper2():
            print(s)
            return 5

        return promise_resolve(console_output_helper2)

    return console_output_helper


# AsyncFn() -> (AsyncFn() -> Int)
async def console_input():
    async def input_helper():
        return input()

    return input_helper


# this is Python's correspnding server to server.ts, I think I will
# be using flask. Let's just try to set up the server.

import asyncio
from logging import raiseExceptions
import aiohttp
from aiohttp import web
import json
import requests


###########################
# encoding types as Python dict objects.
def json_kind(type_):
    return {"kind": type_}


def pi_to_json(left, right):
    return {"kind": {"left": left, "right": right}}


def promise_resolve(x):
    async def helper():
        return x

    return helper


# let's try to define  a Server object and its attributes:
# register_id, var_registration is going to be dicts
# type is going to be a dict


class Server:
    def __init__(self, port, other_port, loop):
        # initial values.
        self.port = port
        self.other_port = other_port
        self.register_id = 0
        self.registrations = {}
        self.var_registrations = {}

        # runs the server:
        app = self.new_server(port)

        loop.create_task(self.new_server(port))
        print(f"PY_SERVER running at {port}")

    def serialize(self, value, type_):
        if type_["kind"] == "Int":
            return self.register_new(value)
        # only doing Int right now.
        elif type_["kind"] == "Str":
            return self.register_new(promise_resolve(value))
        else:
            arg_type = type_["kind"]["left"]
            return_type = type_["kind"]["right"]
            # I am missing a bunch of awaits

            async def new_func(s):
                x = await self.deserialize(s, arg_type)
                return self.serialize(await (await value())(x), return_type)

            print("alskdjflaksjflaksj")
            return_value = promise_resolve(new_func)
            return self.register_new(return_value)

    def register_new(self, value: any):
        current_register_id = self.register_id
        self.registrations[current_register_id] = value
        print(f"SERVER: registerd id {current_register_id} with value {value}")
        self.register_id += 1
        return current_register_id

    def register_top_level(self, value, var_name, var_type):
        register_id = self.serialize(value, var_type)
        print(f"Server: registered top level {var_name} with id {register_id}")
        self.var_registrations[var_name] = register_id

    async def deregister_top_level(self, value, var_type):
        request = {
            "request_type": "reg_id",
            "var_name": value,
        }
        register_id = json.loads(await self.post(request))
        print(type(register_id))
        return await self.deserialize(register_id, var_type)

    async def deserialize(self, value, type_):
        if type_["kind"] == "Int":
            request = {
                "js_ident": value,
            }
            return int(await self.post(request))
        elif type_["kind"] == "Str":
            request = {"js_ident": value}
            return await self.post(request)
        else:
            arg_type = type_["kind"]["left"]
            return_type = type_["kind"]["right"]
            # PROBLEM: THIS IS A SYNC FUNCTION AND THAT IS RUINING EVERYTHING!!!
            async def new_func(x):
                serialized_x = self.serialize(promise_resolve(x), arg_type)
                request = {
                    "js_ident": value,
                    "value": serialized_x,
                }
                return_id = json.loads(await self.post(request))
                return promise_resolve(await self.deserialize(return_id, return_type))

            return new_func

    async def post(self, value):
        print(f"CLIENT: posting with {value}")
        url = f"http://localhost:{self.other_port}"
        async with aiohttp.ClientSession() as session:
            resp = await session.post(url, json=value)
            async with resp:
                ans = await (resp).text()
                print(f"CLIENT: received {ans} from post")
                return ans

    async def new_server(self, port):
        # here's the code for the server
        app = web.Application()

        async def hello_world(js_ident, value):
            print(js_ident)
            return f"js_ident is {js_ident} and value is {value}!"

        # let's say that js server will send a message like:
        # methods: "POST", json = "{js_ident: ??, value: ??}",
        # the value part is optional.

        async def handle_post(request):
            # serialized_data = request.json
            # data = json.loads(serialized_data)
            dict = await request.json()

            print("////////////////////////////////////////////////////")
            print(f"Py Server receive request with json {dict}")

            if "request_type" in dict:
                reg_id = self.var_registrations[dict["var_name"]]
                print(reg_id)
                return web.Response(text=json.dumps(reg_id))
                # do other stuff not worry about it here.
            else:
                if "js_ident" in dict:
                    if dict["js_ident"] in self.registrations:
                        result_ident = self.registrations[dict["js_ident"]]
                        if "value" in dict:
                            # need to change.
                            ans = await (await result_ident())(dict["value"])
                            print("*****************************")
                            print(type(ans))
                            return web.Response(text=json.dumps(ans))
                        else:
                            return web.Response(text=json.dumps(await result_ident()))
                    else:
                        raise "ident not found"

        app.add_routes(
            [
                web.post("/", handle_post),
            ]
        )

        runner = aiohttp.web.AppRunner(app)
        await runner.setup()
        await aiohttp.web.TCPSite(runner).start()

        # wait forever, running both the web server and the tasks
        await asyncio.Event().wait()


######################
