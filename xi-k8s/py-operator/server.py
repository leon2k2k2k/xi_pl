import json
from aiohttp import web
import aiohttp
import asyncio
import sys
import types


def promise_resolve(x):
    async def helper():
        return x

    return helper


###########################
# encoding types as Python dict objects.
def u():
    return {"kind": "U", "value": "U"}


def prim(x):
    return {"kind": "prim", "value": x}


def pi(left, right, var_id):
    return {"kind": "pi", "left": left, "right": right, "var_id": var_id}


def freevar(index):
    return {"kind": "free_var", "index": index}


def app(left, right):
    return {"kind": "app", "left": left, "right": right}

def instantiate(sexpr, expr, var_index):
    if sexpr["kind"] == "prim":
        return sexpr
    elif sexpr["kind"] == "U":
        return sexpr
    elif sexpr["kind"] == "pi":
        return pi(
            instantiate(sexpr["left"], expr, var_index),
            instantiate(sexpr["right"], expr, var_index),
            sexpr["var_id"],
        )
    elif sexpr["kind"] == "free_var":
        print("this is sexpr ", sexpr, " and var_index", var_index)
        if sexpr["index"] == var_index:
            return expr
        else:
            return sexpr
    elif sexpr["kind"] == "app":
        return app(
            instantiate(sexpr["left"], expr, var_index),
            instantiate(sexpr["right"], expr, var_index),
        )


class Runtime(types.ModuleType):
    def __getattr__(self, s):
        if s == "__path__":
            return ""
        return globals()[s]


runtime = Runtime("runtime")
sys.modules["runtime"] = runtime


async def int_to_string():
    async def int_to_string_helper(x):
        return promise_resolve(x)

    return int_to_string_helper


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

        return promise_resolve(helper2)

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


async def concat():
    async def helper1(x):
        async def helper2(y):
            return promise_resolve(x + y)

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


def promise_resolve(x):
    async def helper():
        return x

    return helper


##################################
# SERVER CODE
# I shove it here because only the server-back needs it.


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

        # # runs the server:
        # app = self.new_server(port)
        loop.create_task(self.new_server())
        print(f"PY_SERVER running at {port}")

    def serialize(self, value, type_):
        if type_["kind"] == "prim":
            if type_["value"] == "Int":
                return self.register_new(value)
            elif type_["value"] == "Str":
                return self.register_new(value)
            else:
                raise ValueError("type_ not understood")
        elif type_["kind"] == "pi":
            var_type = type_["left"]
            return_type = type_["right"]
            var_id = type_["var_id"]

            async def new_func(s):
                x = await self.deserialize(s, var_type)
                return self.serialize(
                    await (await value())(x), instantiate(return_type, x, var_id)
                )

            return_value = promise_resolve(new_func)
            return self.register_new(return_value)
        elif type_["kind"] == "free_var":
            raise ValueError("type_ should not be freevar")
        elif type_["kind"] == "U":
            return value
        elif type_["kind"] == "app":
            functor = type_["left"]
            applied_type = type_["right"]
            if functor["value"] == "IO":
                return self.register_new(value)
            else:
                raise ValueError("{} is not IO and not implemented", functor)
        else:
            raise ValueError("type_ not understood")

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
        if type_["kind"] == "prim":
            if type_["value"] == "Int":
                request = {
                    "remote_ident": value,
                }
                return int(await self.post(request))
            elif type_["value"] == "Str":
                print("I am sending over string: ", value)
                request = {
                    "remote_ident": value,
                }

                return await self.post(request)
            else:
                raise ValueError("type_ not understood")
        elif type_["kind"] == "U":
            return value
        elif type_["kind"] == "free_var":
            raise ValueError("shouldn't see a freevar")
        elif type_["kind"] == "pi":
            var_type = type_["left"]
            return_type = type_["right"]
            var_id = type_["var_id"]

            async def new_func(x):
                print("this is var_type", var_type, " and x ", x)
                if type(x) == dict:
                    serialized_x = self.serialize(x, var_type)
                else:
                    serialized_x = self.serialize(promise_resolve(x), var_type)

                request = {
                    "remote_ident": value,
                    "arg": serialized_x,
                }
                return_id = json.loads(await self.post(request))
                return promise_resolve(
                    await self.deserialize(
                        return_id, instantiate(return_type, x, var_id)
                    )
                )

            return new_func
        elif type_["kind"] == "app":
            functor = type_["left"]
            applied_type = type_["right"]
            if functor["kind"] == "prim":
                if functor["value"] == "IO":
                    # to use using like print_hello : IO UnitType, on the local side
                    # one will call
                    # await (await print_hello())()
                    # , and this should send a request to the
                    # other side.

                    # this tells me that I need to return a function that is wraped in Promise.resolve (() => ...)
                    async def new_func():
                        request = {
                            "remote_ident": value,
                            "IO": True,
                        }
                        await self.post(request)
                        return None

                    return new_func
                else:
                    raise ValueError("{} is not IO", functor["value"])
            else:
                ValueError("{} is not prim", functor["kind"])

    # async def deserialize(self, value, type_):
    #     if type_["kind"] == "Int":
    #         request = {
    #             "js_ident": value,
    #         }
    #         return int(await self.post(request))
    #     elif type_["kind"] == "Str":
    #         request = {"js_ident": value}
    #         return await self.post(request)
    #     else:
    #         arg_type = type_["kind"]["left"]
    #         return_type = type_["kind"]["right"]
    #         # PROBLEM: THIS IS A SYNC FUNCTION AND THAT IS RUINING EVERYTHING!!!
    #         async def new_func(x):
    #             serialized_x = self.serialize(promise_resolve(x), arg_type)
    #             request = {
    #                 "js_ident": value,
    #                 "value": serialized_x,
    #             }
    #             return_id = json.loads(await self.post(request))
    #             return promise_resolve(await self.deserialize(return_id, return_type))

    #         return new_func

    async def post(self, value):
        print(f"CLIENT: posting with {value}")
        url = f"http://localhost:{self.other_port}"
        async with aiohttp.ClientSession() as session:
            resp = await session.post(url, json=value)
            async with resp:
                ans = await (resp).text()
                print(f"CLIENT: received {ans} from post")
                return ans

    async def new_server(self):
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
                if "remote_ident" in dict:
                    if dict["remote_ident"] in self.registrations:
                        result_ident = self.registrations[dict["remote_ident"]]
                        if "arg" in dict:
                            # need to change.
                            ans = await (await result_ident())(dict["arg"])
                            print("*****************************")
                            print(type(ans))
                            return web.Response(text=json.dumps(ans))
                        else:
                            if "IO" in dict:
                                print("hello!!! I received an IO request")
                                return web.Response(
                                    text=json.dumps(await (await result_ident())())
                                )
                            else:

                                return web.Response(
                                    text=json.dumps(await result_ident())
                                )
                    else:
                        raise "ident not found"

        app.add_routes(
            [
                web.post("/", handle_post),
            ]
        )

        # web.run_app(app, host="127.0.0.1", port=self.port)
        runner = aiohttp.web.AppRunner(app)
        await runner.setup()
        await aiohttp.web.TCPSite(runner, host="127.0.0.1", port=self.port).start()

        # wait forever, running both the web server and the tasks
        await asyncio.Event().wait()


######################

######################
import py_operator

loop = asyncio.get_event_loop()
server = Server(8080, 5000, loop)


async def main():
    #     var_311 = promise_resolve(5)
    #     server.register_top_level(var_311, "var_311", prim("Int"))

    # AsyncFn() -> (AsyncFn() -> Int)

    async def create_deployment():
        async def create_deployment_helper():
            py_operator.main()
            return 5

        return create_deployment_helper

    #    how to use it
    #    await (await create_deployment())()
    IO_UnitType = app(prim("IO"), prim("UnitType"))

    server.register_top_level(create_deployment, "create_deployment", IO_UnitType)


loop.run_until_complete(main())
loop.run_forever()
