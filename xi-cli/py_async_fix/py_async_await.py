from py_server_fix import json_kind, pi_to_json, Server
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

    # the type of this is T -> (() - Promise(T))
    def promise_resolve(x):
        async def helper():
            return x

        return helper

    var_0 = promise_resolve(100)

    # this is the "inverse" function
    print(await (var_0)())
    server.register_top_level(var_0, "var_0", json_kind("Int"))

    # plus_3 primitive function

    # the type of plus_3 = () -> Promise(Int -> () -> Promise(Int))
    plus_3 = promise_resolve(lambda x: promise_resolve(x + 3))

    # how to use it
    five = (await plus_3())(await var_0())
    print(await five())

    # note that I can't do the following
    # plus_3 = lambda x: (await plus_3())(await x)
    # as I can't have await inside of lambda.
    # this is why we would like async lambda, but instead
    # we only have async def.

    async def app(plus_3, var_0):
        return (await plus_3())(await var_0())

    print(await (await app(plus_3, var_0))())

    # besides primitive function, we need app and lambda,
    # I have just defined app above, so let's think about lambda.

    # now let's showve that in the middle of an expression:
    # (lambda|x : Int| plus_3 x) (5)
    # we need to generate the following expression:
    async def plus_3_prime(var_0):
        return (await plus_3())(var_0)

    plus_3_prime = promise_resolve(plus_3_prime)
    five = await (await plus_3_prime())(await var_0())
    print("this is the thing I am testing!!!!!!!")
    print(await five())

    # ((lambda|f, x| f x) (plus_3)) (5)
    async def app(f, x):
        return await plus_3

    # (lambda |x : Int| (lambda|y : Int | x + y))
    async def var_123(x):
        async def var_1233(y):
            return await plus(x, y)

        return var_1233

    # note that in js backend this goes to a single expr, but here this becomes two parts,
    # one of them is the async def, and down below we just apply them.

    # now for py(lambda): well, I have to generate the function above
    # and then apply it as such


loop.run_until_complete(main())


loop.run_forever()
