def promise_resolve(x):
    def helper_pr():
        return x

    return helper_pr
# promise_resolve = lambda x: lambda: x


class Runtime:
    def __getattr__(self, s):
        return globals(s)


runtime = Runtime()


def plus():
    def plus_helper1(x):
        def plus_helper2(y):
            return promise_resolve(x + y)

        return promise_resolve(plus_helper2)

    return plus_helper1


def minus():
    def minus_helper1(x):
        def minus_helper2(y):
            return promise_resolve(x - y)

        return promise_resolve(minus_helper2)

    return minus_helper1


def multiply():
    def helper1(x):
        def helper2(y):
            return promise_resolve(x * y)

        return promise_resolve(elper2)

    return helper1


def divide():
    def helper1(x):
        def helper2(y):
            if y == 0:
                return promise_resolve(0)
            else:
                return promise_resolve(x / y)

        return promise_resolve(helper2)

    return helper1


def modulo():
    def helper1(x):
        def helper2(y):
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


#AFn() -> AFn(A) -> 
def io_bind():
    def helper1(_):
        def helper2(_):
            def helper3(arg):
                def helper4(func):
                    # we need to return an AFn() -> (AFn() -> B) from here

                    # This has type AFn() -> (AFn(_, _) -> B)
                    def helper5(_, __):
                        # value : A
                        value = arg(1, 3)
                        # result: AFn() -> (AFn() -> B)
                        result = func(value)

                        return result(1, 3)

                    return promise_resolve(helper5)

                return promise_resolve(helper4)

            return promise_resolve(helper3)

        return promise_resolve(helper2)

    return helper1


# val : A
def io_pure():
    def helper1(_):
        def helper2(val):
            def helper3():
                return promise_resolve(val)

            return helper3

        return promise_resolve(helper2)

    return helper1



# IO A
# AsyncFn() -> (AsyncFn () -> A)

# Int -> IO Int
# AsyncFn() -> (AsyncFn(Int) -> (AsyncFn () -> (AsyncFn() -> A)))

called = False
def console_output():
    def console_output_helper(s):
        def console_output_helper2(_, __):
            global called
            # if not called:
            #     called = True
            # else:
            #     raise Exception("I'm getting called")
            print(s)
            return 5

        return promise_resolve(console_output_helper2)

    return console_output_helper


# AsyncFn() -> (AsyncFn() -> Int)
def console_input():
    def input_helper(_, __):
        return 500

    return input_helper


#  def main():
#     io_5 =  ( io_pure()) (promise_resolve(5))

#     io_bind_fn =  io_bind()
#     console_output_fn =  console_output()
#     five =  io_5()
#     fn_value =  io_bind_fn(five)
#     partially_applied_fn = ( fn_value())
#     s =  partially_applied_fn(console_output_fn)

#      ( s())()

# import io
# io.run(main())
import io
ffi3177 = console_input
ffi3175 = console_output
ffi3178 = io_bind
# ffi3181 = io_pure
# from runtime2 import console_input as ffi3177
# from runtime2 import console_output as ffi3175
# from runtime2 import io_bind as ffi3178
# from runtime2 import io_pure as ffi3181


def main():
    # def var_3174(var_3174):
    #     return promise_resolve(var_3174)

    var_3 = ffi3175

    # def var_3176(var_3176):
    #     return promise_resolve(var_3176)

    var_4 = ffi3177

    def var_3179(var_3179):
        def var_3180(var_3180):
            return (((ffi3181())("Number"))())(10)

        return (
            ((((((ffi3178())("Number"))())("Number"))())(((var_3())(var_3179))()))()
        )(var_3180)

    s = (ffi3178())
    n = (s("Number"))
    m = n()
    h = m("Number")
    b = h()
    c = var_4()
    o = (b(c))()
    u = o(var_3179)
    u()(1, 2)


main()
