
from server import Server
from server import app, promise_resolve, console_output, io_bind, io_pure, prim

import asyncio
loop = asyncio.get_event_loop()
server = Server(5000, 8080, loop)


async def main():

    IO_UnitType = app(prim("IO"), prim("UnitType"))

    create_deployment = promise_resolve(await server.deregister_top_level('create_deployment', IO_UnitType))

    await (await create_deployment())()

#    await (await create_deployment())()
#    async def var_1653(var_1653):
#        return await (await (await (await io_pure())(prim('Int')))())(5)
#    var_222 = await (await (await (await (await (await (await (await
#             io_bind())(await var_3()))())(prim('Int')))())(await (await (await console_output())('hello'))()))())(var_1653)
#    await (await var_222())()

loop.run_until_complete(main())
loop.run_forever()