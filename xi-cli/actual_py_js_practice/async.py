import asyncio
import aiohttp
from quart import Quart

# app = Quart(__name__)


# @app.route("/", methods=['GET'])
# async def handle_post():
#     print("started handling request")
#     await asyncio.sleep(5)
#     print("done handling request")
#     return "hello"


# loop = asyncio.get_event_loop()


# def run_task(loop, app):
#     loop.create_task(app.run_task())


# run_task(loop, app)


# async def send_request():
#     async with aiohttp.ClientSession() as session:
#         resp = await session.get("http://localhost:5000")
#         async with resp:
#             ans = await resp.text()
#             print(f"CLIENT: received {ans} from post")
#             return ans

# ans = loop.run_until_complete(send_request())


# async def promise_resolve(x):
#     return x


# async def main():

#     var_0 = promise_resolve(5)
#     server.register_top_level(await var_0, "var_3", json_kind("Int"))

# asyncio.run(main())

print(int(5))
