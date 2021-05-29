
import asyncio
import aiohttp
from quart import Quart, request
import json
import requests

# here's the code for the server
app = Quart(__name__)


@app.route("/<js_ident>/<value>")
async def hello_world(js_ident, value):
    print(js_ident)
    return f"js_ident is {js_ident} and value is {value}!"
# let's say that js server will send a message like:
# methods: "POST", json = "{js_ident: ??, value: ??}",
# the value part is optional.


@app.route("/", methods=['POST'])
async def handle_post():
    # serialized_data = request.json
    # data = json.loads(serialized_data)
    print("////////////////////////////////////////////////////")
    print(f"Py Server receive request with json {await request.get_json()}")

    return "hello"


loop = asyncio.get_event_loop()


def run_task(loop, app):
    loop.run_until_complete(app.run_task())


run_task(loop, app)
