# this is Python's correspnding server to server.ts, I think I will
# be using flask. Let's just try to set up the server.

from flask import request
from flask import Flask
import json
from markupsafe import escape
app = Flask(__name__)


@app.route("/<js_ident>/<value>")
def hello_world(js_ident, value):
    print(js_ident)
    return f"js_ident is {js_ident} and value is {value}!"


# let's say that js server will send a message like:
# methods: "POST", json = "{js_ident: ??, value: ??}",
# the value part is optional.


@app.route("/", methods=['POST'])
def handle_post():
    # serialized_data = request.json
    # data = json.loads(serialized_data)
    print(request.get_json())
    return "hello"
