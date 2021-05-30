import ast
import astor
import json
import sys

p = json.loads(sys.stdin.read())

def object_to_ast(o):
    if isinstance(o, int):
        return o
    elif isinstance(o, str):
        return o
    elif isinstance(o, list):
        return [object_to_ast(x) for x in o]
    elif isinstance(o, dict):
        if not "ast_type" in o:
            raise Exception("Expected ast_type in object {}".format(o))
        ast_fn = getattr(ast, o["ast_type"])
        if ast_fn is None:
            raise Exception("Unknown ast type {}".format(o["ast_type"]))

        ast_fn_args = {}
        for k, v in o.items():
            if k == "ast_type":
                continue
            ast_fn_args[k] = object_to_ast(v)

        return ast_fn(**ast_fn_args)
    else:
        raise Exception("Unknown object {}".format(o))

print(astor.code_gen.to_source(object_to_ast(p)))
