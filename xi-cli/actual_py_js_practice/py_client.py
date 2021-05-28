import requests
import json
url = "http://localhost:8080"

data = {'request_type': 'reg_id', 'var_name':  'var_127'}
# json_data = json.dumps(data)

test = requests.post(url, json=data)

print(test.text)
