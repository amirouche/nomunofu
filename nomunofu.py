import requests
import json
import logging


log = logging.getLogger(__name__)


class var:

    def __init__(self, name):
        self.name = name


class Nomunofu:

    def __init__(self, url):
        self.url = url

    def query(self, *patterns):
        data = ''
        for pattern in patterns:
            line = '('
            for item in pattern:
                if isinstance(item, var):
                    line += " (var . {}) ".format(item.name)
                elif isinstance(item, str):
                    line += " \"{}\" ".format(item.replace("\"", "\\\""))
                elif isinstance(item, float) or isinstance(item, int):
                    line += " {} ".format(item)
            data += line + ")\n"
        log.debug("request data is: %s", data)
        response = requests.get(self.url, data=data)
        response.raise_for_status()
        out = []
        for line in response.text.split('\n'):
            if line:
                item = json.loads(line)
                out.append(item)
        return out
