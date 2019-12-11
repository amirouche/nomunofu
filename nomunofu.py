import io
import json
import logging

import requests



log = logging.getLogger(__name__)


# client

class var:

    def __init__(self, name):
        self.name = name


class NomunofuException(Exception):
    pass


class Nomunofu:

    def __init__(self, url):
        self.url = url

    def query(self, *patterns, limit=None, offset=None):
        # Validation
        for pattern in patterns:
            assert len(pattern) == 3  # noqa
            for item in pattern:
                assert type(item) in (int, float, str, var)  # noqa

        # Translate into what is expected by the server.  This will
        # rely only on lists because it is faster server side to
        # parse.  Still, it is valid JSON, albeit somewhat unusual.
        query = ['query']
        for pattern in patterns:
            out = []
            for item in pattern:
                if isinstance(item, var):
                    # A variable is encoded as list with a single
                    # item.  This is an optim to avoid the have to
                    # parse a JSON Object server side.
                    out.append([item.name])
                else:
                    out.append(item)
            query.append(out)

        log.debug("query is: %r", query)

        params = dict()
        if offset is not None:
            params['offset'] = offset
        if limit is not None:
            params['limit'] = limit
        response = requests.get(self.url, data=json.dumps(query), params=params)
        if response.status_code == 400:
            raise NomunofuException(response.json())

        response.raise_for_status()

        # Then parse json lines using io.StringIO to allow to stream
        # lines and avoid the need for at least double the memory of
        # the response text.  Return a generator.
        for line in io.StringIO(response.text):
            if line:
                item = json.loads(line)
                yield item

# helpers for rdf namespaces

def rdfschema(suffix):
    return 'http://www.w3.org/2000/01/rdf-schema#' + suffix

def schema(suffix):
    return "http://schema.org/" + suffix
