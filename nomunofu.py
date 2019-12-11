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

    def _request(self, json, params=None):
        response = requests.get(self.url, json=json, params=params)
        if response.status_code == 400:
            raise NomunofuException(response.json())
        response.raise_for_status()
        return response

    def _valid(self, patterns):
        for pattern in patterns:
            if len(pattern) != 3:
                return False
            for item in pattern:
                if type(item) not in (int, float, str, var):
                    return False
        return True

    def _make_query(self, patterns):
        # Translate into what is expected by the server.  This will
        # rely only on lists because it is faster server side to
        # parse.  Still, it is valid JSON, albeit somewhat unusual.
        query = []
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
        return query

    def query(self, *patterns, limit=None, offset=None):
        assert self._valid(patterns)

        query = ['query'] + self._make_query(patterns)

        options = dict()
        if offset is not None:
            options['offset'] = offset
        if limit is not None:
            options['limit'] = limit

        response = self._request(query, options)

        # Then parse json lines using io.StringIO to allow to stream
        # lines and avoid the need for at least double the memory of
        # the response text.  Return a generator.
        for line in io.StringIO(response.text):
            if line:
                item = json.loads(line)
                yield item

    def _aggregation(self, operation, name, patterns):
        assert self._valid(patterns)
        query = [operation, name] + self._make_query(patterns)
        response = self._request(query)
        return response.json()

    def average(self, name, *patterns):
        return self._aggregation('average', name, patterns)

    def sum(self, name, *patterns):
        return self._aggregation('sum', name, patterns)

    def count(self, *patterns):
        return self._aggregation('count', False, patterns)

# helpers for rdf namespaces

def rdfschema(suffix):
    return 'http://www.w3.org/2000/01/rdf-schema#' + suffix

def schema(suffix):
    return "http://schema.org/" + suffix

def wikibase(suffix):
    return 'http://wikiba.se/ontology#' + suffix
