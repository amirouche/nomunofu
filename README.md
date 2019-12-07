# nomunofu

**status: early draft**

Querying wikidata made easy

This project aims to make querying wikidata much easier, and hopefully
also much faster.  No prior knowledge of SPARQL or semantic web tools
is required.  Good knowledge of Python is recommended.  Keep
[wikidata.org](https://wikidata.org) around.

There is two part in this application:

The **server** is shipped as a set of binaries for amd64 Ubuntu 18.04
(LTS) with a ready-made database files, that will allow you to spin
your own instance in no-time.  The server exposes a HTTP/1.0 interface
that allows to query for triples.  It is as simple as possible and
only allows to query using pattern matching.  The end-user does not
directly interact with the server.  Querying is done in Python see the
following paragraph.

The **client**, written in Python, is smarter!  It will query the server
and refine its results based on the user query.  User queries are
expressed using Python code.

## Python Client API

### Quick Start

```python
from nomunofu import Nomunofu
from nomunofu import var


nomunofu = Nomunofu('https://data.hyper.dev:8080')
out = nomunofu.query(
    ("Q1", "name", var("name"))
)
print(out)  # => [{'name': 'Universe'}]
```

### `var(name)`

Instantiate a variable named `name`.

### `Nomunofu(url)`

This will instantiate an object that will allow to query the server.
`url` must point to a nomunofu server.

### `Nomunofu.query(*patterns, limit=None, offset=None)`

Return a list of dictionaries where variables of `patterns` are
associated with the corresponding value as returned by the server.  A
pattern is a tuple of three items aka. triples that can have variable
object returned by `var(name)`, as an item.

### `Nomunofu.count(*patterns)`

Return the equivalent of `len(nomunofu.query(*patterns))` but in more
efficient way.

### `Nomunofu.sum(name, *patterns)`

Compute the sum of values taken by the variable `name` in `patterns`.

### `Nomunofu.avg(name, *patterns)`

Compute the average of values taken by the variable `name` in `patterns`.
