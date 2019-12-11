#!/usr/bin/env python
from nomunofu import *


nomunofu = Nomunofu('http://localhost:8080')
out = nomunofu.query(
    (
        var('uid'),
        rdfschema('label'),
        "Belgium",
    ),
    (
        var('about'),
        schema('about'),
        var('uid'),
    ),
    limit=5,
    offset=7,
)

for item in out:
    print(item)
