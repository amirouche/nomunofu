# nomunofu

**status: early draft**

Querying wikidata made easy

![no muss, no fuss](https://raw.githubusercontent.com/amirouche/nomunofu/master/luca-colapinto-I378DhssWqU-unsplash.jpg)

nomunofu is database server written in GNU Guile that is powered by
WiredTiger ordered key-value store, based on SRFI-167 and SRFI-168.

It allows to store and query triples. The goal is to make it much
easier, definitely faster to query as much as possible tuples of three
items. To achieve that goal, the server part of the database is made
very simple, and it only knows how to do pattern matching. Also, it is
possible to swap the storage engine to something that is horizontally
scalable and resilient (read: foundationdb).

Portable binaries for the current release (v0.1.3) with a small database
file can be retrieved with the following command:

  $ wget [https://hyper.dev/nomunofu-v0.1.3.tar.gz](https://hyper.dev/nomunofu-v0.1.3.tar.gz)

The uncompressed directory is 7GB.

Once you have downloaded the tarball, you can do the following cli
dance to run the database server:

$ tar xf nomunofu-v0.1.3.tar.gz && cd nomunofu && ./nomunofu serve 8080

The database will be available on port 8080. Then you can use the
python client `nomunofu.py` to do queries.

An example query that run on a subset of wikidata:

> instance of (P31) government (Q3624078)

The python code looks like:

```python
In [1]: from nomunofu import Nomunofu
In [2]: from nomunofu import var
In [3]: nomunofu = Nomunofu('http://localhost:8080');
In [4]: nomunofu.query(
(var('uid'),
 'http://www.wikidata.org/prop/direct/P31',
 'http://www.wikidata.org/entity/Q3624078'),
(var('uid'),
 'http://www.w3.org/2000/01/rdf-schema#label',
 var('label')))

Out[4]:
[{'uid': 'http://www.wikidata.org/entity/Q31',
'label': 'Belgium'},
 {'uid': 'http://www.wikidata.org/entity/Q183',
'label': 'Germany'},
 {'uid': 'http://www.wikidata.org/entity/Q148',
'label': 'China'},
 {'uid': 'http://www.wikidata.org/entity/Q148',
'label': "People's Republic of China"},
 {'uid': 'http://www.wikidata.org/entity/Q801',
'label': 'Israel'},
 {'uid': 'http://www.wikidata.org/entity/Q45',
'label': 'Portugal'},
 {'uid': 'http://www.wikidata.org/entity/Q155',
'label': 'Brazil'},
 {'uid': 'http://www.wikidata.org/entity/Q916',
'label': 'Angola'},
 {'uid': 'http://www.wikidata.org/entity/Q233',
'label': 'Malta'},
 {'uid': 'http://www.wikidata.org/entity/Q878',
'label': 'United Arab Emirates'},
 {'uid': 'http://www.wikidata.org/entity/Q686',
'label': 'Vanuatu'},
 {'uid': 'http://www.wikidata.org/entity/Q869',
'label': 'Thailand'},
 {'uid': 'http://www.wikidata.org/entity/Q863',
'label': 'Tajikistan'},
 {'uid': 'http://www.wikidata.org/entity/Q1049',
'label': 'Sudan'},
 {'uid': 'http://www.wikidata.org/entity/Q1044',
'label': 'Sierra Leone'},
 {'uid': 'http://www.wikidata.org/entity/Q912',
'label': 'Mali'},
 {'uid': 'http://www.wikidata.org/entity/Q819',
'label': 'Laos'},
 {'uid': 'http://www.wikidata.org/entity/Q298',
'label': 'Chile'},
 {'uid': 'http://www.wikidata.org/entity/Q398',
'label': 'Bahrain'},
 {'uid': 'http://www.wikidata.org/entity/Q12560',
'label': 'Ottoman Empire'}]
```

As of right now there is less than 10 000 000 triples that were
imported. Blank nodes are included, and only English labels are
imported.

You can grab the source code with the following command:

$ git clone [https://github.com/amirouche/nomunofu](https://github.com/amirouche/nomunofu)

I hope you have a good day!
