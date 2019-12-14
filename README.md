# nomunofu

**status: wip**

Querying wikidata made easy

![no muss, no fuss](https://raw.githubusercontent.com/amirouche/nomunofu/master/luca-colapinto-I378DhssWqU-unsplash.jpg)

nomunofu is database server written in GNU Guile that is powered by
WiredTiger ordered key-value store, based on
[SRFI-167](https://srfi.schemers.org/srfi-167/) and
[SRFI-168](https://srfi.schemers.org/srfi-168/).

It allows to store and query triples, quads and more.  The goal is to
make it much easier, definitely faster to query as much as possible
tuples.  To achieve that goal, the server part of the database is made
very simple, and it only knows how to do pattern matching and count,
sum and average aggregation.  Also, it is possible to swap the storage
engine to something that is horizontally scalable and resilient (read:
[FoundationDB](https://apple.github.io/foundationdb/)).

The *thin server*, *thick client* was choosen to allow the end-user to
more easily workaround bugs in the data, and it also allows to offload
the servers hosting the data from heavy computations.

Portable binaries for the current release v0.2.0-alpha3 can be
retrieved with the following command:

> $ wget [https://hyper.dev/nomunofu-v0.2.0-alpha3.tar.gz](https://hyper.dev/nomunofu-v0.2.0-alpha3.tar.gz)

Once you have downloaded the tarball, decompress the archive with the
follwing command:

> $ tar xf nomunofu-v0.2.0.tar.gz && cd nomunofu

`nomunofu` is a **generic** tuple store, simply said, an nstore.  You
have to pass the count of tuple items to commands.  You will find on
the internet `.nt` turtle files for with three or four items per
tuple.  `nomunofu` support more that 4 items per tuple.

Let's imagine that you have `FILENAME` turtle file that has tuples with
three items.  You can import it inside nomunofu using the following
command:

> ./nomunofu index 3 FILENAME

Then, to start the web server you do the following command:

> ./nomunofu serve 3 8080

The database will be available on port 8080.  The database server
speaks scheme.  There is a tiny scheme client inside
`nomunofu/client.scm`.  You can grab the source code with the
following command:

> $ git clone [https://github.com/amirouche/nomunofu](https://github.com/amirouche/nomunofu)

Happy hacking!


[Amirouche](mailto:amirouche@hyper.dev) ~ zig ~ https://hyper.dev
