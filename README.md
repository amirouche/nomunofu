# nomunofu

**status: wip**

Querying wikidata made easy

![two origami frogs, one on top of the
 other](https://raw.githubusercontent.com/amirouche/nomunofu/wip-chez/origami-frogs.jpg)

nomunofu is database server written in Scheme programming language
that is powered by WiredTiger ordered key-value store, based on
[SRFI-167](https://srfi.schemers.org/srfi-167/) and
[SRFI-168](https://srfi.schemers.org/srfi-168/).

It allows to store and query triples, quads **and more**.  The goal is
to make it much easier, definitely faster to query as much as possible
tuples.  To achieve that goal, the server part of the database is made
very simple, and it only knows how to do pattern matching and count,
sum and average aggregation.  Also, it is possible to swap the storage
engine to something that is horizontally scalable and resilient (read:
[FoundationDB](https://apple.github.io/foundationdb/)).

The *thin server*, *thick client* paradigm was choosen to allow the
end-user to more easily workaround bugs in the data, and it also
allows to offload the servers hosting the data from heavy
computations.

Happy hacking!

[Amirouche](mailto:amirouche@hyper.dev) ~ zig ~ https://hyper.dev
