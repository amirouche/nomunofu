# arew-scheme

[![builds.sr.ht status](https://builds.sr.ht/~amz3/arew-scheme/.build.yml.svg)](https://builds.sr.ht/~amz3/arew-scheme/.build.yml?)

Various libraries for Chez Scheme (R7RS, SRFI and more...)

[![people walking in a library](https://raw.githubusercontent.com/amirouche/arew-scheme/master/gabriel-sollmann-Y7d265_7i08-unsplash.jpg)](https://github.com/amirouche/arew-scheme)


## Status

- no: not planned
- wip: need more tests or documentation
- ok: code, doc and tests were reviewed


### Next step

- SRFI-167: engine: add make-default-engine, see mailing-list
- SRFI-167: memory: add options, errors and tests hooks
- SRFI-167: wiredtiger: add options, errors and tests hooks
- SRFI-168: add options, errors and tests hooks
- SRFI-173: almost ready

### R7RS-small

source: http://r7rs.org/

source: https://akkuscm.org/

- scheme base: wip
- scheme case-lambda: wip
- scheme char: wip
- scheme complex: wip
- scheme cxr: wip
- scheme eval: wip
- scheme file: wip
- scheme inexact: wip
- scheme lazy: wip
- scheme load: wip
- scheme process-context: wip
- scheme r5rs: wip
- scheme read: wip
- scheme repl: wip
- scheme time: wip
- scheme write: wip


### R7RS-large

source: http://r7rs.org/

source: https://peterlane.netlify.com/chez-libs/

- scheme bitwise: SRFI-151: wip
- scheme box: SRFI-111: wip
- scheme bytevector: rnrs bytevectors: wip
- scheme charset: SRFI-14:
- scheme comparator: SRFI-128: wip
- scheme division: SRFI-141:
- scheme ephemeron: SRFI-124: wip
- scheme fixnum: SRFI-143:
- scheme flonum: SRFI-144:
- scheme generator: SRFI-158: wip
- scheme hash-table: SRFI-125: wip
- scheme idque: SRFI-134:
- scheme ilist: SRFI-116:
- scheme list-queue: SRFI-117:
- scheme list: SRFI-1: wip
- scheme lseq: SRFI-127:
- scheme mapping hash: SRFI-146: wip
- scheme mapping: SRFI-146: wip
- scheme regex: SRFI-115:
- scheme rlist: SRFI-101:
- scheme set: SRFI-113:
- scheme show: SRFI-159:
- scheme sort: SRFI-132:
- scheme stream: SRFI-41:
- scheme text: SRFI-135:
- scheme vector @: SRFI-160:
- scheme vector: SRFI-133:


### SRFI

source: https://srfi.schemers.org/

source: https://github.com/arcfide/chez-srfi

source: https://peterlane.netlify.com/chez-libs/

- SRFI-1: scheme list: wip
- SRFI-2:
- SRFI-4:
- SRFI-5:
- SRFI-6:
- SRFI-8:
- SRFI-9:
- SRFI-11:
- SRFI-13:
- SRFI-14: scheme charset:
- SRFI-16:
- SRFI-17:
- SRFI-19:
- SRFI-23:
- SRFI-25:
- SRFI-26:
- SRFI-27:
- SRFI-28:
- SRFI-29:
- SRFI-31:
- SRFI-34:
- SRFI-35:
- SRFI-37:
- SRFI-38:
- SRFI-39:
- SRFI-41: scheme stream:
- SRFI-42:
- SRFI-43:
- SRFI-45:
- SRFI-48:
- SRFI-51:
- SRFI-54:
- SRFI-60:
- SRFI-61:
- SRFI-64: no
- SRFI-67:
- SRFI-69:
- SRFI-78: no
- SRFI-98:
- SRFI-99:
- SRFI-101: scheme rlist:
- SRFI-111: scheme box: wip
- SRFI-113: scheme set:
- SRFI-115: scheme regex:
- SRFI-116: scheme ilist:
- SRFI-117: scheme list-queue:
- SRFI-124: scheme ephemeron: wip
- SRFI-125: scheme hash-table: wip
- SRFI-126:
- SRFI-127: scheme lseq:
- SRFI-128: scheme comparator: wip
- SRFI-129:
- SRFI-132: scheme sort:
- SRFI-133: scheme vector:
- SRFI-134: scheme idque:
- SRFI-135: scheme text:
- SRFI-141: scheme division:
- SRFI-143: scheme fixnum:
- SRFI-144: scheme flonum:
- SRFI-145:
- SRFI-146: scheme mapping: wip
- SRFI-146: scheme mapping hash: wip
- SRFI-151: scheme bitwise:
- SRFI-152:
- SRFI-156:
- SRFI-158: scheme generator: wip
- SRFI-159: scheme show:
- SRFI-160: scheme vector @:
- SRFI-167: pack:
- SRFI-167: engine:
- SRFI-167: memory:
- SRFI-167: wiredtiger:
- SRFI-168:
- SRFI-173: ok


### Purely Functional Data Structures in Scheme

source: https://github.com/ijp/pfds/

- pfds queues:
- pfds deques:
- pfds bbtrees:
- pfds sets:
- pfds dlists:
- pfds priority-search-queues:
- pfds finger trees: wip
- pfds sequences:
- pfds heaps:
- pfds hamts:


### Wishlist

- argon2
- ascii table
- ascii tree
- background task scheduler
- blake2
- bsd sockets
- calendar
- cell terminal (termbox, libtickit, cowan)
- command-line parser
- commonmark
- datetime
- elasticsearch alternative
- fibers (non blocking ports + channels)
- formatter
- graph (e.g. networkx)
- headless browser automation
- html->sxml
- http client
- http server
- image manipulation
- industria cryptography
- json
- linter
- logging, structured logging
- matplotlib / dask / plotly alternative
- pathlib
- psutil
- redis alternative
- snowball stemmer
- subprocess
- sxml->html
- terminal colors
- xml->sxml: https://github.com/jclark/microxml-js or https://dev.yorhel.nl/yxml
- xpath
