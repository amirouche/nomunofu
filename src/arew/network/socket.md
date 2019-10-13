
## `(arew network socket)`

### Abstract

BSD Sockets

### Reference

#### `(close fd)`

Close the socket described by `FD`.

#### `(socket domain type protocol)`

`DOMAIN` can be one the following symbol:

- `unspecified`
- `unix` or `local`
- `inet`
- `inet6`

`TYPE` can be one the following symbol:

- `stream`
- `datagram`
- `raw`
- `rdm`
- `seqpacket`
- `dccp`
- `packet`
- `close-on-exec`
- `non-blocking`

`PROTOCOL` can be one the following symbol:

- `ipv4`
- `icmp`
- `igmp`
- `ipip`
- `gcp`
- `egp`
- `pup`
- `udp`
- `idp`
- `tp`
- `dccp`
- `ipv6`
- `rsvp`
- `gre`
- `esp`
- `ah`
- `beetph`
- `encap`
- `pim`
- `comp`
- `sctp`
- `udplite`
- `mpls`
- `raw`

#### `(connect sockfd address)`

`SOCKFD` must be socket opened with `socket`. `ADDRESS` must be an
association list with the following keys:

- `family` only `inet` value is supported
- `port` a number below 65536
- `address` a string describing an ipv4 address.

#### `(getaddrinfo node service hints)`

`NODE`, `SERVICE` and `HINTS` may be false. If `HINTS` is provided it
must be an association list with the following keys:

- `flags`
- `family`
- `type`
- `protocol`

`flags` value can be one or more the following:

- `passive`
- `canonname`
- `v4mapped`
- `numerichost`
- `all`
- `addrconfig`
- `idn`
- `canonidn`
- `idn-allow-unassigned`
- `idn-use-std3-ascii-rules`

`family`, `type` and `protocol` are described above.

#### `(recv fd bytevector flags)`

`flags` can be `#f` one or more of the following symbols:

- `oob`
- `peek`
- `tryhard`
- `ctrunc`
- `proxy`
- `trunc`
- `dontwait`
- `eor`
- `waitall`
- `fin`
- `syn`
- `confirm`
- `rst`
- `errqueue`
- `nosignal`
- `more`
- `waitforone`
- `batch`
- `fastopen`
- `cmsg-cloexec`

It will return `-1` on error and `errno` is set. Otherwise it return
the count bytes read.

#### `(recvfrom fd bytevector flags)`

`FLAGS` is described above. Return two values. The first value `#f` in
case of error and `errno` can be used to retrieve the error. Otherwise
it return the count bytes read along the address as an association
list.

#### `(send fd bytevector flags)`

`FLAGS` is described above. In case of success return the count of
bytes sent. Otherwise, it return `-1` and `errno` can be used to
retrieve the error.

#### `(sendto fd bytevector flags address)`

`FLAGS` is described above. `address` must be an association list
description of the destination.

#### `(accept sock)`

Return the file descriptor of the accepted connection. Otherwise
raise an error.

#### `(bind sock address)`

#### `(fcntl sock command)`

#### `(fcntl! sock command flags)`

#### `(setsockopt socket level optname optval optlen)`

#### `(listen sock backlog)`

## `(fd->port fd)`

Return a port based on `FD`.
