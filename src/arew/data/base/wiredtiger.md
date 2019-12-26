## `(cffi wiredtiger)`

### `(connection? obj)`

Return `#t` if `OBJ` is a connection, otherwise `#f`.

### `(connection-open path config)`

Open a new connection at `PATH` using `CONFIG`.

### `(connection-close connection config)`

Close `CONNECTION` using `CONFIG`.

### `(session? obj)`

Return `#t` if `OBJ` is a session, otherwise `#f`.

### `(session-open connection config)`

Open a session against `CONNECTION` using `CONFIG`.

### `(session-close session config)`

Close `SESSION` using `CONFIG`.

### `(session-create session name)`

Create a table named `NAME` using `SESSION` with a bytevector column
as key and value.

### `(session-reset session)`

Reset `SESSION`.

### `(session-transaction-begin session config)`

Begin transaction against `SESSION` using `CONFIG`.

### `(session-transaction-commit session config)`

Commit transaction against `SESSION` using `CONFIG`.

### `(session-transaction-rollback session config)`

Rollback transaction against `SESSION` using `CONFIG`.

### `(cursor-open session uri config)`

Open a cursor against `SESSION` at `URI` using `CONFIG`.

### `(cursor? cursor)`

Return `#t` if `CURSOR` is a cursor. Otherwise return `#f`.

### `(cursor-close cursor)`

Close `CURSOR`.

### `(cursor-uri cursor)`

Return `CURSOR`'s uri.

### `(cursor-key-format cursor)`

Return `CURSOR`'s key format.

### `(cursor-value-format cursor)`

Return `CURSOR`'s value format.

### `(cursor-key-ref cursor)`

Return the key pointed by `CURSOR` as a bytevector.

### `(cursor-value-ref cursor)`

Return the value pointed by `CURSOR` as a bytevector.

### `(cursor-next? cursor)`

Move `CURSOR` to the next record, return `#t` if any. Otherwise it
return `#f`.

### `(cursor-prev? cursor)`

Move `CURSOR` to the previous record, return `#t` if any. Otherwise it
return `#f`.

### `(cursor-reset cursor)`

Reset `CURSOR`.

### `(cursor-search? cursor key)`

Try to position `CURSOR` at `KEY`. Return `#t` if it succeed.
Otherwise it return `#f`.

### `(cursor-search-near cursor key)`

Try to position `CURSOR` near `KEY`. Return `'before`, `'exact` or
`'after` depending on where the cursor is positioned. Otherwise it
return `#f`.

### `(cursor-insert cursor key value)`

Insert `KEY` and `VALUE` using `CURSOR`.

### `(cursor-update cursor key value)`

Update `KEY` with `VALUE` using `CURSORS`.

### `(cursor-remove cursor key)`

Remove record that has `KEY` as key using `CURSOR`.
