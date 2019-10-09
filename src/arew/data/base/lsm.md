
## `(arew data base lsm)`

### Abstract

LSM is an embedded database library for key-value data, roughly
similar in scope to Berkeley DB, LevelDB or KyotoCabinet. Both keys
and values are specified and stored as byte arrays. Duplicate keys are
not supported. Keys are always sorted in `memcmp()` order. LSM
supports the following operations for the manipulation and query of
database data:

- Writing a new key and value into the database.
- Deleting an existing key from the database.
- Deleting a range of keys from the database.
- Querying the database for a specific key.
- Iterating through a range of database keys (either forwards or backwards).

Other salient features are:

- A single-writer/multiple-reader MVCC based transactional concurrency
  model. SQL style nested sub-transactions are supported. Clients may
  concurrently access a single LSM database from within a single
  process or multiple application processes.

- An entire database is stored in a single file on disk.

- Data durability in the face of application or power failure. LSM may
  optionally use a write-ahead log file when writing to the database
  to ensure committed transactions are not lost if an application or
  power failure occurs.

See [sqlite's lsm extension
    documentation](https://sqlite.org/src4/doc/trunk/www/lsmusr.wiki).

### Reference

#### `(lsm-new)`

Open a database connection handle.

#### `(lsm-close db)`

Close a database connection handle.

#### `(lsm-config db config value)`

Configuring a database connection.

TODO: document configuration.

#### `(lsm-open db filename)`

Connect to a database.

#### `(lsm-begin db level)`

Open a transaction or sub-transaction. To open a top-level transaction
pass `1` as `LEVEL`. Passing `0` as `LEVEL` is no-op.

#### `(lsm-commit db level)`

Commit transaction and any sub-transactions. A successfull call to
`lsm-commit` ensures that there are at most `LEVEL` nested
transactions open. To commit a top-level transaction, pass `0` as
`LEVEL`. To commit all sub-transactions inside the main transaction,
pass `1` as `LEVEL`.

#### `(lsm-rollback db level)`

Roll back transaction and sub-transactions. A successful call to
`lsm-rollback` restores the database to the state it was in when the
`LEVEL`'th nested sub-transaction (if any) was first opened. And then
closes transactions to ensure that there are at most `LEVEL` nested
transactions open. Passing `0` as `LEVEL` rolls back and closes the
top-level transaction. `LEVEL` equal to `1` also rolls back the
top-level transaction, but leaves it open. `LEVEL` equal to `2` rolls
back the sub-transaction nested directly inside the top-level
transaction (and leaves it open).

#### `(lsm-insert db key value)`

Write a new value into the database where `KEY` and `VALUE` are
bytevectors. If a value with a duplicate key already exists it is
replaced.

#### `(lsm-delete db key)`

Delete a value from the database. No error is returned if the
specified `KEY` does not exist in the database.

#### `(lsm-delete-range db key1 key2)`

TODO: Implement it.

#### `(lsm-cursor-open db)`

Open a cursor.

#### `(lsm-cursor-close cursor)`

Close a cursor.

#### `(lsm-cursor-seek cursor key seek)`

Position the cursor at `KEY` according to `SEEK` symbol.

#### `(lsm-cursor-first cursor)`

Position the cursor at the first value.

#### `(lsm-cursor-last cursor)`

Position the cursor at the last value.

#### `(lsm-cursor-next cursor)`

Move the cursor at the next value.

#### `(lsm-cursor-prev cursor)`

Move the cursor at the previous value.

#### `(lsm-cursor-valid? cursor)`

Determine whether or not the cursor currently points to a valid entry.

#### `(lsm-cursor-key cursor)`

Retrieve the key as a bytevector.

#### `(lsm-cursor-value cursor)`

Retrieve the value as a bytevector.
