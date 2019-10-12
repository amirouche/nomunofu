
## `(arew stream)`

### Abstract

Wanna be fast and functional streams.

### Reference

#### `(list->stream lst)`

TODO

#### `(stream->list stream)`

TODO

#### `(stream-null)`

Return an empty stream.

#### `(stream-empty? stream)`

Return `#t` if the stream is empty. Otherwise, return `#f`.

#### `(stream-car stream)`

Return the first element of `STREAM`.

#### `(stream-map proc stream)`

Return a stream where `PROC` was applied to every value of `STREAM`.

#### `(stream-for-each proc stream)`

Apply `PROC` to every value of stream. The return value is undefined.

#### `(stream-filter predicate? stream)`

Return a stream of values from `STREAM` for which `PREDICATE?` return
`#t`.

#### `(stream-apppend . streams)`

Return a stream made of the given `STREAMS`.

#### `(stream-concatenate stream)`

`STREAM` must be stream of stream. Return a stream of the values.
