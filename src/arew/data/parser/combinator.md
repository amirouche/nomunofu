
## `(arew data parser combinator)`

### Abstract

parser combinators inspired from the following projects:

- https://epsil.github.io/gll/
- https://docs.racket-lang.org/parsack/index.html
- https://docs.racket-lang.org/megaparsack/
- https://git.dthompson.us/guile-parser-combinators.git
- https://gitlab.com/tampe/stis-parser

### Reference

#### `(make-pseudo-xchar char)`

Make an extended character without line, column or offset information.

#### `(parse parser stream)`

Parse `STREAM` using `PARSER`. Return `#f` in case of error.

#### `(parse-any stream)`

Parser that succeed with anything in `STREAM`.

#### `(parse-char char)`

Parser that succeed with the given `CHAR`

#### `(parse-char-set char-set)`

Parser that succeed with the given `CHAR-SET` from `(scheme charset)`.

#### `(parse-each <parser> ...)` syntax

Parser that succeed if every parser succeed in sequence.

#### `(parse-either <parser> ...)` syntax

Parser that succeed when the first given parser succeed.

#### `(parse-lift proc parser)`

Apply `PROC` to the result of `PARSER`.

#### `(parse-maybe parser)`

If `PARSER` succeed return its result, otherwise return `#f` as result.

#### `(parse-one-or-more parser)`

Parser that succeed if `PARSER` succeed one or more time.

#### `(parse-only predicate? parser)`

Parser that succeed only when the result of `PARSER` returns `#t` when
passed to `PREDICATE?`.

#### `(parse-return value`)

Parser that always succeed with `VALUE` as result.

#### `(parse-xstring string)`

Parser that succeed when `STRING` can be parsed as a sequence of
extended characters.

#### `(parse-when predicate? parser)`

Parser that succeed when the next value returns `#t` when passed as
argument to `PREDICATE?`. Return the result of `PARSER`.

#### `(parse-when* parser other)`

More general form of `parse-when`. If `PARSER` succeed, return the
result of `OTHER` parser.

#### `(parse-unless predicate? parser)`

Parser that succeed unless the next value returns `#t` when passed as
argument to `PREDICATE?`. Return the result of `PARSER`.

#### `(parse-unless* parser other)`

More general form of `parse-unless`. If `PARSER` fails, return the
result of `OTHER` parser.

#### `(parse-xchar char)`

Parser that succeed if the next value is an extended character that is a
`CHAR`.

#### `(parse-zero-or-more parser)`

Succeed if `PARSER` succeed zero or more time.

#### `(string->stream string)`

Create a stream of extended characters based on `STRING`.

#### `(xchar-char xchar)`

Return the character of `XCHAR`.

#### `(xchar? obj)`

Return `#t` if `OBJ` is an extended character. Otherwise, return `#f`.
