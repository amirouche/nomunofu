
## `(arew data json)`

### Abstract

Provide a `(json->scm string)` procedure that parse a json string into
a Scheme object.

### Reference

#### `(json->scm string)`

Return a Scheme object representation of the JSON `STRING`. In case of
error, return `#f`.

Arrays are represented as a list. JSON Objects are represented as an
association list prefixed with a star symbol where keys are symbols.
