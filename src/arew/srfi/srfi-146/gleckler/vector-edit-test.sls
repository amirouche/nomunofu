(define-library (gleckler vector-edit-test)
  (import (scheme base) (srfi :64) (gleckler vector-edit))
  (export run-vector-edit-tests)
  (include "vector-edit-test.scm"))
