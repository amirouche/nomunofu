(define-library (gleckler hamt-misc-test)
  (import (scheme base) (srfi :64) (gleckler hamt-misc))
  (export run-hamt-misc-tests)
  (include "hamt-misc-test.scm"))
