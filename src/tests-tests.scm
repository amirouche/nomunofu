(library (tests-tests)

  (export smoke-test)

  (import (chezscheme)
          (tests))

  (begin

    (define smoke-test
      (test #t #t))))
