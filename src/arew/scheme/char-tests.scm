(import (srfi :64)
        (scheme base)
        (scheme char)
        (scheme process-context))

(test-begin "scheme-char")

(test-end)


(define xpass (test-runner-xpass-count (test-runner-current)))
(define fail (test-runner-fail-count (test-runner-current)))
(if (and (= xpass 0) (= fail 0))
    (exit 0)
    (exit 1))
