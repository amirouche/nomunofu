(import (srfi :64)
        (scheme base)
        (scheme case-lambda)
        (scheme process-context))


(test-begin "scheme-case-lambda")

(define add1
  (case-lambda
    ((a) (add1 a 0))
    ((a b) (+ 1 a b))))

(test-equal "case-lambda-1" (add1 1) 2)
(test-equal "case-lambda-2" (add1 1 2) 4)

(test-end)


(define xpass (test-runner-xpass-count (test-runner-current)))
(define fail (test-runner-fail-count (test-runner-current)))
(if (and (= xpass 0) (= fail 0))
    (exit 0)
    (exit 1))
