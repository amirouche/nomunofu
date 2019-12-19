(import (srfi :64)
        (arew)
        (prefix (arew filename) filename:)
        (scheme base)
        (scheme process-context)
        (cffi wiredtiger))


(define-syntax-rule (with-directory name body ...)
  (begin
    (when (filename:exists? name)
      (filename:delete name))
    (filename:make name)
    (let ((out (begin body ...)))
      (filename:delete name)
      out)))

(test-begin "wiredtiger")

(test-equal "connection-open"
  #t
  (with-directory "wiredtiger-tests"
    (let ((cnx (connection-open "wiredtiger-tests" "create")))
      (connection-close cnx "")
      #t)))


(test-equal "nominal"
  #vu8(42)
  (with-directory "wiredtiger-tests"
    (let* ((cnx (connection-open "wiredtiger-tests" "create"))
           (session (session-open cnx ""))
           (_1 (session-create session "table:tests"))
           (cursor (cursor-open session "table:tests" "")))
      (session-transaction-begin session "")
      (cursor-insert cursor #vu8(101) #vu8(42))
      (session-transaction-commit session "")
      (session-transaction-begin session "")
      (cursor-search? cursor #vu8(101))
      (let ((out (cursor-value-ref cursor)))
        (session-transaction-commit session "")
        (cursor-close cursor)
        (session-reset session)
        (session-close session "")
        (connection-close cnx "")
        out))))

(test-end)


(define xpass (test-runner-xpass-count (test-runner-current)))
(define fail (test-runner-fail-count (test-runner-current)))
(if (and (= xpass 0) (= fail 0))
    (exit 0)
    (exit 1))
