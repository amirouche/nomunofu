(library (arew data base wiredtiger-tests)

  (export test-000
          test-001)

  (import (tests)
          (arew scheme base)
          (arew base)
          (arew cffi stdlib)
          (only (chezscheme) mkdir string-append)
          (arew data base wiredtiger))

  (define (call-with-new-directory name proc)
    (let ((filepath (mkdtemp (string-append "/tmp/arew-wiredtiger-tests-" name "-"))))
      (proc filepath)))

  (define test-000
    (test #t
          (call-with-new-directory "000"
            (lambda (filepath)
              (let ((cnx (connection-open filepath "create")))
                (connection-close cnx "")
                #t)))))


  (define test-001
    (test
     #vu8(42)
     (call-with-new-directory "001"
       (lambda (filepath)
         (let* ((cnx (connection-open filepath "create"))
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
             out)))))))
