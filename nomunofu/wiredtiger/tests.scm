(define-module (nomunofu wiredtiger tests))

(import (nomunofu testing))
(import (nomunofu wiredtiger))
(import (rnrs bytevectors))


;; helper: (with-cnx cnx body ...)

(define-syntax-rule (with-connection name connection e ...)
  ;; TODO: use guard to close connection in case of exception
  (with-directory "wt"
    (let* ((name connection)
           (out (begin e ...)))
      (connection-close name "")
      out)))


(define-public test-000
  (test #t (with-connection cnx (connection-open "wt" "create") #t)))

(define-public test-001
  (test
   (list (list "a" "b") (list 42))
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       ;; create a table
       (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
       (session-create session "index:nodes:index" "columns=(b,c)")
       ;; open a cursor over that table
       (let ((cursor (cursor-open session "table:nodes" "")))
         (session-transaction-begin session "isolation=\"snapshot\"")
         (cursor-key-set cursor 42)
         (cursor-value-set cursor "a" "b")
         (cursor-insert cursor)
         (session-transaction-commit session "")
         (let ((index (cursor-open session "index:nodes:index(a)" "")))
           (cursor-next? index)
           (list (call-with-values (lambda () (cursor-key-ref index)) list)
                 (call-with-values (lambda () (cursor-value-ref index)) list))))))))

(define-public test-002
  (test
   #f
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       ;; create a table
       (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
       ;; open a cursor over that table
       (let ((cursor (cursor-open session "table:nodes" "")))
         (cursor-key-set cursor 42)
         (cursor-search? cursor))))))

(define-public test-003
  (test
   #t
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       ;; create a table
       (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
       ;; open a cursor over that table
       (let ((cursor (cursor-open session "table:nodes" "")))
         (cursor-key-set cursor 42)
         (cursor-value-set cursor "b" "c")
         (cursor-insert cursor)
         (cursor-key-set cursor 42)
         (cursor-search? cursor))))))

(define-public test-004
  (test
   #f
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
       (let ((cursor (cursor-open session "table:nodes" "")))
         (cursor-key-set cursor 42)
         (cursor-search-near cursor))))))

(define-public test-005
  (test
   'before
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
       (let ((cursor (cursor-open session "table:nodes" "")))
         ;; prepare
         (cursor-key-set cursor 42)
         (cursor-value-set cursor "magic number")
         (cursor-insert cursor)
         ;; test
         (cursor-key-set cursor 43)
         (cursor-search-near cursor))))))

(define-public test-006
 (test
  'after
  (with-connection cnx (connection-open "wt" "create")
    (let ((session (session-open cnx "")))
      (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
      (let ((cursor (cursor-open session "table:nodes" "")))
        ;; prepare
        (cursor-key-set cursor 42)
        (cursor-value-set cursor "magic number")
        (cursor-insert cursor)
        ;; test
        (cursor-key-set cursor 41)
        (cursor-search-near cursor))))))

(define-public test-007
  (test
   'after
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
       (let ((cursor (cursor-open session "table:nodes" "")))
         ;; prepare
         (cursor-key-set cursor 41)
         (cursor-value-set cursor "another number")
         (cursor-insert cursor)
         (cursor-key-set cursor 42)
         (cursor-value-set cursor "magic number")
         (cursor-insert cursor)
         (cursor-key-set cursor 45)
         (cursor-value-set cursor "random number")
         (cursor-insert cursor)
         ;; test
         (cursor-key-set cursor 43)
         (cursor-search-near cursor))))))

(define-public test-008
  (test
   'exact
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
       (let ((cursor (cursor-open session "table:nodes" "")))
         ;; prepare
         (cursor-key-set cursor 41)
         (cursor-value-set cursor "another number")
         (cursor-insert cursor)
         (cursor-key-set cursor 42)
         (cursor-value-set cursor "magic number")
         (cursor-insert cursor)
         (cursor-key-set cursor 45)
         (cursor-value-set cursor "random number")
         (cursor-insert cursor)
         ;; test
         (cursor-key-set cursor 42)
         (cursor-search-near cursor))))))

(define-public test-009
  (test
   1
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       (session-create session "table:terms" "key_format=r,value_format=S")
       (let ((cursor (cursor-open session "table:terms" "append")))
         (cursor-value-set cursor "timesink")
         (cursor-insert cursor)
         (cursor-key-ref cursor))))))

(define-public test-010
  (test
  '(41 42 43)
   (with-connection cnx (connection-open "wt" "create")
     (let ((session (session-open cnx "")))
       (session-create session "table:terms" "key_format=r,value_format=u")
       (let ((cursor (cursor-open session "table:terms" "append")))
         (cursor-value-set cursor #vu8(41 42 43))
         (cursor-insert cursor)
         (cursor-key-set cursor 1)
         (cursor-search? cursor)
         ;; make a copy as list
         (bytevector->u8-list (cursor-value-ref cursor)))))))
