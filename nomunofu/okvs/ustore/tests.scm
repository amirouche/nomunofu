(define-module (nomunofu okvs ustore tests))

(import (nomunofu testing))
(import (nomunofu bytevector))
(import (nomunofu okvs engine))
(import (nomunofu okvs ustore))
(import (nomunofu okvs wiredtiger))


(define engine (make-default-engine))

(define ustore (make-ustore engine '(13 37)))

(define-public test-00
  (test  ;; check that ULID is 16 bytes.
   16
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (bytevector-length
                   (okvs-in-transaction okvs
                     (lambda (transaction)
                       (object->ulid transaction ustore 42))))))
         (engine-close engine okvs)
         out)))))

(define-public test-01
  (test  ;; check idempotency.
   42
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (ulid->object transaction ustore
                                    (object->ulid transaction ustore 42))))))
         (engine-close engine okvs)
         out)))))

(define-public test-02
  (test  ;; check same object returns same ulid.
   #t
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt")))
       (let ((out (okvs-in-transaction okvs
                    (lambda (transaction)
                      (bytevector=?
                       (object->ulid transaction ustore 42)
                       (object->ulid transaction ustore 42))))))
         (engine-close engine okvs)
         out)))))
