;; This is a two-way mapping between scheme objects and an ulid.  This
;; is meant to save space and speed up queries.  It rely on
;; libsodium's generic hash digest as an intermediate internal
;; representation.
;;
;; TODO: use okvs/mapping
;;
(library (arew srfi srfi-167 ustore)

  (export maybe-object->ulid
          make-ustore
          object->ulid
          ulid->object)

  (import (arew scheme base)
          (arew base)
          (arew cffi sodium)
          (arew srfi srfi-167 ulid)
          (arew srfi srfi-167 engine))


  (define %hash->object '(0))
  (define %hash->ulid '(1))
  (define %ulid->hash '(2))

  (define-record-type <ustore>
    (make-ustore engine prefix)
    ustore?
    (engine ustore-engine)
    (prefix ustore-prefix))

  (define (maybe-object->ulid transaction ustore object)
    (let ((engine (ustore-engine ustore)))
      (let* ((value (engine-pack engine object))
             (hash (sodium-generic-hash value))
             (key (engine-pack engine
                               (append (ustore-prefix ustore)
                                       %hash->ulid
                                       (list hash)))))
        ;; try to get ulid from hash->ulid subspace
        (engine-ref engine transaction key))))

  (define (ustore-gensym transaction ustore)
    (let loop ((uid (random-bytes 16)))
      (if (maybe-object->ulid transaction ustore uid)
          (loop (random-bytes 16))
          uid)))

  (define (ask? transaction ustore uid)
    (let* ((engine (ustore-engine ustore)))
      (engine-ref engine transaction
                  (engine-pack engine
                               (append (ustore-prefix ustore)
                                       %ulid->hash
                                       (list uid))))))

  (define (ulid* transaction ustore)
    (let loop ((uid (ulid)))
      (if (ask? transaction ustore uid)
          (loop (ulid))
          uid)))

  (define (object->ulid transaction ustore object)
    (let ((engine (ustore-engine ustore)))
      (let* ((value (engine-pack engine object))
             (hash (sodium-generic-hash value))
             (key (engine-pack engine
                               (append (ustore-prefix ustore)
                                       %hash->ulid
                                       (list hash)))))
        ;; try to get ulid from hash->ulid subspace
        (let ((out (engine-ref engine transaction key)))
          (if out
              out
              ;; otherwise, create a new identifier and store it.
              (let ((out (ulid* transaction ustore)))
                (engine-set! engine transaction
                             (engine-pack engine
                                          (append (ustore-prefix ustore)
                                                  %hash->object
                                                  (list hash)))
                             value)

                (engine-set! engine transaction
                             (engine-pack engine
                                          (append (ustore-prefix ustore)
                                                  %hash->ulid
                                                  (list hash)))
                             out)

                (engine-set! engine transaction
                             (engine-pack engine
                                          (append (ustore-prefix ustore)
                                                  %ulid->hash
                                                  (list out)))
                             hash)

                out))))))

  (define (ulid->object transaction ustore ulid)
    "Retrieve the object with the given ULID. Otherwise return #f"
    (let ((engine (ustore-engine ustore)))
      ;; try to find the hash
      (and=>
       (engine-ref engine
                   transaction
                   (engine-pack engine
                                (append (ustore-prefix ustore)
                                        %ulid->hash
                                        (list ulid))))
       (lambda (hash)
         ;; there is a hash, there must be a value
         (let ((value (engine-ref engine transaction
                                  (engine-pack engine
                                               (append (ustore-prefix ustore)
                                                       %hash->object
                                                       (list hash))))))
           (car (engine-unpack engine value))))))))
