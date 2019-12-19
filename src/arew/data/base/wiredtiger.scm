#!chezscheme
;; TODO: replace void* in struct with function pointers
;; TODO: foreign-free make-double-pointer
(define-library (cffi wiredtiger)
  (export connection?
          connection-open
          connection-close
          session?
          session-open
          session-close
          session-create
          session-reset
          session-transaction-begin
          session-transaction-commit
          session-transaction-rollback
          cursor-open
          cursor?
          cursor-key-ref
          cursor-value-ref
          cursor-next?
          cursor-prev?
          cursor-reset
          cursor-search?
          cursor-search-near
          cursor-insert
          cursor-update
          cursor-remove
          cursor-close)

  (import (only (scheme base) pk))
  (import (except (chezscheme)
                  define-record-type
                  make-hash-table
                  hash-table?
                  hash-table-map
                  hash-table-for-each))

  (import (scheme hash-table))
  (import (scheme comparator))

  (import (only (arew) define-syntax-rule))

  (begin

    (define wiredtiger (load-shared-object "local/lib/libwiredtiger.so"))

    ;; ffi helpers

    (define (bytevector->pointer bv)
      (let ((pointer (foreign-alloc (* 8 (bytevector-length bv)))))
        (let loop ((index 0))
          (unless (= index (bytevector-length bv))
            (foreign-set! 'unsigned-8 pointer index (bytevector-u8-ref bv index))
            (loop (+ index 1))))
        pointer))

    (define-syntax-rule (check code)
      (let ((code* code))
        (unless (zero? code*)
          (error 'wiredtiger "error" code*))))

    (define (make-double-pointer)
      (foreign-alloc 8))

    (define-syntax-rule (pointer->ftype struct pointer)
      (make-ftype-pointer struct (foreign-ref 'void* pointer 0)))

    (define-syntax-rule (ftype->pointer ftype)
      (ftype-pointer-address ftype))

    (define-syntax-rule (foreign-procedure* return ptr args ...)
      (foreign-procedure ptr (args ...) return))

    ;; item

    (define-ftype %item
      (struct
       [data void*]
       [size size_t]
       [mem void*]
       [memsize size_t]
       [flags unsigned-32]))

    ;; connection

    (define-ftype %connection
      (struct
       [async-flush void*]
       [async-new-op void*]
       [close void*]
       [debug-info void*]
       [reconfigure void*]
       [get-home void*]
       [configure-method void*]
       [is-new void*]
       [open-session void*]
       [query-timestamp void*]
       [set-timestamp void*]
       [rollback-to-stable void*]
       [load-extensions void*]
       [add-data-source void*]
       [add-collator void*]
       [add-compressor void*]
       [add-encryptor void*]
       [add-extrator void*]
       [set-file-system void*]
       [get-extension-api void*]))

    (define (connection? obj)
      (ftype-pointer? %connection obj))

    (define connection-open
      (let ((proc (foreign-procedure* int "wiredtiger_open" string void* string void*)))
        (lambda (path config)
          (let ((out (make-double-pointer)))
            (check (proc path 0 config out))
            (pointer->ftype %connection out)))))

    (define (connection-close connection config)
      (let ((proc (foreign-procedure* int (ftype-ref %connection (close) connection) void* string)))
        (check (proc (ftype->pointer connection) config))))

    ;; session

    (define-ftype %session
      (struct
       [connection void*]
       [app-private void*]
       [close void*]
       [reconfigure void*]
       [strerror void*]
       [open-cursor void*]
       [alter void*]
       [create void*]
       [import void*]
       [compact void*]
       [drop void*]
       [join void*]
       [log-flush void*]
       [log-printf void*]
       [rebalance void*]
       [rename void*]
       [reset void*]
       [salvage void*]
       [truncate void*]
       [upgrade void*]
       [verify void*]
       [begin-transaction void*]
       [commit-transaction void*]
       [prepare-transaction void*]
       [rollback-transaction void*]
       [timestamp-transaction void*]
       [query-timestamp void*]
       [checkpoint void*]
       [snapshot void*]
       [transaction-pinned-range void*]
       [transaction-sync void*]
       [breakpoint void*]))

    (define (session? obj)
      (ftype-pointer? %session obj))

    (define (session-open connection config)
      (let* ((ptr (ftype-ref %connection (open-session) connection))
             (proc (foreign-procedure* int ptr void* void* string void*))
             (out (make-double-pointer)))
        (check (proc (ftype->pointer connection)
                     0
                     config
                     out))
        (pointer->ftype %session out)))

    (define (session-close session config)
      (let* ((ptr (ftype-ref %session (close) session))
             (proc (foreign-procedure* int ptr void* string)))
        (check (proc (ftype->pointer session) config))))

    (define (session-create session name)
      (let* ((ptr (ftype-ref %session (create) session))
             (proc (foreign-procedure* int ptr void* string string)))
        (check (proc (ftype->pointer session) name "key_format=u,value_format=u"))))

    (define (session-reset session)
      (let* ((ptr (ftype-ref %session (reset) session))
             (proc (foreign-procedure* int ptr void*)))
        (check (proc (ftype->pointer session)))))

    (define (session-transaction-begin session config)
      (let* ((ptr (ftype-ref %session (begin-transaction) session))
             (proc (foreign-procedure* int ptr void* string)))
        (check (proc (ftype->pointer session) config))))

    (define (session-transaction-commit session config)
      (let* ((ptr (ftype-ref %session (commit-transaction) session))
             (proc (foreign-procedure* int ptr void* string)))
        (check (proc (ftype->pointer session) config))))

    (define (session-transaction-rollback session config)
      (let* ((ptr (ftype-ref %session (rollback-transaction) session))
             (proc (foreign-procedure* int ptr void* string)))
        (check (proc (ftype->pointer session) config))))

    ;; cursor

    (define-ftype %cursor
      (struct
       [session void*]
       [uri void*]
       [key-format void*]
       [value-format void*]
       [get-key void*]
       [get-value void*]
       [set-key void*]
       [set-value void*]
       [compare void*]
       [equals void*]
       [next void*]
       [prev void*]
       [reset void*]
       [search void*]
       [search-near void*]
       [insert void*]
       [modify void*]
       [update void*]
       [remove void*]
       [reserve void*]
       [close void*]
       [reconfigure void*]
       [cache void*]
       [reopen void*]
       [uri-hash unsigned-64]
       [q (struct
           [tqe-next void*]
           [tqe-prev void*])]
       [recno unsigned-64]
       [raw-recno-buf (array 9 unsigned-8)] ;; size of unsigned 64 + 1
       [json-private void*]
       [lang-private void*]
       [key %item]
       [value %item]
       [saved-err int]
       [internal-uri void*]
       [flags unsigned-32]))

    (define (cursor-open session uri config)
      (let* ((ptr (ftype-ref %session (open-cursor) session))
             (proc (foreign-procedure* int ptr void* string void* string void*))
             (out (make-double-pointer)))
        (check (proc (ftype->pointer session)
                     uri
                     0
                     config
                     out))
        (pointer->ftype %cursor out)))

    (define (cursor? obj)
      (ftype-pointer? %cursor obj))

    (define (cursor-uri cursor)
      (ftype-ref %cursor (uri) cursor))

    (define (cursor-key-format cursor)
      (ftype-ref %cursor (key-format) cursor))

    (define (cursor-value-format cursor)
      (ftype-ref %cursor (value-format) cursor))

    (define (item->bytevector item) ;; XXX: copy!
      (let* ((item (make-ftype-pointer %item item))
             (data (ftype-ref %item (data) item))
             (size (ftype-ref %item (size) item))
             (bytevector (make-bytevector size)))
        (let loop ((index (- size 1)))
          (when (>= index 0)
            (let ((value (foreign-ref 'unsigned-8 data index)))
              (bytevector-u8-set! bytevector index value)
              (loop (- index 1)))))
        bytevector))

    (define (bytevector->item bytevector) ;; XXX: copy!
      (let* ((size (bytevector-length bytevector))
             (data (bytevector->pointer bytevector))
             (item (make-ftype-pointer %item (foreign-alloc (ftype-sizeof %item)))))
        (ftype-set! %item (data) item data)
        (ftype-set! %item (size) item size)
        item))

    (define (item-free item)
      (let ((data (ftype-ref %item (data) item)))
        (foreign-free data)
        (foreign-free (ftype-pointer-address item))))

    (define (cursor-value-ref cursor)
        (let ((out (make-double-pointer))
              (proc (foreign-procedure* void*
                                        (ftype-ref %cursor (get-value) cursor)
                                        void*
                                        void*)))
          (check (proc (ftype->pointer cursor) out))
          (item->bytevector out)))

    (define (cursor-value-set cursor bytevector)
      (let* ((ptr (ftype-ref %cursor (set-value) cursor))
             (proc (foreign-procedure* void ptr void* void*))
             (item (bytevector->item bytevector)))
        (proc (ftype->pointer cursor) (ftype-pointer-address item))
        item))

    (define (cursor-key-ref cursor)
      (let ((out (make-double-pointer))
            (proc (foreign-procedure* void*
                                      (ftype-ref %cursor (get-key) cursor)
                                      void*
                                      void*)))
        (check (proc (ftype->pointer cursor) out))
        (item->bytevector out)))

    (define (cursor-key-set cursor bytevector)
      (let* ((ptr (ftype-ref %cursor (set-key) cursor))
             (proc (foreign-procedure* void ptr void* void*))
             (item (bytevector->item bytevector)))
        (proc (ftype->pointer cursor) (ftype-pointer-address item))
        item))

    (define (cursor-next? cursor)
      (let* ((ptr (ftype-ref %cursor (next) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (let ((out (proc (ftype->pointer cursor))))
          (cond
           ((= out 0) #t)
           ((= out -31803) #f)
           (else (error 'wiredtiger "error" out))))))

    (define (cursor-prev? cursor)
      (let* ((ptr (ftype-ref %cursor (prev) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (let ((out (proc (ftype->pointer cursor))))
          (cond
           ((= out 0) #t)
           ((= out -31803) #f)
           (else (error 'wiredtiger "error" out))))))

    (define (cursor-reset cursor)
      (let* ((ptr (ftype-ref %cursor (reset) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (check (proc (ftype->pointer cursor)))))

    (define (%cursor-search cursor)
      (let* ((ptr (ftype-ref %cursor (search) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (let ((error (proc (ftype->pointer cursor))))
          (cond
           ((= error 0) #t)
           ((= error -31803) #f) ;; key not found
           (else (error 'wiredtiger "cursor-search failed" error))))))

    (define (cursor-search? cursor key)
      (let ((item (cursor-key-set cursor key)))
        (let ((out (%cursor-search cursor)))
          (item-free item)
          out)))

    (define (integer->nearness integer)
      (cond
       ((< 0 integer) 'after)
       ((= 0 integer) 'exact)
       ((> 0 integer) 'before)))

    (define (%cursor-search-near cursor)
      (let* ((ptr (ftype-ref %cursor (search-near) cursor))
             (proc (foreign-procedure* int ptr void* void*))
             (out (foreign-alloc 32))
             (error (proc (ftype->pointer cursor) out)))
        (let ((nearness (foreign-ref 'int out 0)))
          (foreign-free out)
          (cond
           ((= error 0)
            (let ((nearness (integer->nearness nearness)))
              nearness))
           ((= error -31803) ;; key not found
            #f)
           (else (error 'wiredtiger "error" error))))))

    (define (cursor-search-near cursor key)
      (let ((item (cursor-key-set cursor key)))
        (let ((out (%cursor-search-near cursor)))
          (item-free item)
          out)))

    (define (%cursor-insert cursor)
      (let* ((ptr (ftype-ref %cursor (insert) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (check (proc (ftype->pointer cursor)))))

    (define (cursor-insert cursor key value)
      (let ((key* (cursor-key-set cursor key))
            (value* (cursor-value-set cursor value)))
        (%cursor-insert cursor)
        (item-free key*)
        (item-free value*)))

    (define (%cursor-update cursor)
      (let* ((ptr (ftype-ref %cursor (update) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (check (proc (ftype->pointer cursor)))))

    (define (cursor-update cursor key value)
      (let ((key* (cursor-key-set cursor key))
            (value* (cursor-value-set cursor value)))
        (%cursor-update cursor)
        (item-free key*)
        (item-free value*)))

    (define (cursor-remove cursor key)
      (let* ((ptr (ftype-ref %cursor (remove) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (let ((item (cursor-key-set cursor key)))
          (check (proc (ftype->pointer cursor)))
          (item-free item))))

    (define (cursor-close cursor)
      (let* ((ptr (ftype-ref %cursor (close) cursor))
             (proc (foreign-procedure* int ptr void*)))
        (check (proc (ftype->pointer cursor)))))
    ))
