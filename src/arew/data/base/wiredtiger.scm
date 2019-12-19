#!chezscheme
(library (arew data base wiredtiger)
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
          cursor-remove
          cursor-close)

  (import (except (chezscheme)
                  define-record-type
                  make-hash-table
                  hash-table?
                  hash-table-map
                  hash-table-for-each
                  raise)
          (only (arew scheme base) raise)
          (arew scheme hash-table)
          (arew scheme comparator)
          (arew base))

  (begin

    (define wiredtiger (load-shared-object "local/lib/libwiredtiger.so"))

    ;; bytevector cffi helper

    (define (bytevector->pointer bv)
      (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

    (define (call-with-lock obj thunk)
      (lock-object obj)
      (call-with-values thunk
        (lambda out
          (unlock-object obj)
          (apply values out))))

    ;; ffi helpers

    (define-syntax-rule (check code)
      (let ((code* code))
        (unless (zero? code*)
          (raise (cons 'wiredtiger code*)))))

    (define (make-double-pointer)
      (foreign-alloc 8))

    (define-syntax-rule (pointer->ftype struct pointer)
      (make-ftype-pointer struct (foreign-ref 'void* pointer 0)))

    (define-syntax-rule (ftype->pointer ftype)
      (ftype-pointer-address ftype))

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
       [close (* (function (void* string) int))]
       [debug-info void*]
       [reconfigure void*]
       [get-home void*]
       [configure-method void*]
       [is-new void*]
       [open-session (* (function (void* void* string void*) int))]
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
      (let ((proc (foreign-procedure "wiredtiger_open" (string void* string void*) int)))
        (lambda (path config)
          (let ((out (make-double-pointer)))
            (check (proc path
                         0
                         config
                         out))
            (pointer->ftype %connection out)))))

    (define (connection-close connection config)
      (let ((proc (ftype-ref %connection (close *) connection)))
        (check (proc (ftype->pointer connection) config))))

    ;; session

    (define-ftype %session
      (struct
       [connection void*]
       [app-private void*]
       [close (* (function (void* string) int))]
       [reconfigure void*]
       [strerror void*]
       [open-cursor (* (function (void* string void* string void*) int))]
       [alter void*]
       [create (* (function (void* string string) int))]
       [import void*]
       [compact void*]
       [drop void*]
       [join void*]
       [log-flush void*]
       [log-printf void*]
       [rebalance void*]
       [rename void*]
       [reset (* (function __collect_safe (void*) int))]
       [salvage void*]
       [truncate void*]
       [upgrade void*]
       [verify void*]
       [begin-transaction (* (function (void* string) int))]
       [commit-transaction (* (function (void* string) int))]
       [prepare-transaction void*]
       [rollback-transaction (* (function (void* string) int))]
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
      (let* ((proc (ftype-ref %connection (open-session *) connection))
             (out (make-double-pointer)))
        (check (proc (ftype->pointer connection)
                     0
                     config
                     out))
        (pointer->ftype %session out)))

    (define (session-close session config)
      (let* ((proc (ftype-ref %session (close *) session)))
        (check (proc (ftype->pointer session) config))))

    (define %config "key_format=u,value_format=u")

    (define (session-create session name)
      (let* ((proc (ftype-ref %session (create *) session)))
        (check (proc (ftype->pointer session)
                     name
                     %config))))

    (define (session-reset session)
      (let ((proc (ftype-ref %session (reset *) session)))
        (check (proc (ftype->pointer session)))))

    (define (session-transaction-begin session config)
      (let ((proc (ftype-ref %session (begin-transaction *) session)))
        (check (proc (ftype->pointer session) config))))

    (define (session-transaction-commit session config)
      (let ((proc (ftype-ref %session (commit-transaction *) session)))
        (check (proc (ftype->pointer session) config))))

    (define (session-transaction-rollback session config)
      (let ((proc (ftype-ref %session (rollback-transaction) session)))
        (check (proc (ftype->pointer session) 0))))

    ;; cursor

    (define-ftype %cursor
      (struct
       [session void*]
       [uri void*]
       [key-format void*]
       [value-format void*]
       [get-key (* (function __collect_safe (void* void*) int))]
       [get-value (* (function __collect_safe (void* void*) int))]
       [set-key (* (function __collect_safe (void* void*) int))]
       [set-value (* (function __collect_safe (void* void*) int))]
       [compare void*]
       [equals void*]
       [next (* (function __collect_safe (void*) int))]
       [prev (* (function __collect_safe (void*) int))]
       [reset (* (function __collect_safe (void*) int))]
       [search (* (function __collect_safe (void*) int))]
       [search-near (* (function __collect_safe (void* void*) int))]
       [insert (* (function __collect_safe (void*) int))]
       [modify void*]
       [update void*]
       [remove (* (function __collect_safe (void*) int))]
       [reserve void*]
       [close (* (function __collect_safe (void*) int))]
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
      (let* ((proc (ftype-ref %session (open-cursor *) session))
             (out (make-double-pointer)))
        (check (proc (ftype->pointer session)
                     uri
                     0
                     config
                     out))
        (pointer->ftype %cursor out)))

    (define (cursor? obj)
      (ftype-pointer? %cursor obj))

    (define (item->bytevector item) ;; XXX: copy!
      (let* ((item (make-ftype-pointer %item item))
             (data (ftype-ref %item (data) item))
             (size (ftype-ref %item (size) item))
             (bytevector (make-bytevector size)))
        (let loop ((index 0))
          (unless (= index size)
            (let ((value (foreign-ref 'unsigned-8 data index)))
              (bytevector-u8-set! bytevector index value)
              (loop (+ index 1)))))
        bytevector))

    (define (bytevector->item bytevector)
      (let* ((size (bytevector-length bytevector))
             (data (bytevector->pointer bytevector))
             (ptr (foreign-alloc (ftype-sizeof %item)))
             (item (make-ftype-pointer %item ptr)))
        (ftype-set! %item (data) item data)
        (ftype-set! %item (size) item size)
        ptr))

    (define (cursor-value-ref cursor)
      (let ((out (make-double-pointer))
            (proc (ftype-ref %cursor (get-value *) cursor)))
        (check (proc (ftype->pointer cursor) out))
        (item->bytevector out)))

    (define (cursor-value-set cursor bytevector)
      (let* ((proc (ftype-ref %cursor (set-value *) cursor))
             (item (bytevector->item bytevector)))
        (proc (ftype->pointer cursor) item)
        item))

    (define (cursor-key-ref cursor)
      (let ((out (make-double-pointer))
            (proc (ftype-ref %cursor (get-key) cursor)))
        (check (proc (ftype->pointer cursor) out))
        (item->bytevector out)))

    (define (cursor-key-set cursor bytevector)
      (let* ((proc (ftype-ref %cursor (set-key *) cursor))
             (item (bytevector->item bytevector)))
        (proc (ftype->pointer cursor) item)
        item))

    (define (cursor-next? cursor)
      (let* ((proc (ftype-ref %cursor (next *) cursor))
             (out (proc (ftype->pointer cursor))))
        (cond
         ((= out 0) #t)
         ((= out -31803) #f)
         (else (raise (cons 'wiredtiger out))))))

    (define (cursor-prev? cursor)
      (let* ((proc (ftype-ref %cursor (prev *) cursor))
             (out (proc (ftype->pointer cursor))))
        (cond
         ((= out 0) #t)
         ((= out -31803) #f)
         (else (raise (cons 'wiredtiger out))))))

    (define (cursor-reset cursor)
      (let ((proc (ftype-ref %cursor (reset *) cursor)))
        (check (proc (ftype->pointer cursor)))))

    (define (%cursor-search cursor)
      (let* ((proc (ftype-ref %cursor (search *) cursor))
             (error (proc (ftype->pointer cursor))))
        (cond
         ((= error 0) #t)
         ((= error -31803) #f) ;; key not found
         (else (raise (cons 'wiredtiger error))))))

    (define (cursor-search? cursor key)
      (call-with-lock key
        (lambda ()
          (let* ((item (cursor-key-set cursor key))
                 (out (%cursor-search cursor)))
              (foreign-free item)
              out))))

    (define (integer->nearness integer)
      (cond
       ((< 0 integer) 'after)
       ((= 0 integer) 'exact)
       ((> 0 integer) 'before)))

    (define (%cursor-search-near cursor)
      (let* ((proc (ftype-ref %cursor (search-near *) cursor))
             (out (foreign-alloc 32))
             (error (proc (ftype->pointer cursor) out))
             (nearness (foreign-ref 'int out 0)))
        (foreign-free out)
        (cond
         ((= error 0)
          (let ((nearness (integer->nearness nearness)))
            nearness))
         ((= error -31803) ;; key not found
          #f)
         (else (raise (cons 'wiredtiger error))))))

    (define (cursor-search-near cursor key)
      (call-with-lock key
        (lambda ()
          (let* ((item (cursor-key-set cursor key))
                 (out (%cursor-search-near cursor)))
            (foreign-free item)
            out))))

    (define (%cursor-insert cursor)
      (let ((proc (ftype-ref %cursor (insert *) cursor)))
        (check (proc (ftype->pointer cursor)))))

    (define (cursor-insert cursor key value)
      (call-with-lock key
        (lambda ()
          (call-with-lock value
            (lambda ()
              (let ((key* (cursor-key-set cursor key))
                    (value* (cursor-value-set cursor value)))
                (%cursor-insert cursor)
                (foreign-free key*)
                (foreign-free value*)))))))

    (define (cursor-remove cursor key)
      (call-with-lock key
        (lambda ()
          (let ((proc (ftype-ref %cursor (remove *) cursor)))
            (let ((item (cursor-key-set cursor key)))
              (check (proc (ftype->pointer cursor)))
              (foreign-free item))))))

    (define (cursor-close cursor)
      (let ((proc (ftype-ref %cursor (close *) cursor)))
        (check (proc (ftype->pointer cursor)))))))
