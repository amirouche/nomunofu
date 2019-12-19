;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
(library (arew srfi srfi-167 wiredtiger)

  (export make-default-engine)

  (import (arew scheme base)
          (arew base)
          (arew scheme case-lambda)
          (arew scheme hash-table)
          (arew scheme generator)
          (only (arew scheme bytevector) bytevector->u8-list)
          (only (arew srfi srfi-13) string-join)
          (only (arew srfi srfi-128) make-default-comparator)
          (arew srfi srfi-173)
          (arew srfi srfi-167 engine)
          (arew srfi srfi-167 pack)
          (prefix (arew data base wiredtiger) wt:)
          (only (chezscheme)
                make-mutex
                with-mutex))


  (define-record-type <okvs>
    (make-okvs cnx isolation mutex sessions hook-begin hook-commit)
    okvs?
    (cnx okvs-cnx)
    (isolation okvs-isolation)
    (mutex okvs-mutex)
    (sessions okvs-sessions okvs-sessions!)
    (hook-begin okvs-hook-on-transaction-begin)
    (hook-commit okvs-hook-on-transaction-commit))

  (define-record-type <session>
    (make-session object cursor)
    session?
    (object session-object)
    (cursor session-cursor))

  (define-record-type <okvs-transaction>
    (make-transaction okvs session state)
    okvs-transaction?
    (okvs transaction-okvs)
    (session transaction-session)
    (state okvs-transaction-state))

  (define (assq-ref alist key)
    (and=> (assq alist key) cdr))

  ;; TODO: redo this with an alist
  (define (connection-config cache create? memory? wal read-only? eviction-trigger eviction-target eviction mmap)
    (let ((out '()))
      (when cache
        ;; cache is size number in bytes, convert to MB
        (let ((cache (exact (round (/ cache 1024)))))
          (set! out (cons (string-append "cache_size=" (number->string cache) "MB") out))))
      (when create?
        (set! out (cons "create" out)))
      (when memory?
        (set! out (cons "in_memory" out)))
      (when wal
        (set! out (cons (string-append "log=(enabled=true,file_max="
                                       (number->string (exact (/ wal 1024)))
                                       "MB)")
                        out)))
      (when read-only?
        (set! out (cons "readonly=true" out)))
      (when eviction-trigger
        (set! out (cons (string-append "eviction_trigger="
                                       (number->string eviction-trigger))
                        out)))
      (when eviction-target
        (set! out (cons (string-append "eviction_target="
                                       (number->string eviction-target))
                        out)))
      (when eviction
        (let ((min (assq-ref eviction 'min))
              (max (assq-ref eviction 'max)))
          (let ((eviction '()))
            (when min
              (set! eviction (cons (string-append "threads_min="
                                                  (number->string min))
                                   eviction)))
            (when max
              (set! eviction (cons (string-append "threads_max="
                                                  (number->string max))
                                   eviction)))
            (set! out (cons (string-append "eviction=("
                                           (string-join eviction ",")
                                           ")")
                            out)))))
      (unless mmap
        (set! out (cons "mmap=false" out)))
      (string-join out ",")))

  (define NO-HOME-ERROR "HOME can not be #f. It should point to an existing directory.")

  (define (session-config okvs)
    (if (okvs-isolation okvs)
        (string-append "isolation=\"" (symbol->string (okvs-isolation okvs)) "\"")
        ""))

  (define (%get-or-create-session okvs)
    ;; XXX: caller has the reponsability to close or put back the session
    (if (null? (okvs-sessions okvs))
        ;; new session
        (let ((session (wt:session-open (okvs-cnx okvs) (session-config okvs))))
          (make-session session (wt:cursor-open session "table:okvs" "")))
        ;; re-use free session
        (let ((session (car (okvs-sessions okvs))))
          (okvs-sessions! okvs (cdr (okvs-sessions okvs)))
          session)))

  (define (get-or-create-session okvs)
    (with-mutex (okvs-mutex okvs) (%get-or-create-session okvs)))

  (define (okvs-config config)
    (let ((cache #f)
          (isolation #f)
          (create? #t)
          (memory? #f)
          (wal #f)
          (read-only? #f)
          (eviction-trigger #f)
          (eviction-target #f)
          (eviction #f)
          (mmap #t) ;; XXX: default configuration
          )
      (let loop ((config config))
        (if (null? config)
            (values cache
                    isolation
                    create?
                    memory?
                    wal
                    read-only?
                    eviction-trigger
                    eviction-target
                    eviction
                    mmap)
            (begin (case (caar config)
                     ((cache) (set! cache (cdar config)))
                     ((isolation) (set! isolation (cdar config)))
                     ((create?) (set! create? (cdar config)))
                     ((memory?) (set! memory? (cdar config)))
                     ((wal) (set! wal (cdar config)))
                     ((read-only?) (set! read-only? (cdar config)))
                     ((eviction-trigger) (set! eviction-trigger (cdar config)))
                     ((eviction-target) (set! eviction-target (cdar config)))
                     ((eviction) (set! eviction (cdar config)))
                     ((mmap) (set! mmap (cdar config)))
                     (else
                      (error 'okvs/wiredtiger "unknown configuration option" (caar config))))
                   (loop (cdr config)))))))

  (define (%okvs home config)
    (call-with-values (lambda () (okvs-config config))
      (lambda (cache isolation create? memory? wal read-only? eviction-trigger eviction-target eviction mmap)
        (unless home
          (error 'okvs/wiredtiger NO-HOME-ERROR home))
        (let ((config
               (connection-config cache
                                  create?
                                  memory?
                                  wal
                                  read-only?
                                  eviction-trigger
                                  eviction-target
                                  eviction
                                  mmap)))
          (let ((cnx (wt:connection-open home config)))
            (when (and create? (not read-only?))
              (let ((session (wt:session-open cnx "")))
                (wt:session-create session "table:okvs")
                (wt:session-close session "")))
            (make-okvs cnx isolation (make-mutex) '() (make-hook 1) (make-hook 1)))))))

  (define okvs-open
    (case-lambda
      ((home config) (%okvs home config))
      ((home) (%okvs home '()))))

  (define okvs-close
    (case-lambda
      ((okvs config) (okvs-close okvs))
      ((okvs) (wt:connection-close (okvs-cnx okvs) ""))))

  (define %default-comparator (make-default-comparator))

  (define (make-default-state)
    (make-hash-table %default-comparator))

  (define (okvs-transaction-begin okvs make-state . config)
    (let ((session (get-or-create-session okvs)))
      (wt:session-transaction-begin (session-object session) "")
      (let ((tx (make-transaction okvs session (make-state))))
        (hook-run (okvs-hook-on-transaction-begin okvs) tx)
        tx)))

  (define (okvs-transaction-commit transaction . config)
    (hook-run (okvs-hook-on-transaction-commit (transaction-okvs transaction)) transaction)
    ;; commit the transaction and put back the session into okvs
    (wt:session-transaction-commit (session-object (transaction-session transaction)) "")
    (wt:session-reset (session-object (transaction-session transaction)))
    (with-mutex (okvs-mutex (transaction-okvs transaction))
      (okvs-sessions! (transaction-okvs transaction)
                      (cons (transaction-session transaction)
                            (okvs-sessions (transaction-okvs transaction))))))

  (define (okvs-transaction-roll-back transaction . config)
    ;; rollback the transaction and put back the session into okvs
    (wt:session-transaction-rollback (session-object (transaction-session transaction)) "")
    (wt:session-reset (session-object (transaction-session transaction)))
    (with-mutex (okvs-mutex (transaction-okvs transaction))
      (okvs-sessions! (transaction-okvs transaction)
                      (cons (transaction-session transaction)
                            (okvs-sessions (transaction-okvs transaction))))))



  (define (%okvs-in-transaction okvs proc failure success make-state config)
    (let ((transaction (okvs-transaction-begin okvs make-state config)))
      (guard (ex
              (else
               (okvs-transaction-roll-back transaction)
               (failure ex)))
        (call-with-values (lambda () (proc transaction))
          (lambda out
            (okvs-transaction-commit transaction)
            (apply success out))))))

  (define okvs-in-transaction
    (case-lambda
      ((okvs proc)
       (%okvs-in-transaction okvs proc raise values make-default-state '()))
      ((okvs proc failure)
       (%okvs-in-transaction okvs proc failure values make-default-state '()))
      ((okvs proc failure success)
       (%okvs-in-transaction okvs proc failure success make-default-state '()))
      ((okvs proc failure success make-state)
       (%okvs-in-transaction okvs proc failure success make-state '()))
      ((okvs proc failure success make-state config)
       (%okvs-in-transaction okvs proc failure success make-state config))))

  (define (okvs-ref/transaction transaction key)
    (let ((cursor (session-cursor (transaction-session transaction))))
      (if (not (wt:cursor-search? cursor key))
          (begin (wt:cursor-reset cursor) #f)
          (let ((value (wt:cursor-value-ref cursor)))
            (wt:cursor-reset cursor)
            value))))

  (define (okvs-ref okvs-or-transaction key)
    (if (okvs-transaction? okvs-or-transaction)
        (okvs-ref/transaction okvs-or-transaction key)
        (okvs-in-transaction okvs-or-transaction
          (lambda (transaction) (okvs-ref/transaction transaction key)))))

  (define (okvs-set!/transaction transaction key value)
    (let ((cursor (session-cursor (transaction-session transaction))))
      (wt:cursor-insert cursor key value)))

  (define (okvs-set! okvs-or-transaction key value)
    (if (okvs-transaction? okvs-or-transaction)
        (okvs-set!/transaction okvs-or-transaction key value)
        (okvs-in-transaction okvs-or-transaction
          (lambda (transaction) (okvs-set!/transaction transaction
                                                       key
                                                       value)))))

  (define (okvs-delete!/transaction transaction key)
    (let ((cursor (session-cursor (transaction-session transaction))))
      (wt:cursor-remove cursor key)))

  (define (okvs-delete! okvs-or-transaction key)
    (if (okvs-transaction? okvs-or-transaction)
        (okvs-delete!/transaction okvs-or-transaction key)
        (okvs-in-transaction okvs-or-transaction
          (lambda (transaction) (okvs-delete!/transaction transaction
                                                          key)))))

  (define (lexicographic-compare bytevector other)
    ;; Return -1 if BYTEVECTOR is before OTHER, 0 if equal
    ;; and otherwise 1
    (let ((end (min (bytevector-length bytevector)
                    (bytevector-length other))))
      (let loop ((index 0))
        (if (zero? (- end index))
            (if (= (bytevector-length bytevector)
                   (bytevector-length other))
                0
                (if (< (bytevector-length bytevector)
                       (bytevector-length other))
                    -1
                    1))
            (let ((delta (- (bytevector-u8-ref bytevector index)
                            (bytevector-u8-ref other index))))
              (if (zero? delta)
                  (loop (+ 1 index))
                  (if (negative? delta)
                      -1
                      1)))))))

  (define (range-config config)
    (let ((limit #f)
          (reverse? #f)
          (offset #f))
      (let loop ((config config))
        (if (null? config)
            (values limit reverse? offset)
            (begin (case (caar config)
                     ((limit) (set! limit (cdar config)))
                     ((reverse?) (set! reverse? (cdar config)))
                     ((offset) (set! offset (cdar config))))
                   (loop (cdr config)))))))

  (define (%range cursor start-key start-include? end-key end-include? limit offset)
    (let ((continue? #t))
      (lambda ()
        (let loop ()
          (if (not continue?)
              (eof-object)
              (let ((key (wt:cursor-key-ref cursor)))
                (case (lexicographic-compare key start-key)
                  ((-1) (error 'okvs/wiredtiger "Oops!"))
                  ((0) (if start-include?
                           (let ((value (wt:cursor-value-ref cursor)))
                             (set! continue? (wt:cursor-next? cursor))
                             (cons key value))
                           (begin
                             (set! continue? (wt:cursor-next? cursor))
                             (loop))))

                  ((1)
                   (case (lexicographic-compare key end-key)
                     ((-1) (let ((value (wt:cursor-value-ref cursor)))
                             (set! continue? (wt:cursor-next? cursor))
                             (cons key value)))
                     ((0) (if end-include?
                              (let ((value (wt:cursor-value-ref cursor)))
                                (set! continue? #f)
                                (cons key value))
                              (eof-object)))
                     ((1)
                      (eof-object)))))))))))

  (define (range transaction cursor start-key start-include? end-key end-include? limit offset)
    (let ((found (wt:cursor-search-near cursor start-key)))
      (cond
       ((not found) eof-object)
       ((eq? found 'before)
        (if (wt:cursor-next? cursor)
            (%range cursor start-key start-include? end-key end-include? limit offset)
            eof-object))
       (else
        (%range cursor start-key start-include? end-key end-include? limit offset)))))

  (define (%range-reverse cursor start-key start-include? end-key end-include? limit offset)
    (let ((continue? #t))
      (lambda ()
        (let loop ()
          (if (not continue?)
              (eof-object)
              (let ((key (wt:cursor-key-ref cursor)))
                (case (lexicographic-compare key end-key)
                  ((1) (error 'okvs/wiredtiger "Oops!"))
                  ((0) (if end-include?
                           (let ((value (wt:cursor-value-ref cursor)))
                             (set! continue? (wt:cursor-prev? cursor))
                             (cons key value))
                           (begin
                             (set! continue? (wt:cursor-prev? cursor))
                             (loop))))
                  ((-1)
                   (case (lexicographic-compare key start-key)
                     ((1) (let ((value (wt:cursor-value-ref cursor)))
                            (set! continue? (wt:cursor-prev? cursor))
                            (cons key value)))
                     ((0) (if end-include?
                              (let ((value (wt:cursor-value-ref cursor)))
                                (set! continue? #f)
                                (cons key value))
                              (eof-object)))
                     ((-1)
                      (eof-object)))))))))))

  (define (range-reverse transaction cursor start-key start-include? end-key end-include? limit offset)
    (let ((found (wt:cursor-search-near cursor end-key)))
      (cond
       ((not found) eof-object)
       ((eq? found 'after)
        (if (wt:cursor-prev? cursor)
            (%range-reverse cursor start-key start-include? end-key end-include? limit offset)
            eof-object))
       (else
        (%range-reverse cursor start-key start-include? end-key end-include? limit offset)))))

  (define (generator-force-cursor-close cursor generator)
    (lambda ()
      (let ((out (generator)))
        (if (eof-object? out)
            (begin
              (wt:cursor-close cursor)
              out)
            out))))

  (define (%okvs-range transaction start-key start-include? end-key end-include? config)
    (let ((cursor (wt:cursor-open (session-object (transaction-session transaction)) "table:okvs" "")))
      (call-with-values (lambda () (range-config config))
        (lambda (limit reverse? offset)
          (if reverse?
              (let ((out (range-reverse transaction
                                        cursor
                                        start-key
                                        start-include?
                                        end-key
                                        end-include?
                                        limit
                                        offset)))
                (when offset
                  (set! out (gdrop out offset)))
                (when limit
                  (set! out (gtake out limit)))
                (generator-force-cursor-close cursor out))
              (let ((out (range transaction
                                cursor
                                start-key
                                start-include?
                                end-key
                                end-include?
                                limit
                                offset)))
                (when offset
                  (set! out (gdrop out offset)))
                (when limit
                  (set! out (gtake out limit)))
                (generator-force-cursor-close cursor out)))))))

  (define okvs-range/transaction
    (case-lambda
      ((transaction start-key start-include? end-key end-include?)
       (%okvs-range transaction start-key start-include? end-key end-include? '()))
      ((transaction start-key start-include? end-key end-include? config)
       (%okvs-range transaction start-key start-include? end-key end-include? config))))

  (define (okvs-range okvs-or-transaction . args)
    (if (okvs-transaction? okvs-or-transaction)
        (apply okvs-range/transaction okvs-or-transaction args)
        (list->generator
         (okvs-in-transaction okvs-or-transaction
           (lambda (transaction) (generator->list
                                  (apply okvs-range/transaction transaction args)))))))

  (define (strinc bv)
    "Return the first bytevector that is not prefix of BYTEVECTOR"
    ;; See https://git.io/fj34F, TODO: OPTIMIZE
    (let ((bytes (reverse (bytevector->u8-list bv))))

      (let loop ((out bytes))
        (when (null? out)
          (error 'okvs "Key must contain at least one byte not equal to #xFF." bytevector))
        (if (= (car out) #xFF)
            (loop (cdr out))
            (set! bytes out)))
      ;; increment first byte, reverse and return the bytevector
      (apply bytevector (reverse (cons (+ 1 (car bytes)) (cdr bytes))))))

  (define (okvs-range-remove! okvs-or-transaction start-key start-include? end-key end-include?)
    (let ((generator
           (okvs-range okvs-or-transaction start-key start-include? end-key end-include?)))
      (let loop ((pair (generator)))
        (unless (eof-object? pair)
          (let ((key (car pair)))
            (okvs-delete! okvs-or-transaction key)
            (loop (generator)))))))

  (define (okvs-prefix-range-remove! okvs-or-transaction prefix)
    (okvs-range-remove! okvs-or-transaction prefix #t (strinc prefix) #f))

  (define (okvs-prefix-range okvs-or-transaction prefix . config)
    (apply okvs-range okvs-or-transaction prefix #t (strinc prefix) #f config))

  (define (make-default-engine)
    (make-engine okvs-open
                 okvs-close
                 okvs-in-transaction
                 okvs-ref
                 okvs-set!
                 okvs-delete!
                 okvs-range-remove!
                 okvs-prefix-range-remove!
                 okvs-range
                 okvs-prefix-range
                 okvs-hook-on-transaction-begin
                 okvs-hook-on-transaction-commit
                 pack
                 unpack)))
