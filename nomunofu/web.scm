(define-module (nomunofu web))

(import (scheme base))
(import (scheme list))
(import (scheme assume))

(import (ice-9 match))
(import (web http))
(import (web request))
(import (web uri))
(import (rnrs io ports))

(import (nomunofu app))
(import (nomunofu log))
(import (nomunofu fash))
(import (nomunofu json))
(import (nomunofu generator))
(import (nomunofu okvs engine))
(import (nomunofu okvs nstore))
(import (nomunofu okvs ustore))
(import (nomunofu web server))
(import (nomunofu web helpers))
(import (nomunofu web decode))


(define (%make-pattern txn ustore item)
  (if (pair? item)
      (nstore-var (string->symbol (car item)))
      (maybe-object->ulid txn ustore item)))

(define (make-pattern txn ustore pattern)
  (assume (= (length pattern) 3))
  (map (lambda (item) (%make-pattern txn ustore item)) pattern))

(define (alist->json txn ustore alist port)
  (put-char port #\{)
  (let loop ((alist alist))
    (if (pair? (cdr alist))
        (let* ((head (car alist))
               (object (ulid->object txn ustore (cdr head))))
          (put-char port #\")
          (put-string port (symbol->string (car head)))
          (put-char port #\")
          (put-char port #\:)
          (write object port)
          (put-char port #\,)
          (loop (cdr alist)))
        (let* ((head (car alist))
               (object (ulid->object txn ustore (cdr head))))
          (put-char port #\")
          (put-string port (symbol->string (car head)))
          (put-char port #\")
          (put-char port #\:)
          (write object port))))
  (put-char port #\})
  (put-char port #\newline))

(define (invalid? pattern)
  (every nstore-var? pattern))

(define (limit+offset options)
  (let* ((limit (and options (and=> (assq 'limit options)
                                    (compose string->number cadr))))
         (offset (and options (and=> (assq 'offset options)
                                     (compose string->number cadr)))))
    (if limit
        (set! limit (min 1000 limit))
        (set! limit 1000))
    (values limit offset)))

(define (query/transaction transaction nstore ustore patterns options port)
  (let ((pattern (make-pattern transaction ustore (car patterns))))
    (if (invalid? pattern)
        (begin
          (start-response port 400 "Bad request")
          (write "A query can not start with a pattern that is only variables" port))
        (begin
          (start-response port 200 "OK")
          (call-with-values (lambda () (limit+offset options))
            (lambda (limit offset)
              (generator-for-each
               (lambda (item) (alist->json transaction ustore (fash->alist item) port))
               (let loop ((patterns (cdr patterns))
                          (generator (nstore-select transaction nstore pattern)))
                 (if (null? patterns)
                     (begin
                       (when offset
                         (set! generator (gdrop generator offset)))
                       (when limit
                         (set! generator (gtake generator limit)))
                       generator)
                     (let ((pattern (make-pattern transaction ustore (car patterns))))
                       (loop (cdr patterns)
                             ((nstore-where transaction nstore pattern) generator))))))))))))

(define (parse bytevector)
  (json->scheme (utf8->string bytevector)))

(define (aggregate symbol transaction nstore ustore patterns seed proc)
  (let ((pattern (make-pattern transaction ustore (car patterns))))
    (if (invalid? pattern)
        #f
        (generator-fold
         (lambda (item out) (and out
                                 (and=> (fash-ref item symbol)
                                        (lambda (value) (proc value out)))))
         seed
         (let loop ((patterns (cdr patterns))
                    (generator (nstore-select transaction nstore pattern)))
           (if (null? patterns)
               generator
               (let ((pattern (make-pattern transaction ustore (car patterns))))
                 (loop (cdr patterns)
                       ((nstore-where transaction nstore pattern) generator)))))))))

(define (sum app name patterns)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (aggregate (string->symbol name)
                 transaction
                 (app-nstore app)
                 (app-ustore app)
                 patterns
                 0
                 (lambda (value out)
                   (+ (ulid->object transaction
                                    (app-ustore app)
                                    value)
                      out))))))

(define (count app name patterns)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (aggregate (string->symbol name)
                 transaction
                 (app-nstore app)
                 (app-ustore app)
                 patterns
                 0
                 (lambda (value out) (+ out 1))))))


(define (average app name patterns)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (let ((count 0))
        (and=>
         (aggregate (string->symbol name)
                     transaction
                     (app-nstore app)
                     (app-ustore app)
                     patterns
                     0
                     (lambda (value out)
                       (set! count (+ count 1))
                       (+ (ulid->object transaction
                                        (app-ustore app)
                                        value)
                          out)))
         (lambda (sum)
           (inexact (/ sum count))))))))

(define (handle app body options port)
  (let ((query (parse body)))
    (match (car query)
      ("query"
       (if (null? (cdr query))
           (begin
             (start-response port 400 "Bad Request")
             (write "No pattern found" port))
           (engine-in-transaction (app-engine app) (app-okvs app)
             (lambda (transaction)
               (query/transaction transaction
                                   (app-nstore app)
                                   (app-ustore app)
                                   (cdr query)
                                   options
                                   port)))))
      ("sum"
       (let ((out (sum app (cadr query) (cddr query))))
         (if out
             (begin
               (start-response port 200 "OK")
               (write out port))
             (begin
               (start-response port 400 "Bad Request")
               (write "nomunofu!" port)))))
      ("average"
       (let ((out (average app (cadr query) (cddr query))))
         (if out
             (begin
               (start-response port 200 "OK")
               (write out port))
             (begin
               (start-response port 400 "Bad Request")
               (write "nomunofu!" port)))))
      ("count"
       (let ((out (count app (cadr query) (cddr query))))
         (if out
             (begin
               (start-response port 200 "OK")
               (write out port))
             (begin
               (start-response port 400 "Bad Request")
               (write "nomunofu!" port)))))
      (else
       (start-response port 400 "Bad Request")
       (write "Unkown action" port)))))

(define (on-error ex)
  (log-panic "error while handling request" ex))

(define (start-response port code reason)
  (write-response-line (cons 1 0) code reason port)
  (put-string port "\r\n"))

(define-public (subcommand-serve app port)
  (log-info "web server starting at PORT" port)
  (run-server port
              (lambda (request body port)
                ;; (guard (ex (else (on-error ex) #f))
                (begin
                  (let ((options (and=> (uri-query (request-uri request)) decode)))
                    (handle app body options port)))
                (close-port port))))
