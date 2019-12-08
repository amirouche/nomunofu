(define-module (nomunofu web))

(import (scheme base))
(import (scheme list))
(import (scheme assume))

(import (ice-9 match))
(import (web http))
(import (rnrs io ports))

(import (nomunofu app))
(import (nomunofu log))
(import (nomunofu fash))
(import (nomunofu generator))
(import (nomunofu okvs engine))
(import (nomunofu okvs nstore))
(import (nomunofu okvs ustore))
(import (nomunofu web server))
(import (nomunofu web helpers))


(define (%make-query txn ustore item)
  (if (pair? item)
      (nstore-var (cdr item))
      (maybe-object->ulid txn ustore item)))

(define (make-query txn ustore pattern)
  (assume (= (length pattern) 3))
  (map (lambda (item) (%make-query txn ustore item)) pattern))

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

(define (handle/transaction transaction nstore ustore patterns port)
  (let ((items (make-query transaction ustore (car patterns))))
    (if (not (every values items))
        #f
        (generator-for-each
         (lambda (item) (alist->json transaction ustore (fash->alist item) port))
         (let loop ((patterns (cdr patterns))
                    (generator (nstore-select transaction nstore items)))
           (if (null? patterns)
               generator
               (let ((items (make-query transaction ustore (car patterns))))
                 (if (not (every values items))
                     #f
                     (loop (cdr patterns)
                           ((nstore-where transaction nstore items) generator))))))))))

(define (decode bytevector)
  (call-with-input-string
   (utf8->string bytevector)
   (lambda (port)
     (let loop ((item (read port))
                (out '()))
       (if (eof-object? item)
           (reverse out)
           (loop (read port) (cons item out)))))))

(define (handle app body port)
  (let* ((patterns (decode body)))
    (if (null? patterns)
        #f
        (engine-in-transaction (app-engine app) (app-okvs app)
          (lambda (transaction)
            (handle/transaction transaction (app-nstore app) (app-ustore app) patterns port))))))

(define-public (subcommand-serve app port)
  (log-info "web server starting at PORT" port)
  (run-server port
              (lambda (request body port)
                (write-response-line (cons 1 0) 200 "OK" port)
                (put-string port "\r\n")
                (guard (ex (else #f)) ;; TODO: improve error handling
                  (handle app body port))
                (close-port port))))
