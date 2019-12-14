(define-module (nomunofu client))

(import (scheme assume))
(import (scheme base))
(import (web client))
(import (web response))


(define-record-type <nomunofu>
  (make-nomunofu url)
  nomunofu?
  (url nomunofu-url))


(export make-nomunofu)
(export nomunofu?)


(define (string->scheme string)
  (call-with-input-string string read))

(define (make-options limit offset)
  (let ((out "?"))
    (when limit
      (set! out (string-append out "limit=" (number->string limit))))
    (when offset
      (set! out (string-append out (if limit "&" "") "offset=" (number->string offset))))
    out))

(define-public (nomunofu-query nomunofu patterns limit offset)
  (assume (and (list? patterns) (list? (car patterns))))
  (let ((scheme (call-with-output-string
                 (lambda (port)
                   (write `(query ,@patterns) port)))))
    (call-with-values (lambda () (http-get (string-append (nomunofu-url nomunofu)
                                                          "/"
                                                          (make-options limit offset))
                                           #:version '(1 . 0)
                                           #:body scheme
                                           #:decode-body? #f
                                           #:streaming? #t))
      (lambda (response port)
        (set-port-encoding! port "UTF-8")
        (unless (= (response-code response) 200)
          (raise (cons 'nomunofu-client (read-line port))))
        (let loop ((line (pk (read-line port)))
                   (out '()))
          (if (eof-object? line)
              out
              (loop (read-line port) (cons (string->scheme line) out))))))))
