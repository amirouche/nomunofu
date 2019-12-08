(define-module (nomunofu index))


(import (scheme base))
(import (scheme list))
(import (nomunofu app))
(import (nomunofu log))
(import (nomunofu okvs engine))
(import (nomunofu okvs nstore))


(define (add/transaction transaction nstore items)
  (nstore-add! transaction nstore items))

(define (add app items)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (add/transaction transaction (app-nstore app) items))))

(define (decode chars)
  (let loop ((chars chars)
             (out '()))
    (if (null? chars)
        (list->string (reverse out))
        (if (char=? (car chars) #\\)
            (loop (drop chars 6)
                  (cons (integer->char
                         (string->number
                          (list->string (drop (take chars 6) 2)) 16))
                        out))
            (loop (cdr chars) (cons (car chars) out))))))

(define (turtle-parse-string chars)
  (call-with-values (lambda () (span (lambda (x) (not (char=? x #\"))) chars))
    (lambda (item rest)
      (values (decode item) (cdr rest)))))

(define (turtle-parse-iri chars)
  (call-with-values (lambda () (span (lambda (x) (not (char=? x #\>))) chars))
    (lambda (item rest)
      (values (list->string item) (cdr rest)))))

(define (turtle-parse-number chars)
  (call-with-values (lambda () (span char-numeric? chars))
    (lambda (item rest)
      (values (string->number (list->string item)) (cdr rest)))))

(define (turtle-parse-item chars)
  (let ((chars (find-tail (compose not char-whitespace?) chars)))
    (cond
     ((char=? (car chars) #\") (turtle-parse-string (cdr chars)))
     ((char=? (car chars) #\<) (turtle-parse-iri (cdr chars)))
     ((char-numeric? (car chars)) (turtle-parse-number chars))
     ;; TODO: use gensym?
     ((char=? (car chars) #\_) (values #f '()))
     (else (raise (cons 'not-implemented (car chars)))))))

(define (turtle-parse-datetime object)
  (guard (ex (else #f)) ;; TODO: sometime there is weird values like: -34000-01-01T00:00:00Z
    (car (mktime (car (strptime "%FT%T" object)) "UTC"))))

(define (turtle->scheme string)
  (let ((chars (string->list string)))
    (call-with-values (lambda () (turtle-parse-item chars))
      (lambda (subject rest)
        (call-with-values (lambda () (turtle-parse-item rest))
          (lambda (predicate rest)
            (call-with-values (lambda () (turtle-parse-item rest))
              (lambda (object rest)
                (cond
                 ((not object) #f)
                 ((char=? (car rest) #\^)
                  (let ((type (list->string (take-while (compose not char-whitespace?) rest))))
                    (cond
                     ((string=? type "^^<http://www.w3.org/2001/XMLSchema#dateTime>")
                      (list subject predicate (turtle-parse-datetime object)))
                     ((string=? type "^^<http://www.w3.org/2001/XMLSchema#integer>")
                      (list subject predicate (string->number object)))
                     ((string=? type "^^<http://www.w3.org/2001/XMLSchema#decimal>")
                      ;; TODO: add support in okvs/pack
                      (let ((out (string->number object)))
                        (if (exact? out)
                            (list subject predicate out)
                            #f)))
                     ((string=? type "^^<http://www.opengis.net/ont/geosparql#wktLiteral>")
                      ;; TODO: support Point(5.4726111111111 49.497111111111)
                      #f)
                     ((string=? type "^^<http://www.w3.org/2001/XMLSchema#double>")
                      ;; TODO: add support in okvs/pack
                      (let ((out (string->number object)))
                        (if (exact? out)
                            (list subject predicate out)
                            #f)))
                     (else (raise type)))))
                 ((and subject predicate object) (list subject predicate object))
                 (else (raise (list subject predicate object))))))))))))

(define-public (subcommand-index app filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((line (read-line port)))
        (unless (eof-object? line)
          (let ((items (turtle->scheme line)))
            (when items
              (add app items))
            (loop (read-line port))))))))
