(define-module (nomunofu index))


(import (scheme base))
(import (scheme list))
(import (nomunofu app))
(import (nomunofu log))
(import (nomunofu okvs engine))
(import (nomunofu okvs nstore))
(import (nomunofu okvs ustore))


(define (string->tuple transaction ustore string)
  (let loop ((chars (string->list string))
             (out '()))
    (if (or (char=? (car chars) #\.)
            (and (char=? (car chars) #\space)
                 (char=? (cadr chars) #\.)))
        (reverse out)
        (call-with-values (lambda () (turtle-parse-item chars))
          (lambda (item rest)
            (if item
                (let ((item (object->ulid transaction ustore item)))
                  (loop rest (cons item out)))
                #f))))))

(define (add/transaction transaction nstore ustore line)
  (let ((tuple (string->tuple transaction ustore line)))
    (when tuple
      (nstore-add! transaction
                   nstore
                   tuple))))

(define (add app line)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (add/transaction transaction (app-nstore app) (app-ustore app) line))))

(define (decode chars)
  (let loop ((chars chars)
             (out '()))
    (if (null? chars)
        (list->string (reverse out))
        (if (and (char=? (car chars) #\\)
                 (not (null? (cdr chars)))
                 (char=? (cadr chars) #\u))
            (loop (drop chars 6)
                  (cons (integer->char
                         (string->number
                          (list->string (drop (take chars 6) 2)) 16))
                        out))
            (loop (cdr chars) (cons (car chars) out))))))


(define (parse-escaped-string chars)
  (let loop ((chars chars)
             (previous #\space)
             (out '()))
    (if (char=? (car chars) #\")
        (values (reverse out) chars)
        (if (and (char=? (car chars) #\\)
                 (char=? (cadr chars) #\"))
            (loop (cddr chars) #\" (cons #\" out))
            (loop (cdr chars) (car chars) (cons (car chars) out))))))

(define (turtle-parse-string chars)
  (call-with-values (lambda () (parse-escaped-string chars))
    (lambda (item rest)
      (cond
       ((char=? (cadr rest) #\space)
        (values (decode item) (cdr rest)))
       ((char=? (cadr rest) #\^)
        (call-with-values (lambda () (span (compose not char-whitespace?) (cdddr rest)))
          (lambda (type rest)
            (let ((type (list->string type))
                  (item (list->string item)))
              (cond
               ((string=? type "<http://www.w3.org/2001/XMLSchema#dateTime>")
                (values (turtle-parse-datetime item) rest))
               ((string=? type "<http://www.w3.org/2001/XMLSchema#integer>")
                (values (string->number item) rest))
               ((string=? type "<http://www.w3.org/2001/XMLSchema#decimal>")
                (values (string->number item) rest))
               ((string=? type "<http://www.opengis.net/ont/geosparql#wktLiteral>")
                ;; TODO: support Point(5.4726111111111 49.497111111111)
                (values item rest))
               ((string=? type "<http://www.w3.org/2001/XMLSchema#double>")
                (values (string->number item) rest))
               ((string=? type "<http://www.w3.org/1998/Math/MathML>")
                (values item rest))
               (else (raise (cons 'unknown-type type))))))))
       ((char=? (cadr rest) #\@)
        (call-with-values (lambda () (span (compose not char-whitespace?) (cdr rest)))
          (lambda (lang rest)
            (values (list (decode item) (list->string (cdr lang))) (cdr rest)))))
       (else
        (log-warn "boggus STRING; ignoring the whole tuple!" (list->string chars))
        (values #f #f))))))

(define (turtle-parse-iri chars)
  (call-with-values (lambda () (span (lambda (x) (not (char=? x #\>))) chars))
    (lambda (item rest)
      (values (string->symbol (list->string item)) (cdr rest)))))

(define (turtle-parse-number chars)
  (call-with-values (lambda () (span char-numeric? chars))
    (lambda (item rest)
      (values (string->number (list->string item)) (cdr rest)))))

(define (turtle-parse-gensym chars)
  (call-with-values (lambda () (span (compose not char-whitespace?) chars))
    (lambda (item rest)
      (values (list->string item) (cdr rest)))))

(define (turtle-parse-item chars)
  (let ((chars (find-tail (compose not char-whitespace?) chars)))
    (cond
     ((char=? (car chars) #\") (turtle-parse-string (cdr chars)))
     ((char=? (car chars) #\<) (turtle-parse-iri (cdr chars)))
     ((char-numeric? (car chars)) (turtle-parse-number chars))
     ((char=? (car chars) #\_) (turtle-parse-gensym chars))
     (else (raise (cons 'not-implemented (car chars)))))))

(define (turtle-parse-datetime object)
  (guard (ex (else object)) ;; TODO: sometime there is weird values
    ;; like: -34000-01-01T00:00:00Z
    (car (mktime (car (strptime "%FT%T" object)) "UTC"))))

(define-public (subcommand-index app filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((line (read-line port))
                 (index 0))
        (unless (eof-object? line)
          (when (= (modulo index 10000) 0)
            (display index) (newline))
          (add app line)
          (loop (read-line port)
                (+ index 1)))))))
