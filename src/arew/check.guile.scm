(define-module (arew check))

(import (srfi srfi-1))
(import (ice-9 ftw))


(define-syntax-rule (test expected actual)
  (lambda ()
    (let ((expected* expected)
          (actual* actual))
      (if (equal? expected* actual*)
          (list #t)
          (list #f expected* actual*)))))

(export test)

(define (failure? lst)
  (not (car lst)))

(define (success? lst)
  (car lst))

(define (failure-expected lst)
  (list-ref lst 1))

(define (failure-actual lst)
  (list-ref lst 2))

(define (filename->module-name filename)
  (let ((something (substring filename
                              4
                              (- (string-length filename) 4))))
    (map string->symbol (string-split something #\/))))

(define %error? #f)

(define (run-one name variable)
  (let ((thunk (variable-ref variable)))
    (display "** ")
    (display name)
    (newline)
    (let ((out (thunk)))
      (unless (car out)
        (set! %error? #t)
        (display "*** ")
        (display (cdr out))
        (newline)))))

(define (run filename)
  (let ((module-name (filename->module-name filename)))
    (display "* ")
    (display module-name)
    (display " @ ")
    (display filename)
    (newline)
    (let ((module (resolve-interface module-name))
          (exports '()))
      (module-for-each
       (lambda (name variable)
         (set! exports (cons (cons name variable) exports)))
       module)
      (set! exports
            (sort exports (lambda (a b) (string<? (symbol->string (car a))
                                                  (symbol->string (car b))))))
      (for-each (lambda (x) (run-one (car x) (cdr x))) exports))))

(define-public (run-check)
  (map-in-order run (cdr (command-line)))
  (if %error?
      (exit 1)
      (exit 0)))
