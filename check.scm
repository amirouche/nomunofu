(import (ice-9 ftw))

(define %test-suite '())


(define %current-working-directory (getcwd))

(define (maybe-add-to-test-suite! filename)
  (when (string-suffix? "tests.scm" filename)
    (set! %test-suite (cons filename %test-suite))))

(ftw %current-working-directory
     (lambda (filename statinfo flag)
       (case flag
         ((regular) (maybe-add-to-test-suite! filename) #t)
         ((directory) #t)
         (else #t))))

(define %test-suite (sort %test-suite string<?))

(define (filename->module-name filename)
  (let ((something (substring filename
                              (+ (string-length %current-working-directory) 1)
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


(for-each run %test-suite)

(when %error?
  (exit 1))
