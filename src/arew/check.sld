(define-library (arew check)

  (export run-check test)

  (import (scheme base))
  (import (scheme write))

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define-syntax-rule (test expected actual)
      (lambda ()
        (let ((expected* expected)
              (actual* actual))
          (if (equal? expected* actual*)
              (list #t)
              (list #f expected* actual*)))))

    (define (failure? lst)
      (not (car lst)))

    (define (success? lst)
      (car lst))

    (define (failure-expected lst)
      (list-ref lst 1))

    (define (failure-actual lst)
      (list-ref lst 2))


    (define (run-check)
      (display 42) (newline))))
