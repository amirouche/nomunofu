(library (arew base)

  (export pk define-syntax-rule)
  (import (arew scheme base)
          (arew scheme write))

  (define (pk . args)
    (write args (current-error-port))
    (display #\newline (current-error-port))
    (flush-output-port (current-error-port))
    (car (reverse args)))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body)))))))
