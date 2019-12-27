(library (arew base)

  (export pk define-syntax-rule const and=> compose)

  (import (scheme base)
          (scheme write)
          (only (chezscheme)
                syntax-case
                datum->syntax
                syntax->datum
                syntax))

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
           ((keyword args ...) body))))))

  (define-syntax-rule (and=> v proc)
    (let ((v* v)
          (proc* proc))
      (if v* (proc* v*) #f)))

  (define-syntax-rule (const value)
    (lambda args value))

  (define (compose . rest)
    (let* ((reversed (reverse rest))
           (first (car reversed))
           (rest (cdr reversed)))
      (lambda (value)
        (let loop ((out (first value))
                   (rest rest))
          (if (null? rest)
              out
              (loop ((car rest) out) (cdr rest))))))))
