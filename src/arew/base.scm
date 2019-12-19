(library (arew base)

  (export pk define-syntax-rule -> const and=>)

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
           ((keyword args ...) body))))))

  (define-syntax-rule (and=> v proc)
    (let ((v* v)
          (proc* proc))
      (if v* (proc* v*) #f)))


  (define-syntax-rule (const value)
    (lambda args value))

  (define-syntax compose
    (lambda (x)
      (syntax-case x ()
        [(k exp0 . exps)
         (let* ([reversed (cons (syntax->datum #'exp0)
                                (syntax->datum #'exps))]
                [out (let loop ([first (car reversed)]
                                [rest (cdr reversed)])
                       (if (null? rest)
                           first
                           (let ([func (car first)]
                                 [args (cdr first)])
                             (append `(,func ,@args)
                                     (list (loop (car rest) (cdr rest)))))))])
           (datum->syntax #'k out))])))

  (define-syntax ->
    (lambda (x)
      (syntax-case x ()
        [(k exp0 . exps)
         (let* ([reversed (reverse (cons (syntax->datum #'exp0)
                                         (syntax->datum #'exps)))]
                [out (let loop ([first (car reversed)]
                                [rest (cdr reversed)])
                       (if (null? rest)
                           first
                           (let ([func (car first)]
                                 [args (cdr first)])
                             (append `(,func ,@args)
                                     (list (loop (car rest) (cdr rest)))))))])
           (datum->syntax #'k out))]))))
