(library (arew cffi stdlib)

  (export mkdtemp current-processor-count)

  (import (chezscheme))
  (import (arew base))

  (define stdlib (load-shared-object #f))

  (define-syntax-rule (foreign-procedure* return ptr args ...)
    (foreign-procedure ptr (args ...) return))

  (define current-processor-count
    (let ((proc (foreign-procedure* int "get_nprocs")))
      (lambda ()
        (proc))))

  (define mkdtemp
    (let ((proc (foreign-procedure* string "mkdtemp" string)))
      (lambda (string)
        (proc (string-append string "XXXXXX"))))))
