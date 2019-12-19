(library (arew cffi stdlib)

  (export mkdtemp)

  (import (chezscheme))
  (import (arew base))

  (define stdlib (load-shared-object #f))

  (define-syntax-rule (foreign-procedure* return ptr args ...)
    (foreign-procedure ptr (args ...) return))

  (define mkdtemp
    (let ((proc (foreign-procedure* string "mkdtemp" string)))
      (lambda (string)
        (proc (string-append string "XXXXXX"))))))
