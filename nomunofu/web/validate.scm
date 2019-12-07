(define-module (nomunofu web validate))

(import (srfi srfi-1))


(define-public (validate spec alist)
  "Validate ALIST based using SPEC

SPEC must be an association list that maps keys to validation
procedures. Validation procedure must take a single parameter that
will be the associated value in ALIST based on equal?. The validation
procedure must return an error message as string if the value is
invalid"
  (fold (lambda (validator errors)
          (let ((error ((cdr validator) (assoc-ref alist (car validator)))))
            (if (string? error)
                (cons (cons (car validator) error) errors)
                errors)))
        '()
        spec))

(define-public (make-valiador . validators)
  (lambda (value)
    (let loop ((validators validators))
      (unless (null? validators)
        (let ((out ((car validators) value)))
          (if (string=? out)
              out
              (loop (cdr validators))))))))
