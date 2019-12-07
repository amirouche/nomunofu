(define-module (nomunofu web style))

(import (ice-9 match))


(define (->string value)
  (match value
    (('hsla h s l a) (format #f "hsla(~a, ~a%, ~a%, ~a)" h s l a))
    ((? string? string) string)
    ((? symbol? symbol) (symbol->string symbol))))


(define (make-value value)
  (match value
    (('hsla h s l a) (format #f "hsla(~a, ~a%, ~a%, ~a)" h s l a))
    ((? pair? lst) (string-join (map ->string lst) " "))
    (_ (->string value))))

(define-public (make-style alist)
  "Create style from an ALIST"
  (let loop ((alist alist)
             (out ""))
    (if (null? alist)
        (string-append out ";")
        (loop (cdr alist)
              (string-append out
                             "; "
                             (symbol->string (caar alist))
                             ": "
                             (make-value (cdar alist)))))))

(define-public (make-class alist)
  "Create html class attribute with an ALIST"
  (let loop ((alist alist)
             (out ""))
    (if (null? alist)
        out
        (if (cdar alist)
            (loop (cdr alist) (string-append (caar alist) " " out))
            (loop (cdr alist) out)))))
