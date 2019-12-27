(library (arew linux)

  (export memory-total)

  (import (scheme base)
          (only (chezscheme) open-input-file)
          (only (arew srfi srfi-13) string-trim-both))


  (define (string-split string char)
    (let loop ((index 0)
               (item '())
               (out '()))
      (if (= (string-length string) index)
          (reverse (cons (list->string (reverse item)) out))
          (if (char=? (string-ref string index) char)
              (loop (+ index 1) '() (cons (list->string (reverse item)) out))
              (loop (+ index 1) (cons (string-ref string index) item) out)))))

  (define (memory-total)
    (let* ((port (open-input-file "/proc/meminfo"))
           (line (read-line port))
           (value (cadr (string-split line #\:)))
           (value (string-trim-both value))
           (value (car (string-split value #\space)))
           (out (string->number value)))
      (close-port port)
      out)))
