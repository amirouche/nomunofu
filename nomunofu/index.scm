(define-module (nomunofu index))


(import (scheme base))
(import (ice-9 rdelim))
(import (nomunofu app))
(import (nomunofu okvs engine))
(import (nomunofu okvs nstore))


(define (add/transaction transaction nstore line)
  (let ((items (string-split line #\space)))
    (pk items)
    (nstore-add! transaction nstore items)))

(define (add app line)
  (engine-in-transaction (app-engine app) (app-okvs app)
    (lambda (transaction)
      (add/transaction transaction (app-nstore app) line))))

(define-public (subcommand-index app filename)
  (let* ((file (call-with-input-file filename read-string))
         (lines (string-split file #\newline)))
    (for-each (lambda (line) (add app line)) lines)))
