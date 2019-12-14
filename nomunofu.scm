(import (scheme base))
(import (scheme process-context))
(import (ice-9 match))
(import (ice-9 threads))

(import (nomunofu app))
(import (nomunofu log))
(import (nomunofu web))
(import (nomunofu index))
(import (nomunofu okvs engine))
(import (nomunofu okvs wiredtiger))
(import (nomunofu okvs ustore))
(import (nomunofu okvs nstore))


(log-toggle!)


(define (memory-total)
  (let ((port (open-input-file "/proc/meminfo")))
    (let* ((line (read-line port))
           (value (cadr (string-split line #\:)))
           (value (string-trim-both value))
           (value (car (string-split value #\space))))
      (string->number value))))

;; TODO: rework the config to include eviction trigger and eviction
;; target, max number of thread set to the count of cpu core:
;;
;;   https://source.wiredtiger.com/3.2.0/tune_cache.html
;;
;; TODO: Check statistics.

(define (make-config read-only?)
  `((cache . ,(exact (round (* (memory-total) 0.8))))
    ;; (wal . ,(* 1 1024 1024))
    (read-only? . ,read-only?)
    (mmap . #f)
    (eviction-trigger . 65)
    (eviction-target . 50)
    (eviction (min . 1)
              (max . ,(current-processor-count)))))

(define %config
  (if (string=? (cadr (program-arguments)) "serve")
      (make-config #t)
      (make-config #f)))

(define engine (make-default-engine))
(define ustore (make-ustore engine '(0)))
(define nstore (nstore engine '(1) '(graph subject predicate object)))

(define directory (getcwd))
(define okvs (engine-open engine directory %config))
(define app (make-app engine okvs ustore nstore))

(match (cdr (program-arguments))
  (`("serve" ,port)
   (subcommand-serve app (string->number port)))
  (`("index" ,filename)
   (subcommand-index app filename))
  (else (display "Usage:

  nomunofu index FILENAME
  nomunofu serve PORT\n")))

(engine-close engine okvs)
