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
(use-modules (statprof))


(log-toggle!)


(define (memory-total)
  (let ((port (open-input-file "/proc/meminfo")))
    (let* ((line (read-line port))
           (value (cadr (string-split line #\:)))
           (value (string-trim-both value))
           (value (car (string-split value #\space))))
      (string->number value))))


;; TODO: Check statistics:
;;
;;   https://source.wiredtiger.com/3.2.0/tune_cache.html
;;

(define (make-config read-only?)
  `((cache . ,(exact (round (* (memory-total) 0.8))))
    ;; (wal . ,(* 1 1024 1024)) ;; TODO: make all this configurable
    (read-only? . ,read-only?)
    (mmap . #f)
    (eviction-trigger . 65)
    (eviction-target . 50)
    (eviction (min . 1)
              (max . ,(min (current-processor-count) 20)))))

(define (make-app* n read-only?)
  (define engine (make-default-engine))
  (define ustore (make-ustore engine '(0)))
  (define nstore* (nstore engine '(1) (iota n)))
  (define config (make-config read-only?))
  (define directory (getcwd))
  (define okvs (engine-open engine directory config))
  (values engine okvs (make-app engine okvs ustore nstore*)))

(match (cdr (program-arguments))
  (`("serve" ,n ,port)
   (call-with-values (lambda () (make-app* (string->number n) #t))
     (lambda (engine okvs app)
       (subcommand-serve app (string->number port)))))
  (`("index" ,n ,filename)
   (statprof (lambda ()
               (call-with-values (lambda () (make-app* (string->number n) #f))
                 (lambda (engine okvs app)
                   (subcommand-index app filename)
                   (engine-close engine okvs))))))
  (else (display "Usage:

  nomunofu index N FILENAME
  nomunofu serve N PORT\n")))
