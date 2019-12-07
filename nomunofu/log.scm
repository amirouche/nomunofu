(define-module (nomunofu log))

(import (scheme time))
(import (nomunofu colorizer))
(import (ice-9 threads))


(define mutex (make-mutex))

(define %active? #f)

(define-public (log-toggle!)
  (when %active?
    (log-info "log system is disabled"))
  (set! %active? (not %active?))
  (when %active?
    (log-info "log system is active")))

(define %null-args '(null))

(define (make-log level color)
  (case-lambda
    ((msg args)
     (when %active?
       (with-mutex mutex
         (let ((port (current-error-port)))
           (display (current-jiffy) port)
           (display " - " port)
           (display (colorizer color level) port)
           (display " - " port)
           (display (colorizer color msg) port)
           (display " - " port)
           (display args port)
           (newline port)))))
    ((msg)
     (when %active?
       (with-mutex mutex
         (let ((port (current-error-port)))
           (display (current-jiffy) port)
           (display " - " port)
           (display (colorizer color level) port)
           (display " - " port)
           (display (colorizer color msg) port)
           (newline port)))))))

(define-public log-trace (make-log "trace" 'GREEN))
(define-public log-debug (make-log "debug" 'BLUE))
(define-public log-info (make-log "info" 'WHITE))
(define-public log-warn (make-log "warn" 'YELLOW))
(define-public log-error (make-log "error" 'RED))
(define-public log-panic (make-log "panic" 'MAGENTA))
