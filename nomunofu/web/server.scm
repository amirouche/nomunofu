(define-module (nomunofu web server))

(import (ice-9 threads))
(import (web request))


(define %mutex (make-mutex))

(define %worker-count (- (current-processor-count) 1))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk handler-thunk) (thunk))
      (lambda (thunk handler-thunk)
        (let ((handler #f))
          (catch 'interrupt
            (lambda ()
              (dynamic-wind
                (lambda ()
                  (set! handler
                        (sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
                thunk
                (lambda ()
                  (if handler
                      ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                      (sigaction SIGINT (car handler) (cdr handler))
                      ;; restore original C handler.
                      (sigaction SIGINT #f)))))
            (lambda (k . _) (handler-thunk)))))))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

(define* (http-open #:key
                    (host #f)
                    (family AF_INET)
                    (addr (if host
                              (inet-pton family host)
                              INADDR_LOOPBACK))
                    (port 8080)
                    (socket (make-default-socket family addr port)))
  (listen socket 128)
  (sigaction SIGPIPE SIG_IGN)
  socket)

(define (handle client handler)
  (let* ((request (read-request client))
         (body (read-request-body request)))
    (set-port-encoding! client "UTF-8")
    (handler request body client)))

(define (worker mutex socket handler)
  (let loop ()
    (lock-mutex mutex)
    (let ((client (car (accept socket))))
      (unlock-mutex mutex)
      (handle client handler)
      (loop))))

(define (%run-server socket handler)
  (let ((mutex (make-mutex)))
    (let loop ((index %worker-count))
      (unless (zero? index)
        (call-with-new-thread
         (lambda ()
           (worker mutex socket handler)))
        (loop (- index 1))))))

(define-public (run-server port handler)
  (let ((socket (http-open #:port port)))
    (call-with-sigint
     (lambda ()
       (%run-server socket handler)
       (let loop ()
         (sleep 3600)
         (loop)))
     (lambda ()
       (close-port socket)))))
