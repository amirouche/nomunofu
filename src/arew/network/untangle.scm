#!chezscheme
(define (arew network untangle)

  (export untangle
          spawn
          socket
          accept
          fd->port)

  (import (except (chezscheme) read write)
          (prefix (arew scheme hash-table) scheme:)
          (arew scheme comparator)
          (arew network epoll)
          (prefix (arew network socket) socket:))

  ;; inspired from https://stackoverflow.com/a/51777980/140837
  ;;
  ;; single thread, single event-loop
  ;;
  (begin

    ;; event loop

    (define %prompt #f)
    (define %queue '())
    (define %wouldblock (list 'wouldblock))

    (define EWOULDBLOCK 11)

    (define (call-with-prompt thunk handler)

      (call-with-values (lambda ()
                          (call/1cc
                           (lambda (k)
                             (set! %prompt k)
                             (thunk))))
        (lambda out
          (cond
           ((and (pair? out) (eq? (car out) %wouldblock))
            (apply handler (cdr out)))
           (else (apply values out))))))

    (define (abort-to-prompt . args)
      (call/1cc
       (lambda (k)
         (let ((prompt %prompt))
           (set! %prompt #f)
           (apply prompt (cons %wouldblock (cons k args)))))))

    (define make-event cons)
    (define event-continuation car)
    (define event-mode cdr)

    (define (exec epoll thunk waiting)
      (call-with-prompt
       thunk
       (lambda (k fd mode)
         ;; if the thunk aborts, then mark fd as waiting output or input
         (case mode
           ((write) (epoll-ctl epoll 1 fd (make-epoll-event-out fd)))
           ((read) (epoll-ctl epoll 1 fd (make-epoll-event-in fd)))
           (else (error 'untangle "mode not supported" mode)))
         (scheme:hash-table-set! waiting fd (make-event k mode)))))

    (define (run-once epoll waiting)
      ;; execute every callback waiting in queue
      (let loop ()
        (unless (null? %queue)
          (let ((head (car %queue))
                (tail (cdr %queue)))
            (set! %queue tail)
            (exec epoll head waiting)
            (loop))))
      ;; wait for ONE event
      (let ((event (make-epoll-event)))
        (let ((count (epoll-wait epoll event 1 -1)))
          (let* ((fd (event-fd event))
                 (event (scheme:hash-table-ref waiting fd)))
            (scheme:hash-table-delete! waiting fd)
            (case (event-mode event)
              ((write) (epoll-ctl epoll 2 fd (make-epoll-event-out fd)))
              ((read) (epoll-ctl epoll 2 fd (make-epoll-event-in fd))))
            (spawn (event-continuation event))))))

    (define (spawn thunk)
      (set! %queue (cons thunk %queue)))

    (define integer-comparator
      (make-comparator integer? = < number-hash))

    (define (untangle thunk)
      (let ((epoll (epoll-create1 0))
            (waiting (scheme:make-hash-table integer-comparator)))
        (spawn thunk)
        (let loop ()
          (unless (and (null? %queue) (scheme:hash-table-empty? waiting))
            (run-once epoll waiting))
            (loop))))

    ;; async sockets

    (define (socket domain type protocol)
      (let ((fd (socket:socket domain type protocol)))
        ;; TODO: logior 2048 with existing flags
        (socket:fcntl! fd 4 2048) ;; SOCK_NONBLOCK
        fd))

    (define (accept fd)
      (let ((out (socket:%accept fd 0 0)))
        (if (= out -1)
            (let ((code (socket:errno)))
              (if (= code EWOULDBLOCK)
                  (begin
                    (abort-to-prompt fd 'read)
                    (accept fd))
                  (error 'accept (socket:strerror code))))
            out)))

    (define (read fd bv start n)
      (lock-object bv)
      (let ((pointer (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 start))))
        (let loop ()
          (let ((out (socket:%recv fd pointer n 0)))
            (if (= out -1)
                (let ((code (socket:errno)))
                  (if (= code EWOULDBLOCK)
                      (begin
                        (abort-to-prompt fd 'read)
                        (loop))
                      (error 'socket (socket:strerror code))))
                (begin
                  (unlock-object bv)
                  out))))))

    (define (write fd bv start n)
      (lock-object bv)
      (let ((pointer (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 start))))
        (let loop ()
          (let ((out (socket:%send fd pointer n 0)))
            (if (= out -1)
                (let ((code (socket:errno)))
                  (if (= code EWOULDBLOCK)
                      (begin
                        (abort-to-prompt fd 'read)
                        (loop))
                      (error 'socket (socket:strerror code))))
                (begin
                  (unlock-object bv)
                  out))))))

    (define (fd->port fd)
      ;; TODO: logior 2048 with existing flags
      (socket:fcntl! fd 4 2048) ;; nonblocking
      (make-custom-binary-input/output-port
       (format "socket ~a" fd)
       (lambda (bv start n) (read fd bv start n))
       (lambda (bv start n) (write fd bv start n))
       #f ;; get-position
       #f ;; set-position
       (lambda ()
         (socket:close fd))))

    ))
