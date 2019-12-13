(import (nomunofu okvs ulid))
(import (gcrypt random))


(define (current-milliseconds)
  (let ((seconds+microseconds (gettimeofday)))
    (+ (* (car seconds+microseconds) (expt 10 3))
       (round (/ (cdr seconds+microseconds) (expt 10 3))))))

(define (call-with-delta thunk)
  (let ((start (current-milliseconds)))
    (thunk)
    (- (current-milliseconds) start)))

(define (do-times count thunk)
  (let loop ((count count))
    (unless (zero? count)
      (thunk)
      (loop (- count 1)))))


(call-with-values (lambda () (call-with-delta (lambda () (do-times 10000 ulid))))
  (lambda (delta)
    (pk 'x-ulid-in-milliseconds delta)))

(define (random)
  (random-token 16))

(call-with-values (lambda () (call-with-delta (lambda () (do-times 10000 random))))
  (lambda (delta)
    (pk 'x-random-in-milliseconds delta)))
