(import (srfi :64)
        (scheme base)
        (scheme process-context))


(test-begin "scheme-base")

(test-equal "1" (+ 1 2 3 4) 10)

(test-equal "2" (+ 1) 1)

(test-equal "3" (* 1 2 3 4) 24)

(test-equal "4" (* 1) 1)

(test-equal "5" (- 1) -1)

(test-equal "6" (- 1 1) 0)

(test-equal "7" (/ 1 2) 1/2)

;; propertly guard the exception 'numerical-overflow
(test-equal "6" (guard (ex (else #t)) (/ 1 0)) #t)

(test-equal "7" (/ 1 2 3 4 5) 1/120)

(test-equal "8" (< 1 2 3 4 5) #t)

(test-equal "8.1" (< 1 2 3 4 5 5) #f)

(test-equal "9" (< 10 5) #f)

(test-equal "10" (<= 1 2 3 4 5 5) #t)

(test-equal "10" (<= 1 2 3 4 5) #t)

(test-equal "11" (<= 10 5) #f)

(test-equal "12" (= 10 5) #f)

(test-equal "13" (= 10 10 10 10) #t)

(test-equal "14" (> 10111 1011 11 1) #t)

(test-equal "15" (> 10 100) #f)

(test-equal "15" (>= 10111 10111 1011 11 1) #t)

(test-equal "16" (>= 10 100) #f)

(test-equal "17" (abs -10) 10)

(test-equal "18" (abs 10) 10)

(test-equal "19" (and 1 2 3) 3)

(test-equal "20" (and 1 #f 3) #f)

(test-equal "21" (append '(1 2 3)) '(1 2 3))

(test-equal "22" (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(test-equal "23" (apply + '(1 2 3)) 6)

(test-equal "24" (apply + 1 2 3 '(4 5 6)) 21)

(test-equal "25" (assoc 1 '((1 . 2) (3 . 4))) '(1 . 2))

(test-equal "26" (assoc 42 '((1 . 2) (3 . 4))) #f)

(test-equal "27" (assq 1 '((1 . 2) (3 . 4))) '(1 . 2))

(test-equal "28" (assq 42 '((1 . 2) (3 . 4))) #f)

(test-equal "29" (assv 1 '((1 . 2) (3 . 4))) '(1 . 2))

(test-equal "30" (assv 42 '((1 . 2) (3 . 4))) #f)

(test-equal "31" (boolean=? #t #f) #f)

(test-equal "32" (boolean=? #t #t) #t)

;; TOOD: FIXME
;; (test-equal "33" (boolean=? #t 23) #f)

(test-equal "34" (boolean? #t) #t)

(test-equal "35" (boolean? 23) #f)

(test-equal "36" (bytevector 0 1 2 3 4 5) #vu8(0 1 2 3 4 5))

(test-equal "37" (bytevector-append #vu8(0 1 2) #vu8(3 4 5)) #vu8(0 1 2 3 4 5))

(test-equal "38" (bytevector-copy #vu8(0 1 2)) #vu8(0 1 2))

(test-equal "39" (bytevector-copy #vu8(0 1 2) 1) #vu8(1 2))

(test-equal "40" (bytevector-copy #vu8(0 1 2) 1 2) #vu8(1))

(test-equal "41" (let ((bv (make-bytevector 4 0)))
                   (bytevector-copy! bv 1 #vu8(0 1 2 3 4) 1 3)
                   bv)
            #vu8(0 1 2 0))

(test-equal "42" (bytevector-length #vu8(0 1 2)) 3)

(test-equal "43" (bytevector-u8-ref #vu8(0 1 2) 2) 2)

(test-equal "44" (let ((bv (make-bytevector 4 0)))
                   (bytevector-u8-set! bv 1 42)
                   bv)
            #vu8(0 42 0 0))

(test-equal "45" (bytevector? #vu8(0 1 2)) #t)

(test-equal "46" (bytevector? 123456) #f)


(test-assert "error"
  (guard (ex ((string=? (error-object-message ex) "nok")))
    (error 'tests-raise "nok" 'climate-change)))

(test-assert "error"
  (guard (ex ((string=? (error-object-message ex) "nok")))
    (error "nok" 'climate-change)))

(test-end)


(define xpass (test-runner-xpass-count (test-runner-current)))
(define fail (test-runner-fail-count (test-runner-current)))
(if (and (= xpass 0) (= fail 0))
    (exit 0)
    (exit 1))
