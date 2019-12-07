(define-module (nomunofu okvs pack tests))

(import (nomunofu testing))
(import (nomunofu okvs pack))


(define-public test-000
  ;; flat list
  (let ((expected (list %null
                        #t
                        #f
                        0
                        #vu8(42 101 255)
                        "hello world"
                        'symbol
                        42
                        (expt 2 64)
                        -42
                        (- (expt 2 64)))))
    (test expected (unpack (apply pack expected)))))

(define-public test-001
  ;; nested list at the beginning
  (let ((expected (list (list 1 2 3) 4 5)))
    (test expected (unpack (apply pack expected)))))

(define-public test-002
  ;; nested list in the middle
  (let ((expected (list 1 (list 2 3) 4 5)))
    (test expected (unpack (apply pack expected)))))

(define-public test-003
  ;; nested list in the end
  (let ((expected (list 1 2 (list 3 4))))
    (test expected (unpack (apply pack expected)))))
