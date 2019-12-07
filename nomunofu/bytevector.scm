(define-module (nomunofu bytevector))

(import (rnrs bytevectors))


(re-export bytevector=?)
(re-export bytevector-length)

(define-public (bytevector-append . bvs)
  (let* ((total (let loop ((bvs bvs)
                           (out 0))
                  (if (null? bvs)
                      out
                      (loop (cdr bvs) (+ (bytevector-length (car bvs)) out)))))
         (out (make-bytevector total)))
    (let loop ((bvs bvs)
               (index 0))
      (unless (null? bvs)
        (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
        (loop (cdr bvs) (+ index (bytevector-length (car bvs))))))
    out))
