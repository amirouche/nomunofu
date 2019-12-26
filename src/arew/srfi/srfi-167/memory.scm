;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
(library (arew srfi srfi-167 memory)

  (export okvs-open
          okvs?
          okvs-close
          make-default-state
          okvs-transaction?
          okvs-transaction-state
          okvs-in-transaction
          okvs-ref
          okvs-set!
          okvs-delete!
          okvs-range-remove!
          okvs-range
          okvs-prefix-range
          okvs-hook-on-transaction-begin
          okvs-hook-on-transaction-commit
          make-default-engine)

  (import (only (chezscheme) void)
          (arew base)
          (arew scheme base)
          (arew scheme case-lambda)
          (arew scheme list)
          (arew scheme hash-table)
          (only (arew scheme bytevector)
                bytevector=?
                u8-list->bytevector
                bytevector->u8-list)
          (arew scheme comparator)
          (arew scheme generator)
          (arew scheme mapping)
          (arew srfi srfi-173)
          (arew srfi srfi-167 pack)
          (arew srfi srfi-167 engine))

  ;;
  ;; This a memory based okvs implementation backed by the r7rs
  ;; library (scheme mapping). Roll-back operation is supported.
  ;;

  (define-record-type <okvs>
    (make-okvs store hook-on-transaction-begin hook-on-transaction-commit)
    okvs?
    (store okvs-store okvs-store!)
    (hook-on-transaction-begin okvs-hook-on-transaction-begin)
    (hook-on-transaction-commit okvs-hook-on-transaction-commit))

  (define (lexicographic-compare bytevector other)
    ;; Return -1 if BYTEVECTOR is before OTHER, 0 if equal
    ;; and otherwise 1
    (let ((end (min (bytevector-length bytevector)
                    (bytevector-length other))))
      (let loop ((index 0))
        (if (zero? (- end index))
            (if (= (bytevector-length bytevector)
                   (bytevector-length other))
                0
                (if (< (bytevector-length bytevector)
                       (bytevector-length other))
                    -1
                    1))
            (let ((delta (- (bytevector-u8-ref bytevector index)
                            (bytevector-u8-ref other index))))
              (if (zero? delta)
                  (loop (+ 1 index))
                  (if (negative? delta)
                      -1
                      1)))))))

  (define (lexicographic<? bytevector other)
    (negative? (lexicographic-compare bytevector other)))

  (define vector-hash
    (comparator-hash-function
     (make-vector-comparator (make-default-comparator)
                             bytevector?
                             bytevector-length
                             bytevector-u8-ref)))

  (define (make-lexicographic-comparator)
    (make-comparator bytevector? bytevector=? lexicographic<? vector-hash))

  (define (okvs-open home . args)
    (make-okvs (mapping (make-lexicographic-comparator))
               (make-hook 1)
               (make-hook 1)))

  (define (okvs-close . args)
    (void))

  (define-record-type <okvs-transaction>
    (make-okvs-transaction database store state)
    okvs-transaction?
    (database okvs-transaction-database okvs-transaction-database!)
    (store okvs-transaction-store okvs-transaction-store!)
    (state okvs-transaction-state))

  (define (okvs-transaction-begin database make-state . args)
    (let ((transaction (make-okvs-transaction database
                                              (okvs-store database)
                                              (make-state))))
      (hook-run (okvs-hook-on-transaction-begin database) transaction)
      transaction))

  (define (okvs-transaction-commit transaction . args)
    (hook-run (okvs-hook-on-transaction-commit
               (okvs-transaction-database transaction))
              transaction)
    (okvs-store! (okvs-transaction-database transaction)
                 (okvs-transaction-store transaction)))

  (define (okvs-transaction-roll-back transaction . args)
    (void))

  (define (%okvs-in-transaction okvs proc failure success make-state config)
    (let ((transaction (okvs-transaction-begin okvs make-state config)))
      (guard (ex
              (else
               (okvs-transaction-roll-back transaction)
               (failure ex)))
        (call-with-values (lambda () (proc transaction))
          (lambda out
            (okvs-transaction-commit transaction)
            (apply success out))))))

  (define (make-default-state)
    (make-hash-table (make-default-comparator)))

  (define okvs-in-transaction
    (case-lambda
      ((okvs proc) (okvs-in-transaction okvs proc raise values make-default-state '()))
      ((okvs proc failure)
       (okvs-in-transaction okvs proc failure values make-default-state '()))
      ((okvs proc failure success)
       (%okvs-in-transaction okvs proc failure success make-default-state '()))
      ((okvs proc failure success make-state)
       (%okvs-in-transaction okvs proc failure success make-state '()))
      ((okvs proc failure success make-state config)
       (%okvs-in-transaction okvs proc failure success make-state config))))

  (define (okvs-ref okvs-or-transaction key)
    (mapping-ref/default (okvs-transaction-store okvs-or-transaction) key #f))

  (define (okvs-set! okvs-or-transaction key value)
    (okvs-transaction-store! okvs-or-transaction (mapping-set (okvs-transaction-store okvs-or-transaction) key value)))

  (define (okvs-delete! okvs-or-transaction key)
    (okvs-transaction-store! okvs-or-transaction (mapping-delete (okvs-transaction-store okvs-or-transaction) key)))

  (define (okvs-range-remove! okvs-or-transaction start-key start-include? end-key end-include?)
    (let ((generator (okvs-range okvs-or-transaction start-key start-include? end-key end-include?)))
      (let loop ((pair (generator)))
        (unless (eof-object? pair)
          (let ((key (car pair)))
            (okvs-delete! okvs-or-transaction key)
            (loop (generator)))))))

  (define (okvs-range-init store key)
    (let ((value (mapping-ref/default store key #f)))
      (if value
          (list (cons key value))
          '())))

  (define (okvs-range okvs-or-transaction start-key start-include? end-key end-include? . args)
    (let* ((store (okvs-transaction-store okvs-or-transaction)))
      (let loop ((key (mapping-key-successor store start-key (const #f)))
                 (out (if start-include?
                          (okvs-range-init store start-key)
                          '())))
        (if (not key)
            (list->generator (reverse! out))
            (case (lexicographic-compare key end-key)
              ((-1)
               (loop (mapping-key-successor store key (const #f))
                     (cons (cons key (mapping-ref/default store key #f)) out)))
              ((0)
               (if end-include?
                   (loop #f (cons (cons key (mapping-ref/default store key #f)) out))
                   (loop #f out)))
              ((1) (loop #f out)))))))

  (define (strinc bytevector)
    "Return the first bytevector that is not prefix of BYTEVECTOR"
    ;; See https://git.io/fj34F, TODO: OPTIMIZE
    (let ((bytes (reverse! (bytevector->u8-list bytevector))))
      ;; strip #xFF
      (let loop ((out bytes))
        (when (null? out)
          (error 'okvs "Key must contain at least one byte not equal to #xFF." bytevector))
        (if (= (car out) #xFF)
            (loop (cdr out))
            (set! bytes out)))
      ;; increment first byte, reverse and return the bytevector
      (u8-list->bytevector (reverse! (cons (+ 1 (car bytes)) (cdr bytes))))))

  (define (okvs-prefix-range okvs-or-transaction prefix . config)
    (apply okvs-range okvs-or-transaction prefix #t (strinc prefix) #f config))

  (define (make-default-engine)
    (make-engine okvs-ref
                 okvs-set!
                 okvs-delete!
                 okvs-range-remove!
                 okvs-range
                 okvs-prefix-range
                 okvs-hook-on-transaction-begin
                 okvs-hook-on-transaction-commit
                 pack
                 unpack))

  )
