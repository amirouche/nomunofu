;; This is an implementation of srfi-151, mostly by renaming the
;; procedures already in guile.  it also includes some procedures from
;; Olin Shivers' srfi-33 and some new procedures provided by
;; srfi-151. This library contains parts derived or directly copied
;; from the SRFI reference implementation, and is thus under the srfi
;; license.  I disclaim all copyright for my code and edits and put
;; those under the public domain. For everything below the bits
;; function:
;;
;; Copyright 2017 John Cowan
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions: The above
;; copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-module (srfi srfi-151))


;; Import all procedures that are used verbatim, but renamed, by srfi-151
(import (only (srfi srfi-60)
              bitwise-if
              bit-set?
              copy-bit
              first-set-bit
              bit-field
              copy-bit-field
              rotate-bit-field
              reverse-bit-field
              integer->list
              list->integer
              booleans->integer))

;; re-export everything that can be used unchanged from srfi-60
;; and rename when needed
(re-export (lognot . bitwise-not)
           (logand . bitwise-and)
           (logior . bitwise-ior)
           (logxor . bitwise-xor)
           (ash    . arithmetic-shift)
           integer-length
           bitwise-if
           bit-set?
           (logtest . any-bit-set?)
           copy-bit
           first-set-bit
           bit-field
           (copy-bit-field    . bit-field-replace)
           (rotate-bit-field  . bit-field-rotate)
           (reverse-bit-field . bit-field-reverse))

;; TODO: here it should be something like re-export! but that form
;; doesn't exist, yet.
(re-export (logcount . bit-count))

;; export the procedures that can actually be found in source form in this library
(export bits->list
        list->bits
        bits

        bitwise-nand
        bitwise-nor
        bitwise-andc1
        bitwise-andc2
        bitwise-orc1
        bitwise-orc2
        bitwise-eqv
        every-bit-set?
        bit-field-any?
        bit-field-every?
        bit-field-clear
        bit-field-set
        bit-field-replace-same

        ;; From bitwise-other.scm
        bits->vector
        vector->bits
        bit-swap
        bitwise-fold
        bitwise-for-each
        bitwise-unfold
        make-bitwise-generator)


(define* (bits->list i #:optional len)
  (reverse
   (if len
       (integer->list i len)
       (integer->list i))))

(define (list->bits lst)
  (list->integer (reverse lst)))

(define (bits . args)
  (apply booleans->integer (reverse args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVERYTHING FROM HERE IS COPIED FROM THE REFERENCE IMPLEMENTATION
;; AND THUS COPYRIGHTED BY JOHN COWAN, LICENSED UNDER THE SRFI LICENSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper function -- make a mask of SIZE 1-bits, e.g. (%MASK 3) =
;;; #b111.  Suppose your Scheme's fixnums are N bits wide (counting
;;; the sign bit, not counting any tag bits). This version, due to
;;; Marc Feeley, will handle SIZE in the range [0,N-1] without
;;; overflowing to bignums.  (For SIZE >= N, the correct bignum value
;;; is also produced.)

(define (mask start end)
   (lognot (ash -1 (- end start))))


(define (bitwise-nand  i j)  (lognot (logand i j)))
(define (bitwise-nor   i j)  (lognot (logior i j)))
(define (bitwise-andc1 i j)  (logand (lognot i) j))
(define (bitwise-andc2 i j)  (logand i (lognot j)))
(define (bitwise-orc1  i j)  (logior (lognot i) j))
(define (bitwise-orc2  i j) (logior i (lognot j)))


(define (bitwise-eqv . args)
  (let lp ((args args) (ans -1))
    (if (pair? args)
        (lp (cdr args) (lognot (logxor ans (car args))))
        ans)))


(define (every-bit-set? test-bits n)
  (= test-bits (logand test-bits n)))


(define (bit-field-any? n start end)
  (not (zero? (logand (ash n (- start)) (mask start end)))))


;; Part of Olin's late revisions; code by John Cowan; public domain.
(define (bit-field-every? n start end)
  (let ((m (mask start end)))
    (eqv? m (logand (ash n (- start)) m))))


;; Integrating i-b-f reduces nicely.
(define (bit-field-clear n start end)
  (copy-bit-field n 0 start end))


;; Counterpart to above, not in SRFI 33, written by John Cowan, public domain
(define (bit-field-set n start end)
  (copy-bit-field n -1 start end))


(define (bit-field-replace-same to from start end)
  (bitwise-if (ash (mask start end) start) from to))

;;;; bitwise-other - functions not from SRFI 33 or SRFI 60
;;; Copyright John Cowan 2017
;;; edited to use guile procedures by Linus Björnstam

(define bits->vector
  (case-lambda
    ((i) (list->vector (bits->list i)))
    ((i len) (list->vector (bits->list i len)))))

(define (vector->bits vect)
  (list->bits (vector->list vect)))

(define (bit-swap n1 n2 i)
  (let ((n1-bit (bit-set? n1 i))
        (n2-bit (bit-set? n2 i)))
    (copy-bit n2 (copy-bit n1 i n2-bit) n1-bit)))

(define (bitwise-fold proc seed i)
  (let ((len (integer-length i)))
    (let loop ((n 0) (r seed))
      (if (= n len)
        r
        (loop (+ n 1) (proc (bit-set? n i) r))))))

(define (bitwise-for-each proc i)
  (let ((len (integer-length i)))
    (let loop ((n 0))
      (when (< n len)
        (proc (bit-set? n i))
        (loop (+ n 1))))))

(define (bitwise-unfold stop? mapper successor seed)
  (let loop ((n 0) (result 0) (state seed))
    (if (stop? state)
      result
        (loop (+ n 1)
              (copy-bit n result (mapper state))
              (successor state)))))

(define (make-bitwise-generator i)
  (lambda ()
    (let ((bit (bit-set? 0 i)))
       (set! i (ash i -1))
       bit)))
