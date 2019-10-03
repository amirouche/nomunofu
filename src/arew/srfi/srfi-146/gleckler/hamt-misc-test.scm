;;;; Tests of utilities used by HAMT

;;; Copyright MMIV-MMXVII Arthur A. Gleckler.  All rights reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.



(define (run-hamt-misc-tests)
  (test-begin "hamt-misc")

  (test-group "(do-list)"
    (let ((index-accumulator '())
	  (value-accumulator '())
	  (all-values '(1 2 3 4 5)))
      (do-list (value all-values)
	(set! value-accumulator (cons value value-accumulator)))
      (test-equal all-values (reverse value-accumulator))
      (do-list (value index all-values)
	(set! index-accumulator (cons index index-accumulator)))
      (test-equal '(4 3 2 1 0) index-accumulator)))

  (test-end))
