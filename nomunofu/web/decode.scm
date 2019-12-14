;;; Copyright © 2017-2018  Amirouche Boubekki <amirouche@hypermove.net>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(define-module (nomunofu web decode))

(import (ice-9 match))
(import (rnrs bytevectors))
(import (srfi srfi-1))
(import (srfi srfi-26))
(import (web uri))

;;;
;;; decode, TODO: rewrite.
;;;

(define (acons-list k v alist)
  "Add V to K to alist as list"
  (let ((value (assoc-ref alist k)))
    (if value
        (let ((alist (alist-delete k alist)))
          (acons k (cons v value) alist))
        (acons k (list v) alist))))

(define (list->alist lst)
  "Build a alist of list based on a list of key and values.

   Multiple values can be associated with the same key"
  (let next ((lst lst)
             (out '()))
    (if (null? lst)
        out
        (next (cdr lst) (acons-list (caar lst) (cdar lst) out)))))

(define-public (decode string-or-bytevector)
  "Convert BV querystring or form data to an alist"
  (let ((string (if (string? string-or-bytevector)
                    string-or-bytevector
                    (utf8->string string-or-bytevector))))
    (let ((pairs (map (cut string-split <> #\=)
                      ;; semi-colon and amp can be used as pair separator
                      (append-map (cut string-split <> #\;)
                                  (string-split string #\&)))))
      (list->alist (map (match-lambda
                          ((key value)
                           (cons (string->symbol (uri-decode key)) (uri-decode value)))) pairs)))))
