;;; Copyright © 2014       David Thompson <davet@gnu.org>
;;; Copyright © 2015-2018  Amirouche Boubekki <amirouche@hypermove.net>
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
(define-module (nomunofu web static))

;; stdlib
(import (ice-9 binary-ports))

;; local
(import (nomunofu web mime-types))
(import (nomunofu web helpers))

;;;
;;; static assets rendering
;;;

(define (valid? path)
  (null? (filter (lambda (component) (equal? component "..")) path)))

(define (directory? filename)
  (string=? filename (dirname filename)))

(define-public (render-static-asset path)
  (if (valid? path)
      ;; XXX: the requested file is translated into a filepath related to where
      ;; the server is executed.
      (let ((filename (string-join (cons* (getcwd) "static" path)
                                   "/")))
        (if (and (file-exists? filename) (not (directory? filename)))
            (values `((content-type ,(mime-type filename)))
                    (call-with-input-file filename get-bytevector-all))
            (not-found (string-join (cons "static" path) "/" 'prefix))))
      (forbidden)))
