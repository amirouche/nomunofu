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
(define-module (nomunofu web helpers))

;; stdlib
(import (web request))
(import (web response))
(import (web uri))

;; local
(import (nomunofu web html))


;;; helpers

(define-public (request-path-components request)
  "Split the URI path of REQUEST into a list of component strings.  For
example: \"/foo/bar\" yields '(\"foo\" \"bar\")."
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define-public (sxml->response sxml)
  (values '((content-type . (text/html)))
          (lambda (port)
            (sxml->html sxml port))))

(define-public (scheme->response scheme)
  (values '((content-type . (application/scheme)))
          (lambda (port)
            (write scheme port))))

(define-public (forbidden)
  (values (build-response #:code 403)
          "Forbidden"))

(define-public (bad-request reason)
  (values (build-response #:code 400)
          reason))

(define-public (not-found uri)
  (values (build-response #:code 404)
          (string-append "Resource not found: " uri)))

(define (make-set-cookie-header pair) ;; XXX: https://mdn.io/Set-Cookie,
  (let ((key (car pair))
        (value (cdr pair)))
    ;; The cookie will have the lifetime of a session cookie.
    `(Set-Cookie . ,(string-append (symbol->string key)
                                   "= "
                                   value
                                   "; HttpOnly"
                                   "; SameSite=Strict"))))

(define (alist->set-cookie-headers alist)
  (let loop ((alist alist)
             (out '()))
    (if (null? alist)
        out
        (loop (cdr alist) (cons (make-set-cookie-header (car alist)) out)))))

(define-public redirect
  (lambda* (uri #:key cookies)
    (if cookies
        (let ((headers (append `((Location . ,uri))
                               (alist->set-cookie-headers cookies))))
          (values (build-response #:code 303 #:headers headers) ""))
        (values (build-response #:code 303 #:headers `((Location . ,uri))) ""))))

(define-public (internal-error)
  (values (build-response #:code 500) "Internal error."))

(define (%make-cookie item)
  (let ((item* (string-split item #\=)))
    (cons (string->symbol (car item*)) (cadr item*))))

(define-public (request-cookies request)
  (let ((headers (request-headers request)))
    (and=> (assq-ref headers 'cookie)
           (lambda (cookie)
             (map %make-cookie (map string-trim (string-split cookie #\;)))))))
