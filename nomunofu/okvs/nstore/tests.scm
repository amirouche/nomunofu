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
(define-module (nomunofu okvs nstore tests))

(import (only (rnrs) guard error))

(import (nomunofu fash))
(import (nomunofu generator))
(import (nomunofu okvs nstore))
(import (nomunofu okvs engine))
(import (nomunofu okvs wiredtiger))
(import (nomunofu testing))


(define engine (make-default-engine))

(define (triplestore)
  (nstore engine (list 42 1337) '(uid key value)))

(define-public test-000
  (test ;; empty database
   #f
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       ;; ask
       (let ((out (engine-in-transaction
                   engine okvs
                   (lambda (transaction)
                     (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
         (engine-close engine okvs)
         out)))))

(define-public test-001
  (test ;; add and ask triplestore"
   #t
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       ;; add
       (engine-in-transaction
        engine okvs
        (lambda (transaction)
          (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))))
       ;; ask
       (let ((out
              (engine-in-transaction
               engine okvs
               (lambda (transaction)
                 (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
         (engine-close engine okvs)
         out)))))

(define-public test-002
  (test ;; "add, rm and ask triplestore"
   #f
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       (let ((out
              (engine-in-transaction
               engine okvs
               (lambda (transaction)
                 ;; add!
                 (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                 ;; remove!
                 (nstore-delete! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                 ;; ask
                 (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
         (engine-close engine okvs)
         out)))))

(define-public test-003
  (test ;; "blog query post titles"
   '("DIY a database" "DIY a full-text search engine")
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       (engine-in-transaction
        engine okvs
        (lambda (transaction)
          ;; add hyper.dev blog posts
          (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
          (nstore-add! transaction triplestore '("123456" post/title "DIY a database"))
          (nstore-add! transaction triplestore '("123456" post/blog "P4X432"))
          (nstore-add! transaction triplestore '("654321" post/title "DIY a full-text search engine"))
          (nstore-add! transaction triplestore '("654321" post/blog "P4X432"))
          ;; add dthompson.us blog posts
          (nstore-add! transaction triplestore '("1" blog/title "dthompson.us"))
          (nstore-add! transaction triplestore '("2" post/title "Haunt 0.2.4 released"))
          (nstore-add! transaction triplestore '("2" post/blog "1"))
          (nstore-add! transaction triplestore '("3" post/title "Haunt 0.2.3 released"))
          (nstore-add! transaction triplestore '("3" post/blog "1"))))
       ;; query
       (let ()
         (define query
           (lambda (transaction blog/title)
             (generator->list (nstore-query
                               (nstore-select transaction triplestore
                                              (list (nstore-var 'blog/uid)
                                                    'blog/title
                                                    blog/title))
                               (nstore-where transaction triplestore
                                             (list (nstore-var 'post/uid)
                                                   'post/blog
                                                   (nstore-var 'blog/uid)))
                               (nstore-where transaction triplestore
                                             (list (nstore-var 'post/uid)
                                                   'post/title
                                                   (nstore-var 'post/title)))))))
         (let* ((out (engine-in-transaction engine okvs (lambda (transaction) (query transaction "hyper.dev"))))
                (out (map (lambda (x) (fash-ref x 'post/title)) out)))
           (engine-close engine okvs)
           out))))))

(define-public test-004
  (test ;; "nstore-select limit and offset"
   '("hyperdev.fr")
   (with-directory "wt"
     (let ((okvs (engine-open engine "wt"))
           (triplestore (triplestore)))
       ;; add!
       (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
       (nstore-add! okvs triplestore '("P4X433" blog/title "hyperdev.fr"))
       (nstore-add! okvs triplestore '("P4X434" blog/title "hypermove.net"))
       (let ((out (engine-in-transaction
                   engine okvs
                   (lambda (transaction)
                     (generator-map->list
                      (lambda (item) (fash-ref item 'title))
                      (nstore-select transaction triplestore (list (nstore-var 'uid)
                                                                   'blog/title
                                                                   (nstore-var 'title))
                                   `((limit . 1) (offset . 1))))))))
         (engine-close engine okvs)
         out)))))

(define-public test-005
  (test ;; "nstore validation add via hooks"
   #t
   (with-directory "wt"
     (let* ((okvs (engine-open engine "wt"))
            (triplestore (triplestore))
            (hook (nstore-hook-on-add triplestore)))
       (add-hook! hook (lambda (nstore items)
                         (when (string=? (car items) "private")
                           (error 'nstore-hook "private is private" items))))
       ;; replace with nomunofu testing's test-raise
       (guard (ex (else (engine-close engine okvs) #t))
         (nstore-add! okvs triplestore '("private" private "private"))
         #f)))))

(define-public test-006
  (test ;; "nstore validation delete via hooks"
   #t
   (with-directory "wt"
     (let* ((okvs (engine-open engine "wt"))
            (triplestore (triplestore))
            (hook (nstore-hook-on-delete triplestore)))
       (add-hook! hook (lambda (nstore items)
                         (when (string=? (car items) "private")
                           (error 'nstore-hook "private is private" items))))
       ;; replace with nomunofu testing's test-raise
       (guard (ex (else (engine-close engine okvs) #t))
         (nstore-delete! okvs triplestore '("private" private "private"))
         #f)))))
