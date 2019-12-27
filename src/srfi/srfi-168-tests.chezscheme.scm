;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
(library (srfi srfi-168-tests)

  (export test-000
          test-001
          test-002
          test-003
          test-004
          test-005
          test-006)

  (import (scheme base)
          (scheme mapping hash)
          (scheme generator)
          (srfi srfi-168)
          (srfi srfi-167 engine)
          (srfi srfi-167 wiredtiger)
          (srfi srfi-173)
          (arew cffi stdlib)
          (tests))

  (define (call-with-new-directory name proc)
    (let ((filepath (mkdtemp (string-append "/tmp/arew-wiredtiger-tests-" name "-"))))
      (proc filepath)))

  (define engine (make-default-engine))

  (define (triplestore)
    (nstore engine (list 42 1337) '(uid key value)))

  (define test-000
    (test ;; empty database
     #f
     (call-with-new-directory "000"
       (lambda (filepath)
         (let ((okvs (engine-open engine filepath))
               (triplestore (triplestore)))
           ;; ask
           (let ((out (engine-in-transaction
                          engine okvs
                        (lambda (transaction)
                          (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
             (engine-close engine okvs)
             out))))))

  (define test-001
    (test ;; add and ask triplestore"
     #t
     (call-with-new-directory "001"
       (lambda (filepath)
         (let ((okvs (engine-open engine filepath))
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
             out))))))

  (define test-002
    (test ;; "add, rm and ask triplestore"
     #f
     (call-with-new-directory "002"
       (lambda (filepath)
         (let ((okvs (engine-open engine filepath))
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
             out))))))

  (define test-003
    (test ;; "blog query post titles"
     '("DIY a database" "DIY a full-text search engine")
     (call-with-new-directory "003"
       (lambda (filepath)
         (let ((okvs (engine-open engine filepath))
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
                    (out (map (lambda (x) (hashmap-ref x 'post/title)) out)))
               (engine-close engine okvs)
               out)))))))

  (define test-004
    (test ;; "nstore-select limit and offset"
     '("hyperdev.fr")
     (call-with-new-directory "004"
       (lambda (filepath)
         (let ((okvs (engine-open engine filepath))
               (triplestore (triplestore)))
           ;; add!
           (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
           (nstore-add! okvs triplestore '("P4X433" blog/title "hyperdev.fr"))
           (nstore-add! okvs triplestore '("P4X434" blog/title "hypermove.net"))
           (let ((out (engine-in-transaction
                          engine okvs
                        (lambda (transaction)
                          (generator-map->list
                           (lambda (item) (hashmap-ref item 'title))
                           (nstore-select transaction triplestore (list (nstore-var 'uid)
                                                                        'blog/title
                                                                        (nstore-var 'title))
                                          `((limit . 1) (offset . 1))))))))
             (engine-close engine okvs)
             out))))))

  (define test-005
    (test ;; "nstore validation add via hooks"
     #t
     (call-with-new-directory "005"
       (lambda (filepath)
         (let* ((okvs (engine-open engine filepath))
                (triplestore (triplestore))
                (hook (nstore-hook-on-add triplestore)))
           (hook-add! hook (lambda (nstore items)
                             (when (string=? (car items) "private")
                               (error 'nstore-hook "private is private" items))))
           ;; replace with nomunofu testing's test-raise
           (guard (ex (else (engine-close engine okvs) #t))
             (nstore-add! okvs triplestore '("private" private "private"))
             #f))))))

  (define test-006
    (test ;; "nstore validation delete via hooks"
     #t
     (call-with-new-directory "006"
       (lambda (filepath)
         (let* ((okvs (engine-open engine filepath))
                (triplestore (triplestore))
                (hook (nstore-hook-on-delete triplestore)))
           (hook-add! hook (lambda (nstore items)
                             (when (string=? (car items) "private")
                               (error 'nstore-hook "private is private" items))))
           ;; replace with nomunofu testing's test-raise
           (guard (ex (else (engine-close engine okvs) #t))
             (nstore-delete! okvs triplestore '("private" private "private"))
             #f)))))))
