(define-module (nomunofu testing))

(export test)
(export test-raise)
(export with-directory)

(import (rnrs))


(define-syntax-rule (test expected actual)
  (lambda ()
    (let ((expected* expected)
          (actual* actual))
      (if (equal? expected* actual*)
          (list #t)
          (list #f 'unexpected expected* actual*)))))


(define %does-not-raise (list 'does-not-raise))
(define %wrong-exception (list 'wrong-exception))
(define %good-exception (list 'good-exception))


(define-syntax-rule (test-raise predicate? thunk)
  (lambda ()
    (let ((out (guard (ex ((predicate? ex) %good-exception) (else %wrong-exception))
                 (thunk)
                 %does-not-raise)))
      (if (eq? out %good-exception)
          (list #t)
          (list #f 'raise-error predicate? out)))))

;; helper: (with-directory path body ...)

(define (path-exists? path)
  "Return #true if path is a file or directory.
   #false if it doesn't exists"
  (access? path F_OK))

(define (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be prepended"
  (let ((path (string-join rest "/")))
    (if (string-prefix? "/" path)
        path
        (string-append (getcwd) "/" path))))

(define (path-split path)
  (let ((parts (string-split path #\/)))
    (if (equal? (car parts) "")
        (cons (string-append "/" (cadr parts)) (cddr parts))
        parts)))

(define (path-mkdir dirpath parents)
  "Create DIRPATH directory and its parents if PARENTS is true"
  (if parents
      (let* ((parts (path-split dirpath))
             (paths (let loop ((dirs (cdr parts))
                               (out (list (car parts))))
                      (if (null? dirs)
                          (reverse out)
                          (loop (cdr dirs) (cons (apply path-join (list (car out) (car dirs))) out))))))
        (and (map (lambda (p) (if (not (path-exists? p)) (mkdir p))) paths) #true))
      (if (not (path-exists? dirpath)) (and (mkdir dirpath) #true))))

(define (path-walk dirpath proc)
  (define dir (opendir dirpath))
  (let loop ()
    (let ((entry (readdir dir)))
      (cond
       ((eof-object? entry))
       ((or (equal? entry ".") (equal? entry "..")) (loop))
       (else (let ((path (path-join dirpath entry)))
               (if (equal? (stat:type (stat path)) 'directory)
                   (begin (path-walk path proc) (loop))
                   (begin (proc path) (loop))))))))
  (closedir dir)
  (proc (path-join dirpath)))

(define (rmtree path)
  (path-walk path (lambda (path)
                    (if (equal? (stat:type (stat path)) 'directory)
                        (rmdir path)
                        (delete-file path)))))

;; TODO: a) replace with call-with-tmp-directory, b) guard on
;; exception to delete the directory c) make final rmtree optional, d)
;; make it possible to return multiple values.
(define-syntax-rule (with-directory path e ...)

  (begin
    (when (access? path F_OK)
      (rmtree path))
    (mkdir path)
    (let ((out (begin e ...)))
      (rmtree path)
      out)))
