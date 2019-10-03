#!/usr/bin/env scheme
(import (chezscheme))
(import (tests))


(define (string-split string char)
  (let loop ((index 0)
             (item '())
             (out '()))
    (if (= (string-length string) index)
        (reverse! (cons (list->string (reverse! item)) out))
        (if (char=? (string-ref string index) char)
            (loop (+ index 1) '() (cons (list->string (reverse! item)) out))
            (loop (+ index 1) (cons (string-ref string index) item) out)))))

(define (guess-library-name filepath)
  ;; Try to guess the library name based on the FILEPATH.
  ;; e.g. "./src/foo/bar/baz-test.scm" -> (foo bar baz-test)
  (cdr (map string->symbol (string-split (substring filepath 2 (- (string-length filepath) 4)) #\/))))

(define (library-exports* filepath library-name)
  ;; return the procedure defined in library LIBRARY-NAME at FILEPATH
  ;; XXX: hackish at best, there might be a better solution
  (let ((env (interaction-environment)))
    (let ((program `(begin
                      (import ,library-name)
                      (let ((exports (library-exports ',library-name)))
                        exports))))
      (let ((exports (eval program env)))
        (let ((program `(begin
                          (import ,library-name)
                          (map cons ',exports (list ,@exports)))))
          (reverse (eval program env)))))))

(define (run-one pair)
  (display "*** ")
  (display (car pair))
  (newline)
  (guard (x (else (display-condition x) (newline)))
    (let ((result ((cdr pair))))
      (when (failure? result)
        (display "**** expected: ")
        (write (failure-expected result))
        (newline)
        (display "**** actual: ")
        (write (failure-actual result))
        (newline)))))

(define (run filepath)
  ;; run the tests found at FILEPATH
  (display "** running tests found in ")
  (display filepath)
  (newline)
  (let ((library-name (guess-library-name filepath)))
    (let ((tests (library-exports* filepath library-name)))
      (for-each run-one tests))))

(parameterize ([compile-profile 'source])
  (let ((args (cdr (command-line))))
    (if (null? args)
        (begin
          (display "* tests")
          (newline)
          (run "./src/tests-tests.scm")
          ;; (run "./src/srfi/srfi-1-tests.scm")
          ;; (run "./src/srfi/srfi-2-tests.scm")
          ;; (run "./src/srfi/srfi-4-tests.scm")
          ;; (run "./src/srfi/srfi-16-tests.scm")
          ;; (run "./src/srfi/srfi-26-tests.scm")
          )

        (for-each run args))))

(profile-dump-html "profile/")
