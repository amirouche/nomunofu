#!/usr/bin/env scheme
(import (chezscheme))
(import (only (arew srfi srfi-1) map-in-order))
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
      (if (failure? result)
          (begin
            (display "**** expected: ")
            (write (failure-expected result))
            (newline)
            (display "**** actual: ")
            (write (failure-actual result))
            (newline)
            1)
          0))))

(define (run filepath)
  ;; run the tests found at FILEPATH
  (display "** running tests found in ")
  (display filepath)
  (newline)
  (let ((library-name (guess-library-name filepath)))
    (let ((tests (library-exports* filepath library-name)))
      (apply + (map-in-order run-one tests)))))

;; TODO: replace with glob pattern
(define filepaths
  (list "./src/tests-tests.scm"
        ;; scheme
        "./src/arew/scheme/base-tests.scm"
        "./src/arew/scheme/case-lambda-tests.scm"
        ;; srfi
        "./src/arew/srfi/srfi-1-tests.scm"
        "./src/arew/srfi/srfi-146-tests.scm"
        "./src/arew/srfi/srfi-146/hash-tests.scm"
        "./src/arew/srfi/srfi-151-tests.scm"
        "./src/arew/srfi/srfi-158-tests.scm"

        ))

(define (exit* count)
  (if (zero? count)
      (exit 0)
      (exit 1)))

(parameterize ([compile-profile 'source])
  (let ((args (cdr (command-line))))
    (exit* (if (null? args)
               (begin
                 (display "* tests")
                 (newline)
                 (apply + (map-in-order run filepaths)))
               (apply + (map-in-order run args))))))

(profile-dump-html "profile/")
