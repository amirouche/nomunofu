(library (arew check)
  (export failure?
          success?
          failure-expected
          failure-actual
          test
          run-check)

  (import (chezscheme)
          (arew base)
          (only (srfi srfi-1) map-in-order))

  (begin

    (define-syntax-rule (test expected actual)
      (lambda ()
        (let ((expected* expected)
              (actual* actual))
          (if (equal? expected* actual*)
              (list #t)
              (list #f expected* actual*)))))

    (define (failure? lst)
      (not (car lst)))

    (define (success? lst)
      (car lst))

    (define (failure-expected lst)
      (list-ref lst 1))

    (define (failure-actual lst)
      (list-ref lst 2))

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

    (define (exit* count)
      (profile-dump-html "profile/")
      (if (zero? count)
          (exit 0)
          (exit 1)))

    (define (run-check)
      (parameterize ([compile-profile 'source])
        (let ((args (cdr (command-line))))
          (exit* (apply + (map-in-order run args))))))))
