(library (arew data microxml)
  (export microxml->sxml)
  (import
   (arew scheme base)
   (arew data yxml)
   (arew srfi srfi-145))

  (define %element-name 0)
  (define %attributes 1)
  (define %content 2)

  (define (content-massage lst)
    ;; reverse the list, join bytevectors and convert them to strings.
    (let loop ((lst lst)
               (data '())
               (out '()))
      (cond
       ((null? lst)
        (if (null? data)
            out
            (cons (utf8->string (apply bytevector-append data)) out)))
       ((bytevector? (car lst)) (loop (cdr lst) (cons (car lst) data) out))
       (else
        (assume (vector? (car lst)))
        (if (null? data)
            (loop (cdr lst) '() (cons (car lst) out))
            (let ((string (utf8->string (apply bytevector-append data))))
              (loop (cdr lst) '() (cons (car lst) (cons string out)))))))))

  (define (make-machine)
    (let ((stack '()))
      (lambda (event . args)
        (case event
          ((element-data)
           (let* ((data (car args))
                  (head (car stack))
                  (content (vector-ref head %content)))
             (vector-set! head %content (cons data content))))
          ((element-start)
           (let ((element-name (string->symbol (car args))))
             (set! stack (cons (vector element-name '() '()) stack))))
          ((element-end)
           (let* ((child (car stack))
                  (child-content (content-massage (vector-ref child %content))))
             (vector-set! child %content child-content)
             (unless (null? (cdr stack))
               (let* ((parent (cadr stack))
                      (parent-content (vector-ref parent %content)))
                 (vector-set! parent %content (cons child parent-content))
                 (set! stack (cdr stack))))))
          ((attribute-value)
           (let* ((data (car args))
                  (head (car stack))
                  (attributes (vector-ref head %attributes))
                  (key (caar attributes))
                  (value (cdar attributes))
                  (new-value (cons data value)))
             (vector-set! head %attributes (cons (cons key new-value) (cdr attributes)))))
          ((attribute-start)
           (let* ((name (string->symbol (car args)))
                  (head (car stack))
                  (attributes (vector-ref head %attributes)))
             (vector-set! head %attributes (cons (cons name '()) attributes))))
          ((attribute-end)
           (let* ((head (car stack))
                  (attributes (vector-ref head %attributes))
                  (key (caar attributes))
                  (value (cdar attributes))
                  (new-value (utf8->string (apply bytevector-append (reverse value)))))
             (vector-set! head %attributes (cons (list key new-value) (cdr attributes)))))
          ((finalize) (car stack))
          (else (error 'microxml "not supported, yet!" event))))))


  (define (sxmlize obj)
    (if (string? obj)
        obj
        (if (null? (vector-ref obj %attributes))
            (cons (vector-ref obj %element-name) (map sxmlize (vector-ref obj %content)))
            `(,(vector-ref obj %element-name)
               (* ,@(vector-ref obj %attributes))
               ,@(map sxmlize (vector-ref obj %content))))))

  (define (microxml->sxml string)
    (let ((input (string->utf8 string))
          (yxml (yxml-init))
          (eval! (make-machine)))
      (let ((error?
             (let loop ((index 0))
               (if (= index (bytevector-length input))
                   #f
                   (let ((code (yxml-parse yxml (bytevector-u8-ref input index))))
                     (case code
                       ((0) (loop (+ index 1)))
                       ((-5 -4 -3 -2 -1) #t)
                       ((1)
                        (eval! 'element-start (yxml-element-name yxml))
                        (loop (+ index 1)))
                       ((2)
                        (eval! 'element-data (yxml-data yxml))
                        (loop (+ index 1)))
                       ((3)
                        (eval! 'element-end)
                        (loop (+ index 1)))
                       ((4)
                        (eval! 'attribute-start (yxml-attribute yxml))
                        (loop (+ index 1)))
                       ((5)
                        (eval! 'attribute-value (yxml-data yxml))
                        (loop (+ index 1)))
                       ((6)
                        (eval! 'attribute-end)
                        (loop (+ index 1)))
                       ((7) (loop (+ index 1)))
                       ((8) (loop (+ index 1)))
                       ((9) (loop (+ index 1)))))))))
        (if error?
            #f
            (if (= (yxml-eof yxml) 0)
                (sxmlize (eval! 'finalize))
                #f)))))

  )
