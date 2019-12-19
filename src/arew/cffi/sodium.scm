#!chezscheme
(library (arew cffi sodium)

  (export sodium-init sodium-generic-hash)

  (import (chezscheme)
          (arew base))


  (define sodium (load-shared-object "local/lib/libsodium.so"))

  ;; bytevector cffi helper

  (define (bytevector-pointer bv)
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  (define (call-with-lock obj thunk)
    (lock-object obj)
    (call-with-values thunk
      (lambda out
        (unlock-object obj)
        (apply values out))))

  ;; cffi helper

  (define-syntax-rule (check code)
    (let ((code* code))
      (unless (zero? code*)
        (raise (cons 'sodium code*)))))

  ;; libsodium

  (define sodium-init
    (let ((proc (foreign-procedure "sodium_init" () int)))
      (lambda ()
        (check (proc)))))

  (define crypto-generichash-bytes
    (let ((proc (foreign-procedure "crypto_generichash_bytes" () size_t)))
      (lambda ()
        (proc))))

  (define sodium-generic-hash
    (let ((proc (foreign-procedure "crypto_generichash"
                                   (void* size_t void* size_t void* size_t)
                                   int)))
      (lambda (in)
        (let* ((length (crypto-generichash-bytes))
               (out (make-bytevector length)))
          (call-with-lock out
            (lambda ()
              (call-with-lock in
                (lambda ()
                  (check (proc (bytevector-pointer out)
                               length
                               (bytevector-pointer in)
                               (bytevector-length in)
                               0 0))))))
          out)))))
