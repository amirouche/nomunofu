;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: CC0-1.0
#!r6rs

(library (arew scheme load)
  (export load)
  (import
   (arew scheme base)
   (arew scheme case-lambda)
   (arew scheme read)
   (arew scheme eval)
   (arew scheme repl))

  (define load
    (case-lambda
      ((fn)
       (load fn (interaction-environment)))
      ((fn env)
       (call-with-input-file fn
         (lambda (p)
           (let loop ()
             (let ((x (read reader)))
               (unless (eof-object? x)
                 (eval x env)
                 (loop)))))))))))
