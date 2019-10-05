#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (arew srfi srfi-42)
  (export
    do-ec list-ec append-ec string-ec string-append-ec vector-ec
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec
    every?-ec first-ec last-ec fold-ec fold3-ec
    : :list :string :vector :integers :range :real-range :char-range
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch
    dispatch-union :generator-proc)
  (import
    (rnrs)
    (rnrs r5rs)
    (arew srfi srfi-39)
    (arew srfi srfi-23 tricks)
    (for (arew srfi private vanish) expand)
    (arew srfi private include))

  (define-syntax :-dispatch
    (identifier-syntax
      (_ (:-dispatch-param))
      ((set! _ expr) (:-dispatch-param expr))))

  (let-syntax ((define (vanish-define define (:-dispatch))))
    (SRFI-23-error->R6RS "(library (srfi :42 eager-comprehensions))"
     (include/resolve ("srfi" "srfi-42") "srfi-42-body.scm")))

  (define :-dispatch-param (make-parameter (make-initial-:-dispatch)))
)
