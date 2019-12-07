;; The 'nil' configuration applies to all modes.
((scheme-mode . ((eval . (progn
                           ;; example
                           (put 'with-mutex 'scheme-indent-function 1)
                           (put 'okvs-in-transaction 'scheme-indent-function 1)
                           (put 'engine-in-transaction 'scheme-indent-function 2)
                           (put 'with-directory 'scheme-indent-function 1)
                           (put 'match 'scheme-indent-function 1)
                           (put 'with-connection 'scheme-indent-function 2))))))
