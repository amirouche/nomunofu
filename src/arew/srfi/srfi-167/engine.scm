(library (arew srfi srfi-167 engine)

  (export
   make-engine
   engine?
   engine-ref
   engine-set
   engine-delete
   engine-range-remove
   engine-range
   engine-prefix-range
   engine-hook-on-transaction-begin
   engine-hook-on-transaction-commit
   engine-pack
   engine-unpack)

  (import (arew scheme base))

  (define-record-type <engine>
    (make-engine ref
                 set
                 delete
                 range-remove
                 range
                 prefix-range
                 hook-on-transaction-begin
                 hook-on-transaction-commit
                 pack
                 unpack)
    engine?
    (ref engine-ref)
    (set engine-set)
    (delete engine-delete)
    (range-remove engine-range-remove)
    (range engine-range)
    (prefix-range engine-prefix-range)
    (hook-on-transaction-begin engine-hook-on-transaction-begin)
    (hook-on-transaction-commit engine-hook-on-transaction-commit)
    (pack engine-pack)
    (unpack engine-unpack)))
