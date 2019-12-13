;; guile-wiredtiger
;;
;; Copyright © 2014-2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;; guile-wiredtiger is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.
;;
;; guile-wiredtiger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-wiredtiger.  If not, see <http://www.gnu.org/licenses/>
;;
;;; Comment:
;;
;; - 2018/02/21: use to bytestructure and update to wiredtiger 3.0
;;
;; - 2018/09/16: update to wiredtiger 3.1
;;
;; - 2019/05/14: use to wiredtiger 3.2 and support bytes columns
;;
;;; TODO:
;;
;; - Cache function pointers procedures,
;;
;; - Only support a single bytevector argument in cursor-key-set and
;;   cursor cursor-value-set
;;
(define-module (nomunofu wiredtiger))

(import (scheme base))
(import (bytestructures guile))
(import (prefix (nomunofu cffi) ffi:))
(import (nomunofu wiredtiger config))
(import (rnrs bytevectors))


;;; ffi helpers

(define NULL ffi:%null-pointer)
(define POINTER '*)

;; XXX: only use that procedure in your project if you don't need to
;; access static variables
(define (dynamic-link* library-name)
  (let ((shared-object (ffi:dynamic-link library-name)))
    (lambda (return-value function-name . arguments)
      (let ((function (ffi:dynamic-func function-name shared-object)))
        (ffi:pointer->procedure return-value function arguments)))))

(define (pointer->procedure* return-type function-pointer . args_types)
  (ffi:pointer->procedure return-type function-pointer args_types))

;; bytestructures helper

(define (pointer->bytestructure desc pointer)
  (let ((size (bytestructure-descriptor-size desc)))
    (let ((bv (ffi:pointer->bytevector pointer size)))
      (make-bytestructure bv 0 desc))))

(define bytestructure->pointer (compose ffi:bytevector->pointer
                                        bytestructure-bytevector))

(define (make-double-pointer)
  (ffi:bytevector->pointer (u64vector 0)))

(define (bytestructure-ref* desc value)
  (ffi:make-pointer (bytestructure-ref desc value)))

;;;
;;; wiredtiger bindings
;;;

(define wiredtiger (dynamic-link* %libwiredtiger))

(define-public wiredtiger-string-error
  (let ((function (wiredtiger POINTER "wiredtiger_strerror" ffi:int)))
    (lambda (code)
      (ffi:pointer->string (function code)))))

(define (check code)
  (unless (zero? code)
    (raise (cons 'wiredtiger code))))

(define %key-not-found -31803)

;;; wt_item

(define %item
  (bs:struct
   `((data ,uintptr_t)
     (size ,uint32)
     (mem ,uintptr_t)
     (memsize ,size_t)
     (flags ,uint32))))

;;;
;;; Connection
;;;

(define %connection
  (bs:struct
   `((async-flush ,uintptr_t)
     (async-new-op ,uintptr_t)
     (close ,uintptr_t)
     (debug-info ,uintptr_t)
     (reconfigure ,uintptr_t)
     (get-home ,uintptr_t)
     (configure-method ,uintptr_t)
     (is-new ,uintptr_t)
     (open-session ,uintptr_t)
     (query-timestamp ,uintptr_t)
     (set-timestamp ,uintptr_t)
     (rollback-to-stable ,uintptr_t)
     (load-extension ,uintptr_t)
     (add-data-source ,uintptr_t)
     (add-collator ,uintptr_t)
     (add-compressor ,uintptr_t)
     (add-encryptor ,uintptr_t)
     (add-extractor ,uintptr_t)
     (set-file-system ,uintptr_t)
     (get-extension-api ,uintptr_t))))

(define-public connection-open
  (let ((function (wiredtiger ffi:int "wiredtiger_open" POINTER POINTER POINTER POINTER)))
    (lambda (home config)
      (let ((double-pointer (make-double-pointer)))
        (check (function (ffi:string->pointer home) NULL (ffi:string->pointer config) double-pointer))
        (pointer->bytestructure %connection (ffi:dereference-pointer double-pointer))))))

(define-public (connection-close connection config)
  "Close CONNECTION. Any open sessions or cursors will be closed. CONFIG
   must be a string. It accept a single option `leak_memory` a boolean flag
   which default value is `false`. If `leak_memory` is set to `true` memory
   will not be freed during close"
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* connection 'close) POINTER POINTER)))
    (check (function (bytestructure->pointer connection) (ffi:string->pointer config)))))

;;;
;;; Session
;;;

(define %session
  (bs:struct
   `((connection ,uintptr_t)
     (app-private ,uintptr_t)
     (close ,uintptr_t)
     (reconfigure ,uintptr_t)
     (strerror ,uintptr_t)
     (open-cursor ,uintptr_t)
     (alter ,uintptr_t)
     (create ,uintptr_t)
     (compact ,uintptr_t)
     (drop ,uintptr_t)
     (join ,uintptr_t)
     (log-flush ,uintptr_t)
     (log-printf ,uintptr_t)
     (rebalance ,uintptr_t)
     (rename ,uintptr_t)
     (reset ,uintptr_t)
     (salvage ,uintptr_t)
     (truncate ,uintptr_t)
     (upgrade ,uintptr_t)
     (verify ,uintptr_t)
     (begin-transaction ,uintptr_t)
     (commit-transaction ,uintptr_t)
     (prepare-transaction ,uintptr_t)
     (rollback-transaction ,uintptr_t)
     (timestamp-transaction ,uintptr_t)
     (checkpoint ,uintptr_t)
     (snapshot ,uintptr_t)
     (transaction-pinned-range ,uintptr_t)
     (transaction-sync ,uintptr_t))))

(define-public (session-open connection config)
  "Open a session against CONNECTION. CONFIG accepts `isolation` as
   only options. It can be `read-uncommited`, `read-commited` or
   `snapshot`."
  (let ((double-pointer (make-double-pointer))
        (function (pointer->procedure* ffi:int
                                       (bytestructure-ref* connection 'open-session)
                                       POINTER POINTER POINTER POINTER)))
    (check (function (bytestructure->pointer connection) NULL (ffi:string->pointer config) double-pointer))
    (pointer->bytestructure %session (ffi:dereference-pointer double-pointer))))

(define-public (session-create session name config)
  "Create a table, column group, index or file."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* session 'create) POINTER POINTER POINTER)))
    (check (function (bytestructure->pointer session) (ffi:string->pointer name) (ffi:string->pointer config)))))

(define-public (session-close session)
  "Close the session handle. This will release the resources
   associated with the session handle, including rolling back any active
   transactions and closing any cursors that remain open in the session."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* session 'close) POINTER)))
    (check (function (bytestructure->pointer session)))))

(define-public (session-transaction-begin session config)
  "Start a transaction. A transaction remains active until ended."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* session 'begin-transaction) POINTER POINTER)))
    (check (function (bytestructure->pointer session) (ffi:string->pointer config)))))

(define-public (session-transaction-commit session config)
  "Commit the current transaction. A transaction must be in progress when this method is called.

   If the transaction was rolledback, it will raise a `wiredtiger` exception"
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* session 'commit-transaction) POINTER POINTER)))
    (check (function (bytestructure->pointer session) (ffi:string->pointer config)))))

(define-public (session-transaction-rollback session config)
  "Rollback the current transaction. A transaction must be in progress
   when this method is called. All cursors are reset."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* session 'rollback-transaction) POINTER POINTER)))
    (check (function (bytestructure->pointer session) (ffi:string->pointer config)))))

(define-public (session-reset session)
  "Resets all cursors associated with this session and discards cached
resources. The session can be re-used immediately after this call
returns. If a transaction is running on this session, then this call
takes no action and return an error."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* session 'reset) POINTER)))
    (check (function (bytestructure->pointer session)))))

;;;
;;; Cursor
;;;

(define %cursor
  (bs:struct
   `((session ,uintptr_t)
     (uri ,uintptr_t)
     (key-format ,uintptr_t)
     (value-format ,uintptr_t)
     (get-key ,uintptr_t)
     (get-value ,uintptr_t)
     (set-key ,uintptr_t)
     (set-value ,uintptr_t)
     (compare ,uintptr_t)
     (equals ,uintptr_t)
     (next ,uintptr_t)
     (prev ,uintptr_t)
     (reset ,uintptr_t)
     (search ,uintptr_t)
     (search-near ,uintptr_t)
     (insert ,uintptr_t)
     (modify ,uintptr_t)
     (update ,uintptr_t)
     (remove ,uintptr_t)
     (reserve ,uintptr_t)
     (close ,uintptr_t)
     (reconfigure ,uintptr_t)
     ;; ... TODO some more fields
     )))

(define-public (cursor-open session uri config)
  "Open a new cursor on a data source."
  (let ((function (pointer->procedure* ffi:int
                                       (bytestructure-ref* session 'open-cursor)
                                       POINTER POINTER POINTER POINTER POINTER)))
    (let* ((double-pointer (make-double-pointer)))
      (check (function (bytestructure->pointer session)
                       (ffi:string->pointer uri)
                       NULL
                       (ffi:string->pointer config)
                       double-pointer))
      (pointer->bytestructure %cursor (ffi:dereference-pointer double-pointer)))))

(define-public (cursor-close cursor)
  "Close the cursor. This releases the resources associated with the cursor handle.
   Cursors are closed implicitly by ending the enclosing connection or
   closing the session in which they were opened."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'close) POINTER)))
    (check (function (bytestructure->pointer cursor)))))


;;; key/value set/ref

(define-public (cursor-key-format cursor)
  (ffi:pointer->string (bytestructure-ref* cursor 'key-format)))

(define-public (cursor-value-format cursor)
  (ffi:pointer->string (bytestructure-ref* cursor 'value-format)))

;; ref

(define (item->string double-pointer)
  (ffi:pointer->string (ffi:dereference-pointer double-pointer)))

(define (item->unsigned-64 double-pointer)
  (u64vector-ref (ffi:pointer->bytevector double-pointer 8) 0))

(define (item->bytevector double-pointer)
  (let ((item (pointer->bytestructure %item double-pointer)))
    (bytevector-copy
     (ffi:pointer->bytevector (bytestructure-ref* item 'data)
                              (bytestructure-ref item 'size)))))

(define %char->type-ref `((#\S . ,item->string)
                          (#\Q . ,item->unsigned-64)
                          (#\r . ,item->unsigned-64)
                          (#\u . ,item->bytevector)))

(define (pointers->scm formats pointers)
  (let loop ((formats (string->list formats))
             (pointers pointers))
    (if (null? formats)
        '()
        (cons ((assoc-ref %char->type-ref (car formats)) (car pointers))
              (loop (cdr formats) (cdr pointers))))))

(define-public (cursor-key-ref cursor)
  "Retrieve the current key"
  ;; TODO: OPTIM: avoid the ffi:pointer->procedure to ease the garbage
  ;; the collector.

  ;; TODO: Drop multiple value return.
  (let ((format (cursor-key-format cursor)))
    (let* ((args (map (lambda _ (make-double-pointer)) (string->list format)))
           (args* (cons (bytestructure->pointer cursor) args))
           (signature (map (lambda _ POINTER) args*))
           (function (ffi:pointer->procedure ffi:int (bytestructure-ref* cursor 'get-key) signature)))
      (check (apply function args*))
      (apply values (pointers->scm format args)))))

(define-public (cursor-value-ref cursor)
  "Retrieve the current value"
  (let ((format (cursor-value-format cursor)))
    (let* ((args (map (lambda _ (make-double-pointer)) (string->list format)))
           (args* (cons (bytestructure->pointer cursor) args))
           (signature (map (lambda _ POINTER) args*))
           (function (ffi:pointer->procedure ffi:int (bytestructure-ref* cursor 'get-value) signature)))
      (check (apply function args*))
      (apply values (pointers->scm format args)))))

;; set

(define (make-string-pointer string)
  (ffi:string->pointer string))

(define (bytevector->item bv)
  (let ((item (bytestructure %item)))
    (bytestructure-set! item 'data (ffi:pointer-address (ffi:bytevector->pointer bv)))
    (bytestructure-set! item 'size (bytevector-length bv))
    (bytestructure->pointer item)))

(define *format->pointer* `((#\S . ,make-string-pointer) ;; TODO: drop it
                            (#\Q . ,ffi:make-pointer) ;; TODO: drop it
                            (#\r . ,ffi:make-pointer) ;; TODO: drop it
                            (#\u . ,bytevector->item)))

(define (formats->items formats values) ;; TODO: drop it
  (let loop ((formats (string->list formats))
             (values values))
    (if (null? formats)
        '()
        (cons ((assoc-ref *format->pointer* (car formats)) (car values))
              (loop (cdr formats)
                    (cdr values))))))

(define-public (cursor-key-set cursor . key)
  "Set the key for the next operation. If an error occurs during this operation,
   a flag will be set in the cursor, and the next operation to access the value
   will fail. This simplifies error handling in applications.

   KEY must consistent with the format of the current object key."
  (let* ((args (cons (bytestructure->pointer cursor) (formats->items (cursor-key-format cursor) key)))
         (signature (map (lambda _ POINTER) args))
         (function (ffi:pointer->procedure ffi:void (bytestructure-ref* cursor 'set-key) signature)))
    (apply function args)))

(define-public (cursor-value-set cursor . value)
  "Set the value for the next operation. If an error occurs during this operation,
   a flag will be set in the cursor, and the next operation to access the
   value will fail. This simplifies error handling in applications.

   VALUE must consistent with the format of the current object value."
  ;; TODO: take a single argument
  (let* ((args (cons (bytestructure->pointer cursor) (formats->items (cursor-value-format cursor) value)))
         (signature (map (lambda _ POINTER) args))
         (function (ffi:pointer->procedure ffi:void (bytestructure-ref* cursor 'set-value) signature)))
    (apply function args)))

;;; other cursor procedures

(define-public (cursor-reset cursor)
  "Reset the position of the cursor. Any resources held by the cursor are released,
   and the cursor's key and position are no longer valid. A subsequent
   iteration with `cursor-next` will move to the first record, or with
   `cursor-prev` will move to the last record."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'reset) POINTER)))
    (check (function (bytestructure->pointer cursor)))))

(define-public (cursor-next? cursor)
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'next) POINTER)))
    (= 0 (function (bytestructure->pointer cursor)))))

(define-public (cursor-prev? cursor)
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'prev) POINTER)))
    (= 0 (function (bytestructure->pointer cursor)))))

(define-public (cursor-search? cursor)
  "On sucess move the cursor to the record matching the key. The key
   must first be set.

   To minimize cursor resources, the `cursor-reset` method should be
   called as soon as the record has been retrieved and the cursor no
   longer needs that position."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'search) POINTER)))
    (not (= %key-not-found (function (bytestructure->pointer cursor))))))

(define (->symbol v)
  (cond
   ((< v 0) 'before)
   ((= v 0) 'exact)
   ((> v 0) 'after)))

(define-public (cursor-search-near cursor)
  "Position CURSOR the record matching the key if it exists, or an adjacent record.
An adjacent record is either the smallest record larger than the key
or the largest record smaller than the key (in other words, a
logically adjacent key).  The key must first be set.

On success, the cursor ends positioned at the returned record; to minimize
cursor resources, the cursor-reset method should be called as soon as the record
has been retrieved and the cursor no longer needs that position.

Return `'before` if the key is positioned before the requested key,
`'exact` if it is positioned at the given key, `'after` if it is
positioned after the key, otherwise `#f`.
"
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'search-near) POINTER POINTER)))
    (let* ((integer (s32vector 0))
           (pointer (ffi:bytevector->pointer integer)))
      (let ((out (function (bytestructure->pointer cursor) pointer)))
        (case out
          ((0) (->symbol (s32vector-ref integer 0)))
          ((-31803) #f)
          (else (check out)))))))

(define-public (cursor-insert cursor)
  "Insert a record and optionally update an existing record."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'insert) POINTER)))
    (check (function (bytestructure->pointer cursor)))))

(define-public (cursor-update cursor)
  "Update a record and optionally insert an existing record."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'update) POINTER)))
    (check (function (bytestructure->pointer cursor)))))

(define-public (cursor-remove cursor)
  "Remove a record. The key must be set."
  (let ((function (pointer->procedure* ffi:int (bytestructure-ref* cursor 'remove) POINTER)))
    (check (function (bytestructure->pointer cursor)))))
