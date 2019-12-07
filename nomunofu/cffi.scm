(library (nomunofu cffi)

  (export
   %null-pointer
   alignof
   bytevector->pointer
   define-wrapped-pointer-type
   dereference-pointer
   double
   dynamic-func
   dynamic-link
   float
   int
   int16
   int32
   int64
   int8
   intptr_t
   long
   make-c-struct
   make-pointer
   null-pointer?
   parse-c-struct
   pointer->bytevector
   pointer->procedure
   pointer->scm
   pointer->string
   pointer-address
   pointer?
   procedure->pointer
   ptrdiff_t
   scm->pointer
   set-pointer-finalizer!
   short
   size_t
   sizeof
   ssize_t
   string->pointer
   uint16
   uint32
   uint64
   uint8
   uintptr_t
   unsigned-int
   unsigned-long
   unsigned-short
   void)

  (import (only (guile)
                dynamic-link
                dynamic-func)
          (system foreign)))
