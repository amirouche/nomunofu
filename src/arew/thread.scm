(library (arew thread)

  (export thread-index)

  (import (scheme base))

  (define thread-index (make-parameter #vu8(255 255))))
