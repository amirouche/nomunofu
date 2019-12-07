(define-module (nomunofu app))

(import (scheme base))


(define-record-type <app>
  (make-app engine okvs ustore nstore)
  app?
  (engine app-engine)
  (okvs app-okvs)
  (ustore app-ustore)
  (nstore app-nstore))

(export make-app)
(export app?)
(export app-engine)
(export app-okvs)
(export app-ustore)
(export app-nstore)
