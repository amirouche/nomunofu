(library (nomunofu app)

  (export make-app
          app?
          app-engine
          app-okvs
          app-ustore
          app-nstore)

  (import (scheme base))


  (define-record-type <app>
    (make-app engine okvs ustore nstore)
    app?
    (engine app-engine)
    (okvs app-okvs)
    (ustore app-ustore)
    (nstore app-nstore)))
