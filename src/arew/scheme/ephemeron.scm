(library (arew scheme ephemeron)
  (export ephemeron?
          make-ephemeron
          ephemeron-broken?
          ephemeron-key
          ephemeron-value)
  (import (arew srfi srfi-124)))
