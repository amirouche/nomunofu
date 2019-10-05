(library (arew srfi srfi-54)
  (export cat)
  (import (except (rnrs) error)
          (rnrs r5rs)
          (arew srfi srfi-23)
          (arew srfi private include))

  (include/resolve ("srfi" "%3a54") "srfi-54-impl.scm"))
