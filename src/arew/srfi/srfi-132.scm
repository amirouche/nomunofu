(library (arew srfi srfi-132)
  (export
    list-sorted? vector-sorted? list-merge vector-merge list-sort vector-sort
    list-stable-sort vector-stable-sort list-merge! vector-merge! list-sort!
    vector-sort! list-stable-sort! vector-stable-sort!
    list-delete-neighbor-dups vector-delete-neighbor-dups
    list-delete-neighbor-dups! vector-delete-neighbor-dups! vector-find-median
    vector-find-median!)
  (import (except (rnrs) list-sort vector-sort vector-sort!)
          (rnrs mutable-pairs)
          (rename (only (arew srfi srfi-133) vector-copy! vector-copy)
                  (vector-copy! r7rs-vector-copy!)
                  (vector-copy r7rs-vector-copy))
          (only (rnrs r5rs) quotient)
          (arew srfi private include))
  (include/resolve ("arew" "srfi" "srfi-132") "delndups.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "lmsort.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "sortp.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "vector-util.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "vhsort.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "visort.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "vmsort.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "vqsort2.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "median.scm")
  (include/resolve ("arew" "srfi" "srfi-132") "sort.scm")  ; must be last
)
