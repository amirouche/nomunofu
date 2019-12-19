(library (arew scheme sort)
  (export
   list-sorted? vector-sorted? list-merge vector-merge list-sort vector-sort
   list-stable-sort vector-stable-sort list-merge! vector-merge! list-sort!
   vector-sort! list-stable-sort! vector-stable-sort!
   list-delete-neighbor-dups vector-delete-neighbor-dups
   list-delete-neighbor-dups! vector-delete-neighbor-dups! vector-find-median
   vector-find-median!)
  (import (arew srfi srfi-132)))
