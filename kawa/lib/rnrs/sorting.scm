(module-name <kawa.lib.rnrs.sorting>)
(module-export list-sort vector-sort vector-sort!)
;(import (kawa lib srfi95))
(import (scheme base)
        (kawa lib srfi 95 "kawa/lib/srfi/95.scm"))
;(require <kawa.lib.srfi.95>)

(define (list-sort less? list)
  (%sort-list (append list '()) less? #f))

(define (vector-sort less? seq)
  (%sort-vector seq less? #f))

(define (vector-sort! proc vector) :: void
  (%vector-sort! vector proc #f))
