(define (\1- x) (invoke-static 'gnu.jemacs.lang.AddOp 'MINUS x 1))

(define (\1+ x) (invoke-static 'gnu.jemacs.lang.AddOp 'PLUS x 1))

(define (% x y)
  (invoke-static 'integer 'remainder x y))
