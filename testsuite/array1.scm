;; Kawa-options: "-f" %F

(define (foo a ::array) (a 0))
(format #t "foo ~w~%" (foo #(a b c)))
;; Output: foo a

(define (foo1 a ::array1) (a 0))
(format #t "foo1 ~w~%" (foo1 #(a b c)))
;; Output: foo1 a
