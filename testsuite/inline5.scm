;; GitLab issue #29 "ArrayIndexOutOfBoundsException in inlineCall"
(format #t "t1: ~w~%"
        ((lambda (#!optional x #!rest y) #f) 1))
;; Output: t1: #f

(format #t "t2: ~w~%"
        ((lambda (#!optional (x -1) #!rest y) (list x y))))
;; Output: t2: (-1 ())
(format #t "t3: ~w~%"
        ((lambda (#!optional (x -1) #!rest y) (list x y)) 1))
;; Output: t3: (1 ())
(format #t "t4: ~w~%"
        ((lambda (#!optional (x -1) #!rest y) (list x y)) 1 2))
;; Output: t4: (1 (2))
(format #t "t5: ~w~%"
        ((lambda (#!optional x (y -1) (z -2) #!rest r) (list x y z r)) 1))
;; Output: t5: (1 -1 -2 ())
(format #t "t6: ~w~%"
        ((lambda (#!optional x (y -1) (z -2) #!rest r) (list x y z r)) 1 2))
;; Output: t6: (1 2 -2 ())
(format #t "t7: ~w~%"
        ((lambda (#!optional x (y -1) (z -2) #!rest r) (list x y z r)) 1 2 3))
;; Output: t7: (1 2 3 ())
(format #t "t8: ~w~%"
        ((lambda (#!optional x (y -1) (z -2) #!rest r) (list x y z r)) 1 2 3 4))
;; Output: t8: (1 2 3 (4))
(format #t "t9: ~w~%"
        ((lambda (#!optional x (y -1) (z -2)) (list x y z)) 1))
;; Output: t9: (1 -1 -2)
(format #t "t10: ~w~%"
        ((lambda (#!optional x (y -1) (z (+ y 100) z-seen)) (list x y z z-seen)) 1 2))
;; Output: t10: (1 2 102 #f)
(format #t "t11: ~w~%"
        ((lambda (#!optional x (y -1) (z -2 z-seen)) (list x y z z-seen)) 1 2 3))
;; Output: t11: (1 2 3 #t)
