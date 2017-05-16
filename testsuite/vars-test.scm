(test-begin "variables-and-patterns" 10)

(let ((foo (lambda ([x::integer ...]) (+ x ...))))
  (test-equal 9 (foo [2 3 4]))
  (test-equal 0 (foo []))
  (test-equal 6 (foo (list 4 2)))
  (test-error #t
              (display (foo [3 4.5 8]))))

(let ((foo (lambda ([x::long ...]) (+ x ...))))
  (test-equal 9 (foo [2 3 4]))
  (test-equal 0 (foo []))
  (test-equal 6 (foo (list 4 2)))
  (test-error #t
              (display (foo [3 "4" 8]))))

(let ((fun (lambda ([[x ...] ...] [y ...])
             [[(+ x y) ...] ...])))
  (test-equal
   [[101 102 103] [210 211 212]]
   (fun [[1 2 3] [10 11 12]] [100 200])))

(let ((A [3 4 5]))
  (test-equal [103 104 105]
              [(+ 100 (scan A)) ...]))

(test-end)
