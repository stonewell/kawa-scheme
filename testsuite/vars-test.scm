(test-begin "variables-and-patterns" 4)

(let ((foo (lambda ([x::integer ...]) (+ x ...))))
  (test-equal 9 (foo [2 3 4]))
  (test-equal 0 (foo []))
  (test-equal 6 (foo (list 4 2)))
  (test-error #t
   (display (foo [3 4.5 8]))))

(test-end)
