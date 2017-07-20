(test-begin "variables-and-patterns" 15)

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

;; simple guards
(let ((fun (lambda (x #!if (> x 0) y) (list x y))))
  (test-equal '(3 4) (fun 3 4)))
(let ((fun (lambda (x y #!if (> x y)) (list x y))))
  (test-equal '(4 2) (fun 4 2))
  (test-equal 'caught
              (try-catch
               (fun 3 4)
               (ex java.lang.IllegalArgumentException
                   'caught))))

;; #!if guard after #!rest
(let ((fun (lambda (x y #!rest r #!if (> x y)) (list x y r))))
  (test-equal '(4 2 (a b c)) (fun 4 2 'a 'b 'c))
  (test-equal 'caught
              (try-catch
               (fun 3 4)
               (ex java.lang.IllegalArgumentException
                   'caught))))

#| Not working yet
(let* ((fun (lambda ([[x y] ...]) [[y x] ...]))
       (A [[11 12] [21 22] [31 32]]))
  (test-equal [[12 11] [22 21] [32 31]]
              (fun A)))
|#

(test-end)
