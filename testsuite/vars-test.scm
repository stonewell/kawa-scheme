(test-begin "variables-and-patterns" 21)

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

(let* ((fun (lambda ([[x y] ...]) [[y x] ...]))
       (A [[11 12] [21 22] [31 32]])
       (B [[11 12] [21 22 23] [31 32]]))
  (test-equal [[12 11] [22 21] [32 31]]
              (fun A))
  (test-error (fun B)))

(let* ((fun (lambda ([[x @r] ...])
              [[@r (+ 100 x)] ...]))
       (A [[11] [21 22 23 24] [31 32]])
       (B [[] [21 22 23 24] [31 32]]))
  (test-equal [[111] [22 23 24 121] [32 131]]
              (fun A))
  (test-error (fun B)))

(let* ((fun (lambda ([[x ... y] ...])
              [[(+ 100 y) (+ 200 x) ...] ...]))
       (A [[11] [21 22 23 24] [31 32]]))
  (test-equal [[111] [124 221 222 223] [132 231]]
              (fun A)))

(let* ((fun (lambda ([[@r x] ...])
              [[@r (+ 100 x)] ...]))
       (A [[11] [21 22 23 24] [31 32]]))
  (test-equal [[111] [21 22 23 124] [31 132]]
              (fun A)))

(test-end)
