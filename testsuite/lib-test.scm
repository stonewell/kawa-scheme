(test-begin "libs" 32)

(import (srfi :2 and-let*))

(test-equal 1 (and-let* () 1))
(test-equal 2 (and-let* () 1 2))
(test-equal #t (and-let* ()))

(test-equal #f (let ((x #f)) (and-let* (x))))
(test-equal 1 (let ((x 1)) (and-let* (x))))
(test-equal #f (and-let* ((x #f)) ))
(test-equal 1  (and-let* ((x 1)) ))
(test-error (eval '(and-let* ( #f (x 1)))))
(test-equal #f (and-let* ( (#f) (x 1)) ))
(test-error (eval '(and-let* (2 (x 1)))))
(test-equal 1 (and-let* ( (2) (x 1)) ))
(test-equal 2 (and-let* ( (x 1) (2)) ))
(test-equal #f (let ((x #f)) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x)  )))
(test-equal 2 (let ((x 1)) (and-let* (x) (+ x 1))))
(define xf #f)
(test-equal #f (and-let* (xf) (+ xf 1)))
(test-equal 2 (let ((x 1)) (and-let* (((positive? x))) (+ x 1))))
(test-equal #t (let ((x 1)) (and-let* (((positive? x))) )))
(test-equal #f (let ((x 0)) (and-let* (((positive? x))) (+ x 1))))
(test-equal 3 (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1))) )
;(must-be-a-syntax-error
;  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
;)

(test-equal 2 (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal 2 (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))
(test-equal #f (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal #f (and-let* (xf ((positive? xf))) (+ xf 1)))
(test-equal #f (and-let* (((begin xf)) ((positive? xf))) (+ xf 1)))

(test-equal #f  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f (and-let* (xf (y (- xf 1)) ((positive? y))) (/ xf y)))
(test-equal 3/2  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))

(define (symbol-parts s::symbol)
  (list (symbol-local-name s) (symbol-namespace-uri s) (symbol-prefix s)))

(test-equal '("abc:def" "" "")
	    (symbol-parts '|abc:def|))
;(test-equal '("abc:def" "" "")
;	    (symbol-parts 'abc:def))

(require 'xml)

(test-equal '("abc" "URI" "")
	    (symbol-parts (element-name #<abc xmlns="URI"/>)))

(test-end)
