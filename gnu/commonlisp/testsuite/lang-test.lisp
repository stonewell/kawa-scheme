(test-init "Common Lisp tests" 150)

(setq y 100)
(defun foo1 (x)
  (lambda (y)
    (/ x y)))
(defvar foo12 (foo1 12))
(test 4 'lexical-test-1 (apply foo12 '(3)))

(defvar xx 20)
(defun xx (a) (+ xx a))
(test 24 'separate-names-1 (xx 4))

;(test t 'eq-test-1 (eq t 't))

(test t 'equal-test-1 (equal "tt" "tt"))
(test nil 'equal-test-2 (equal "tt" "tt "))

(test "The octal value of 18 is 22,
   and the hex value is 12."
      'format-test-1
      (format "The octal value of ~d is ~o,
   and the hex value is ~x." 18 18 18))

(defmacro list-1 (bar) `(list ,bar))
(test '(12) 'defmacro (list-1 12))

(test '(3 4) 'list-1 (list 3 4))
(test nil 'functionp-1 (functionp 'list))
(test t 'functionp-2 (functionp #'list))
(test t 'functionp-3 (functionp (function list)))
(test '(3 4) 'function-1 ((function list) 3 4))

(test 6 'flet-1 (flet ((flet1 (n) (+ n n)))
                  (flet ((flet1 (n) (+ 2 (flet1 n))))
                    (flet1 2))))

(defun dummy-function () 'top-level)

(test 'shadow 'flet-2 (flet ((dummy-function () 'shadow))
                        (funcall #'dummy-function)))

(test 'top-level 'funcall-3 (funcall #'dummy-function))

(test 'shadow 'flet-2 (flet ((dummy-function () 'shadow))
                        (funcall #'dummy-function)))

(test t 'flet-3 (eq (funcall #'dummy-function) (funcall 'dummy-function)))
(test '() 'flet-4 (flet ((dummy-function () 'shadow))
		    (eq (funcall #'dummy-function)
			(funcall 'dummy-function))))

;; # is a non-terminating macro character in Common Lisp.
(test '(|a#com#b|) 'sharp-in-token '(a#|com|#b))

(test nil 'car-1 (car nil))
(test 1 'car-2 (car '(1 . 2)))
(test nil 'cdr-1 (cdr nil))
(test 2 'cdr-2 (cdr '(1 . 2)))
(test nil 'first-1 (first nil))
(test 1 'first-2 (first '(1 . 2)))
(test nil 'rest-1 (rest nil))
(test 2 'rest-2 (rest '(1 . 2)))
(test 'foo 'nth-1 (nth 0 '(foo bar baz)))
(test 'bar 'nth-2 (nth 1 '(foo bar baz)))
(test nil 'nth-3 (nth 3 '(foo bar baz)))
(test nil 'nthcdr-1 (nthcdr 0 '()))
(test nil 'nthcdr-2 (nthcdr 3 '()))
(test '(a b c) 'nthcdr-3 (nthcdr 0 '(a b c)))
(test '(c) 'nthcdr-4 (nthcdr 2 '(a b c)))
(test '() 'nthcdr-5 (nthcdr 4 '(a b c)))
(test 1 'nthcdr-6 (nthcdr 1 '(0 . 1)))

(defvar alist '())
(test '((1 . "one")) 'acons-1 (acons 1 "one" alist))
(test nil 'acons-2 alist)
(test '((1 . "one") (2 . "two"))
      'acons-3
      (setq alist (acons 1 "one" (acons 2 "two" alist))))
(test '(1 . "one")
      'acons-4
      (assoc 1 alist))
(test '((1 . "uno") (1 . "one") (2 . "two"))
      'acons-5
      (setq alist (acons 1 "uno" alist)))
(test '(1 . "uno")
      'assoc-6
      (assoc 1 alist))

(test t 'listp-1 (listp nil))
(test t 'listp-2 (listp (cons 1 2)))
(test nil 'listp-3 (listp t))

(test t 'numberp-1 (numberp 12))
(test t 'numberp-2 (numberp (expt 2 130)))
(test nil 'numberp-3 (numberp nil))
(test nil 'numberp-4 (numberp (cons 1 2)))

(setq fail-expected "zerop returns Scheme boolean type")
(test t 'zerop-1 (zerop 0))
(setq fail-expected "zerop returns Scheme boolean type")
(test nil 'zerop-2 (zerop 1))
(setq fail-expected "zerop returns Scheme boolean type")
(test t 'zerop-3 (zerop -0.0))
(setq fail-expected "zerop returns Scheme boolean type")
(test t 'zerop-4 (zerop 0/100))

(test nil 'consp-1 (consp nil))
(test t 'consp-2 (consp (cons 1 2)))

(test t 'atomp-1 (atom 'sss))
(test nil 'atomp-2 (atom (cons 1 2)))
(test t 'atomp-3 (atom nil))
(test t 'atomp-4 (atom '()))
(test t 'atomp-5 (atom 3))

(test nil 'eql-1 (eql 'a 'b))
(test t 'eql-2 (eql 'a 'a))
(test t 'eql-3 (eql 3 3))
(test nil 'eql-4 (eql 3 3.0))
(test t 'eql-5 (eql 3.0 3.0))
(test nil 'eql-6 (eql (cons 'a 'b) (cons 'a 'c)))
(test nil 'eql-7 (eql (cons 'a 'b) (cons 'a 'b)))
(test t 'eql-8 (eql #\A #\A))
(test nil 'eql-9 (eql "Foo" "FOO"))
(test t 'eql-10 (let ((x (cons 'a 'b))) (eql x x)))
(test t 'eql-11 (let ((x '(a . b))) (eql x x)))

; BUG! Using Scheme booleans (via zerop).
;(test t 'complement-1 (funcall (complement #'zerop) 1))
(test nil 'complement-2 (funcall (complement #'member) 'a '(a b c)))
(test t 'complement-3 (funcall (complement #'member) 'd '(a b c)))

(test '(2 3) 'member-1 (member 2 '(1 2 3)))
(test '((3 . 4)) 'member-2
      (member 2 '((1 . 2) (3 . 4))
	      :test-not #'=
	      :key #'cdr))
(test nil 'member-3 (member 'e '(a b c d)))

(defvar f '+)
(test 3 'apply-1 (apply f '(1 2)))
(setq f #'-)
(test -1 'apply-2 (apply f '(1 2)))
(test 7 'apply-3 (apply #'max 3 5 '(2 7 3)))
(test '((+ 2 3) . 4) 'apply-4 (apply 'cons '((+ 2 3) 4)))
(test 0 'apply-5 (apply #'+ '()))

(defun recursive-times (k n)
  (labels ((temp (n)
	     (if (zerop n) 0 (+ k (temp (1- n))))))
    (temp n)))

(test 6 'labels-1 (recursive-times 2 3))

(multiple-value-bind (f r) (floor 3/2)
  (test '(1 1/2) 'floor-1 (list f r)))
(multiple-value-bind (f r) (floor 3 2)
  (test '(1 1) 'floor-2 (list f r)))
(multiple-value-bind (f r) (floor 5 2)
  (test '(2 1) 'floor-3 (list f r)))
(multiple-value-bind (f r) (floor (/ 5 2))
  (test '(2 1/2) 'floor-4 (list f r)))

(test '(1) 'multiple-value-bind-1
      (multiple-value-bind (x y z) (values 1 2 3) (list x)))
(test '(11 9) 'multiple-value-bind-2
      (multiple-value-bind (f r)
	  (floor 130 11)
	(list f r)))

(test 't 'read-symbol-with-package-1 'COMMON-LISP:t)
(test 't 'read-symbol-with-package-2 'CL:t)
(test ':abc 'read-symbol-with-package-3 'KEYWORD:abc)

(test t 'keywordp-1 (keywordp ':test))
(test nil 'keywordp-2 (keywordp 'CL:symbol))
(test nil 'keywordp-3 (keywordp 123))
(test t 'keywordp-4 (keywordp 'KEYWORD:test))

(test t 'symbol-package-1 (not (null (symbol-package nil))))
(test t 'symbol-package-2 (not (null (symbol-package ':test))))
(test t 'symbol-package-3 (not (eq (symbol-package nil)
				   (symbol-package ':test))))

(test t 'symbols-are-interned-in-current-package
      (eq (symbol-package 'some-new-sym)
	  *package*))

(test t '*package*-exists (boundp '*package*))

(test t 'packagep-1 (packagep *package*))
(test nil 'packagep-2 (packagep nil))
(test t 'packagep-3 (packagep (symbol-package nil)))
(test t 'packagep-4 (packagep (symbol-package ':x)))
(test t 'packagep-5 (packagep (symbol-package 'KAWA:car)))
(test nil 'packagep-6 (packagep "foo"))
(test nil 'packagep-7 (packagep 123))

(test nil 'find-package-1 (packagep (find-package "not-a-package")))
(test t 'find-package-2 (packagep (find-package "CL")))
(test t 'find-package-3 (packagep (find-package '|CL|)))
(test nil 'find-package-4 (packagep (find-package #\x)))
(test t 'find-package-5 (packagep (find-package "KEYWORD")))
(test t 'find-package-6 (eq (find-package *package*) *package*))

;; Kawa's EmptyNamespace doesn't count as a package
(test nil 'empty-namespace-is-not-a-package (find-package ""))

(test t 'intern-1 (symbolp (intern "foo")))
(test t 'intern-2 (keywordp (intern "foo" (symbol-package ':x))))
(test nil 'intern-3 (intern (symbol-name 'nil) (symbol-package 'nil)))
(test t 'intern-4 (intern (symbol-name 't) (symbol-package 't)))
(test ':FOO 'intern-5 (intern "FOO" ':KEYWORD))

(test '(1) 'multiple-value-list-one (multiple-value-list 1))
(test '(1 2) 'multiple-value-list-two (multiple-value-list (values 1 2)))
(test '() 'multiple-value-list-zero (multiple-value-list (values)))

(test 1 'nth-value-1 (nth-value 1 (values 0 1 2 3)))
(test nil 'nth-value-10 (nth-value 10 (values 0 1 2 3)))
(test 0 'nth-value-0 (nth-value 0 0))
(test nil 'nth-value-2 (nth-value 2 0))

(test '(car :INHERITED) 'find-symbol-car
      (multiple-value-list (find-symbol "car")))
(test '(nil nil) 'find-symbol-not-interned
      (multiple-value-list (find-symbol "not interned")))
(test '(car :EXTERNAL) 'find-symbol-kawa
      (multiple-value-list (find-symbol "car" "KAWA")))
(test '(:foo :EXTERNAL) 'find-symbol-keyword
      (multiple-value-list (find-symbol "foo" "KEYWORD")))

(test t 'keyword-package-in-sync-with-keyword-namespace
      (let* ((name "a-fresh-keyword")
	     (pkg "KEYWORD")
	     (found? (nth-value 1 (find-symbol name pkg)))
	     (s1 (intern name pkg))
	     (s2 (invoke-static "gnu.expr.Keyword" "make" (as "String" name))))
	(and (not found?)
	     (eq s1 s2))))

(test t 'kawa-package-in-sync-with-empty-namespace
      (let* ((name "a-fresh-kawa-symbol")
	     (pkg "KAWA")
	     (found? (nth-value 1 (find-symbol name pkg)))
	     (s1 (intern name pkg))
	     (s2 (invoke-static "gnu.mapping.Symbol" "valueOf"
				(as "String" name))))
	(and (not found?)
	     (eq s1 s2))))

(test t '*features*-variable-exists (boundp '*features*))
(test t '*features*-is-a-list (listp *features*))
(test ':kawa '*features*-contains-kawa (car (member ':kawa *features*)))
(test ':common-lisp '*features*-contains-common-lisp
      (car (member ':common-lisp *features*)))

(test '(1 2) 'sharp-plus-reader-1 (list 1 #+kawa 2))
(test '(1) 'sharp-plus-reader-2 (list 1 #+not-a-feature 2))
(test '(1 2) 'sharp-plus-reader-3 (list 1 #+common-lisp 2))
(test '(1) 'sharp-minus-reader-1 (list 1 #-kawa 2))
(test '(1 2) 'sharp-minus-reader-2 (list 1 #-not-a-feature 2))
(test '(1 3) 'sharp-plus-or-empty (list 1 #+(or) 2 3))
(test '(1 3) 'sharp-plus-or-false (list 1 #+(or not-a-feature) 2 3))
(test '(1 2 3) 'sharp-plus-or-some (list 1 #+(or not-a-feature kawa) 2 3))
(test '(1 2 3) 'sharp-plus-and-empty (list 1 #+(and) 2 3))
(test '(1 3) 'sharp-plus-and-false (list 1 #+(and not-a-feature) 2 3))
(test '(1 3) 'sharp-plus-and-some (list 1 #+(and not-a-feature kawa) 2 3))
(test '(1 2 3) 'sharp-plus-and-all (list 1 #+(and kawa kawa) 2 3))
(test '(1 3) 'sharp-plus-not (list 1 #+(not kawa) 2 3))
(test '(1 2 3) 'sharp-plus-not (list 1 #+(not not-a-feature) 2 3))
(test '(1 3) 'sharp-plus-nested (list 1 #+(not (or kawa sbcl)) 2 3))

(test 't 'make-package-1 (packagep (make-package "p1")))
(test 't 'make-package-2 (packagep (make-package "p2" :use ':CL)))

(test ':EXTERNAL 'export-1
      (let* ((pkg (make-package "p3"))
	     (name "TEMP-SYM")
	     (sym (intern name pkg)))
	(export sym pkg)
	(nth-value 1 (find-symbol name pkg))))

(test '() 'test-mapacar-1 (mapcar #'1+ ()))
(test '(2) 'test-mapacar-2 (mapcar #'1+ '(1)))
(test '(2 3) 'test-mapacar-4 (mapcar #'1+ '(1 2)))

(test nil 'unless-1 (unless nil))
(test nil 'unless-2 (unless t))
(test 3 'unless-3 (unless nil 1 2 3))
(test nil 'unless-4 (unless t 1 2 3))

(test nil 'when-1 (when nil))
(test nil 'when-2 (when t))
(test nil 'when-3 (when nil 1 2 3))
(test 3 'when-4 (when t 1 2 3))

(test nil 'progn-1 (progn))
(test 3 'progn-2 (progn 1 2 3))
