(module-compile-options warn-as-error: #t)
(let ((abc 3)
      (abc 4))
  (list abc))
;; Diagnostic: errors1.scm:3:8: duplicate declaration of 'abc'
;; Diagnostic: errors1.scm:2:8: (this is the previous declaration of 'abc')
