(define-syntax define-a
  (lambda (stx)
    (syntax-case stx ()
      ((_ name) #`(define name (lambda () #f))))))

(define-syntax define-bar
  (lambda (stx)
    (syntax-case stx ()
      ((_ name) #`(define name (lambda () name: 'bar #f))))))

(define-syntax define-c
  (lambda (stx)
    (syntax-case stx ()
      ((_ n) #`(define n (letrec ((foo (lambda () name: 'n
                                               #f)))
                           foo))))))

(define-syntax define-d
  (lambda (stx)
    (syntax-case stx ()
      ((_ n) #`(define n (letrec ((foo (lambda () #f)))
                                 (set-procedure-property! foo 'name 'n)
                                 foo))))))

(define e (letrec ((foo (lambda () #f))) foo))

(define-a a)
(format #t "Name of a: ~A~%" (procedure-property a 'name))
;; Output: Name of a: a
(define-bar b)
(format #t "Name of b: ~A~%" (procedure-property b 'name))
;; Output: Name of b: bar
(define-c c)
(format #t "Name of c: ~A~%" (procedure-property c 'name))
;; Output: Name of c: c
(define-d d)
(format #t "Name of d: ~A~%" (procedure-property d 'name))
;; Output: Name of d: d
(format #t "Name of e: ~A~%" (procedure-property e 'name))
;; Output: Name of e: foo

(define-simple-class kittens ()
  (a::long))

(define (kittens-class)
  kittens:class)

(format #t "kittens.class: ~w~%" (kittens-class))
;; Output: kittens.class: class kittens
