(define (hide x) (cdr (cons x x)))
(define (f1 x y #!optional a b (c 8 c-supplied))
  (format #t "f1 x:~w y:~w a:~w b:~w c:~w c-supplied:~w~%"
          x y a b c c-supplied))
(f1 3 4)
((hide f1) 3 4)
;; Output: f1 x:3 y:4 a:#f b:#f c:8 c-supplied:#f
;; Output: f1 x:3 y:4 a:#f b:#f c:8 c-supplied:#f
(f1 8 7 6 5 4)
((hide f1) 8 7 6 5 4)
;; Output: f1 x:8 y:7 a:6 b:5 c:4 c-supplied:#t
;; Output: f1 x:8 y:7 a:6 b:5 c:4 c-supplied:#t
(define (f2 x y #!optional a b (c -1 c-supplied) #!rest r)
  (format #t "f2 x:~w y:~w a:~w b:~w c:~w c-supplied:~w rr:~w~%"
          x y a b c c-supplied r))
(f2 3 4)
((hide f2) 3 4)
;; Output: f2 x:3 y:4 a:#f b:#f c:-1 c-supplied:#f rr:()
;; Output: f2 x:3 y:4 a:#f b:#f c:-1 c-supplied:#f rr:()

(f2 8 7 6 5 4 3 2 1)
((hide f2) 8 7 6 5 4 3 2 1)
;; Output: f2 x:8 y:7 a:6 b:5 c:4 c-supplied:#t rr:(3 2 1)
;; Output: f2 x:8 y:7 a:6 b:5 c:4 c-supplied:#t rr:(3 2 1)
(define (f3 x y #!optional a b #!key k1 k2)
  (format #t "f3 x:~w y:~w a:~w b:~w k1:~w k2:~w~%"
          x y a b k1 k2))
(f3 10 12 13)
;; Output: f3 x:10 y:12 a:13 b:#f k1:#f k2:#f
(f3 10 12 13 14)
;; Output: f3 x:10 y:12 a:13 b:14 k1:#f k2:#f
(f3 10 12 13 14 k2: 100)
;; Output: f3 x:10 y:12 a:13 b:14 k1:#f k2:100

(define (f4 x y #!optional a b #!key k1 k2 #!rest r)
  (format #t "f4 x:~w y:~w a:~w b:~w k1:~w k2:~w r:~w~%"
          x y a b k1 k2 r))
(f4 10 12 13)
;; Output: f4 x:10 y:12 a:13 b:#f k1:#f k2:#f r:()
(f4 10 12 13 14 k1: 99)
;; Output: f4 x:10 y:12 a:13 b:14 k1:99 k2:#f r:()
(f4 10 12 13 14 k2: 100 'a 'b 'c)
;; Output: f4 x:10 y:12 a:13 b:14 k1:#f k2:100 r:(a b c)

(define (f5 x y #!optional a b #!rest r #!key k1 k2)
  (format #t "f5 x:~w y:~w a:~w b:~w k1:~w k2:~w r:~w~%"
          x y a b k1 k2 r))
(f5 10 12 13)
;; Output: f5 x:10 y:12 a:13 b:#f k1:#f k2:#f r:()
(f5 10 12 13 14 k1: 99)
;; Output: f5 x:10 y:12 a:13 b:14 k1:99 k2:#f r:(k1: 99)
(f5 10 12 13 14 k2: 100 'a 'b 'c)
;; Output: f5 x:10 y:12 a:13 b:14 k1:#f k2:100 r:(k2: 100 a b c)

(define (f6 x y #!optional a (b 100 b-supplied) #!rest r #!key k1 (k2 "2" k2-supplied))
  (format #t "f6 x:~w y:~w a:~w b:~w (supplied:~w) k1:~w k2:~w(supplied:~w) r:~w~%"
          x y a b b-supplied k1 k2 k2-supplied r))
(f6 10 12 13)
;; Output: f6 x:10 y:12 a:13 b:100 (supplied:#f) k1:#f k2:"2"(supplied:#f) r:()
(f6 10 12 13 14 k2: 100 'a 'b)
;; Output: f6 x:10 y:12 a:13 b:14 (supplied:#t) k1:#f k2:100(supplied:#t) r:(k2: 100 a b)

(define (f7 #!optional (x 'a) (y 'b y-p) (z 'c z-p))
  (format #t "f7 x:~w y:~w y-p:~w z:~w z-p:~w~%" x y y-p z z-p))
(f7 1 2)
;; Output: f7 x:1 y:2 y-p:#t z:c z-p:#f
(f7)
;; Output: f7 x:a y:b y-p:#f z:c z-p:#f

(define (f8 #!optional
            (x (begin (format #t "f8-default-x~%") 'a))
            (y (begin (format #t "f8-default-y~%") 'b))
            (z (begin (format #t "f8-default-z~%") 'c) z-p))
  (format #t "f8 x:~w y:~w z:~w z-p:~w~%" x y z z-p))
(f8)
;; Output: f8-default-x
;; Output: f8-default-y
;; Output: f8-default-z
;; Output: f8 x:a y:b z:c z-p:#f
(f8 1 2)
;; Output: f8-default-z
;; Output: f8 x:1 y:2 z:c z-p:#f
