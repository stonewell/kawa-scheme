(define (make-named-array name)
  (object (java.util.ArrayList)
          ((*init*)
           (invoke-special <java.util.ArrayList> (this) '*init* 100))
          ((toString)
           (format #f "ArrayList/~a" name))))
(define al (make-named-array "blue"))
(format #t "make-naked-array: ~a~%" (al:toString))
;; Output: make-naked-array: ArrayList/blue
