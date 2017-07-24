;; This implements SRFI-39 "Parameter objects".
(module-export make-parameter as-location%)

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)

(define (make-parameter init #!optional (converter #!null))
  (if (not (eq? converter #!null))
      (set! init (converter init)))
  (let* ((loc (gnu.mapping.ThreadLocation:new))
         (conv ::gnu.mapping.Procedure
               (if (or (eq? converter #!null)
                       (gnu.mapping.Procedure? converter))
                   converter
                   (lambda (x) (converter x)))))
    (invoke loc 'setGlobal init)
    (gnu.mapping.LocationProc:new loc conv)))

(define (as-location% param) :: <gnu.mapping.Location>
  (if (instance? param <gnu.mapping.LocationProc>)
      (gnu.mapping.LocationProc:getLocation param)
      (as <gnu.mapping.Location> param)))
