(module-name (kawa arglist))

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)

(define-procedure arglist-key-count
  (lambda (args ::gnu.mapping.ArgList)::int (args:numKeywords))
  (lambda (args ::java.util.List)::int 0))

(define-procedure arglist-key-start
  (lambda (args ::gnu.mapping.ArgList)::int (args:firstKeyword))
  (lambda (args ::java.util.List)::int (args:size)))

(define-procedure arglist-arg-count
  (lambda (x ::gnu.mapping.ArgList)::int (x:numArguments))
  (lambda (x ::java.util.List)::int (x:size)))

(define-procedure arglist-key-ref
  (lambda (x ::gnu.mapping.ArgList i::int) (x:getKeyword i))
  (lambda (x ::java.util.List i::int)
    (primitive-throw (java.lang.IndexOutOfBoundsException))))

(define-procedure arglist-arg-ref
  (lambda (x ::gnu.mapping.ArgList i::int) (x:getArgAsObject i))
  (lambda (x ::java.util.List i::int) (x:get i)))

(define-procedure arglist-walk
  (lambda (args::gnu.mapping.ArgList proc)::void
          (let ((nargs (args:numArguments))
                (nkeys (args:numKeywords))
                (key0 (args:firstKeyword)))
            (do ((i ::int 0 (+ i 1))) ((= i nargs))
              (proc (args:getKeyword i) (args:getArgAsObject i)))))
  (lambda (args ::java.util.List proc)
    (for-each (lambda (v) (proc #!null v)) args)))
