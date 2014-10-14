(module-name (scheme base))

(require kawa.lib.bytevectors)
(require kawa.lib.case_syntax)
(require kawa.lib.characters)
(require kawa.lib.exceptions)
(require kawa.lib.lists)
(require kawa.lib.misc)
(require kawa.lib.numbers)
(require kawa.lib.parameters)
(require kawa.lib.parameterize)
(require kawa.lib.ports)
(require kawa.lib.prim_syntax)
(require kawa.lib.strings)
(require kawa.lib.std_syntax)
(require kawa.lib.syntax)
(require kawa.lib.vectors)
(require kawa.lib.misc_syntax) ;; FIXME only for deprecated include
(require kawa.lib.DefineRecordType)
;; Sorted by classname
(import (only (gnu kawa functions AddOp) + -))
(import (rename (only (gnu kawa functions CallCC) callcc)
                (callcc call-with-current-continuation)))
(import (rename (only (gnu kawa functions CallCC) callcc)
                (callcc call/cc)))
(import (rename (only (gnu kawa functions DivideOp)
                      modulo floorQuotient quotient remainder)
                (floorQuotient floor-quotient)
                (modulo floor-remainder)
                (quotient truncate-quotient)
                (remainder truncate-remainder)))
(import (only (gnu kawa functions DivideOp) / modulo quotient remainder))
(import (only (gnu kawa functions MultiplyOp) *))
(import (rename (only (gnu kawa lispexpr LangObjType)
                      listType stringType vectorType u8vectorType)
                (listType list) (stringType string) (vectorType vector)
                (u8vectorType bytevector)))
(import (rename (only (kawa lang Quote) plainQuote quasiQuote)
                (plainQuote quote) (quasiQuote quasiquote)))
(import (rename (only (kawa lib prim_syntax) letrec) (letrec letrec*)))
(import (only (kawa standard append) append))
(import (only (kawa standard begin) begin))
(import (rename (only (kawa standard call_with_values) callWithValues)
                (callWithValues call-with-values)))
(import (only (kawa standard expt) expt))
(import (rename (only (kawa standard IfFeature) condExpand)
                (condExpand cond-expand)))
(import (rename (only (kawa standard Include) include includeCi)
                (includeCi include-ci)))
(import (rename (only (kawa standard let_syntax) let_syntax letrec_syntax)
                (let_syntax let-syntax) (letrec_syntax letrec-syntax)))
(import (rename (only (kawa standard Scheme)
                      apply forEach isOdd isEven isEq isEqv isEqual
                      numEqu numGrt numGEq numLss numLEq map not)
                (forEach for-each)
                (isOdd odd?)
                (isEven even?)
                (isEq eq?) (isEqv eqv?) (isEqual equal?)
                (numEqu =) (numGrt >) (numGEq >=) (numLss <) (numLEq <=)))
(import (only (kawa standard SchemeCompilation) lambda))
(import (rename (only (kawa standard set_b) set) (set set!)))
(import (rename (only (kawa standard syntax_error) syntax_error)
                (syntax_error syntax-error)))
(import (rename (only (kawa standard syntax_rules) syntax_rules)
                (syntax_rules syntax-rules)))
(import (rename (only (kawa standard vector_append) vectorAppend)
                (vectorAppend vector-append)))

#| Special unbound symbols:
(export
  ... => _ else unquote unquote-splicing)
|#
(export
 * + - / < <= = > >=
 abs and append apply assoc assq assv
 begin binary-port? boolean=? boolean?
 bytevector bytevector-append bytevector-copy bytevector-copy!
 bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector?
 caar cadr call-with-current-continuation call-with-port call-with-values
 call/cc car case cdar cddr cdr ceiling char->integer char-ready?
 char<=? char<? char=? char>=? char>? char?
 close-input-port close-output-port close-port complex? cond cond-expand
 cons current-error-port current-input-port current-output-port
 define define-record-type define-syntax define-values denominator do
 dynamic-wind
 eof-object? eq? equal? eqv? error
 error-object-irritants error-object-message error-object? even?
 exact exact-integer-sqrt exact-integer? exact? expt
 features file-error? floor floor-quotient floor-remainder floor/
 flush-output-port for-each
 gcd get-output-bytevector get-output-string guard
 if include include-ci inexact
 inexact? input-port-open? input-port? integer->char integer?
 lambda lcm length let let* let*-values let-syntax let-values letrec letrec*
 letrec-syntax list list->string list->vector list-copy list-ref list-set!
 list-tail list?
 make-bytevector make-list make-parameter make-string make-vector map max
 member memq memv min modulo
 negative? newline not null? number->string number? numerator
 odd? open-input-bytevector open-input-string open-output-bytevector
 open-output-string or output-port? output-port-open?
 pair? parameterize peek-char peek-u8 port? positive? procedure?
 quasiquote quote quotient
 raise raise-continuable rational? rationalize read-bytevector read-bytevector!
 read-char read-error? read-line read-string read-u8
 real? remainder reverse round
 set! set-car! set-cdr! square string string->list string->number
 string->symbol string->utf8 string->vector string-append string-copy
 string-copy! string-fill! string-for-each string-length string-map string-ref
 string-set! string<=? string<? string=? string>=? string>? string? substring
 symbol->string symbol=? symbol? syntax-error syntax-rules
 textual-port? truncate truncate-quotient truncate-remainder truncate/
 u8-ready? unless utf8->string
 values vector vector->list vector->string vector-append vector-copy
 vector-copy! vector-fill! vector-for-each vector-length vector-map
 vector-ref vector-set! vector?
 when with-exception-handler write-bytevector write-char write-string write-u8
 zero?)