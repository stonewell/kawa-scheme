;; Definitions for some primitives before we define anything else.

(export
 + - * / ! $<<$ $>>$ $bracket-apply$
 *print-circle* *print-miser-width* *print-right-margin*
 append apply arglist argvector array array-ref array-set! as
 begin bitvector bytevector
 c16vector call-with-current-continuation call-with-values call/cc cond-expand
 constant-fold constant-vector
 define define-alias define-class define-library
 define-namespace define-private-alias
 define-private-namespace define-rewrite-syntax
 define-syntax define-constant define-early-constant
 define-private define-simple-class define-variable
 define-xml-namespace dynamic
 div div0
 eq? equal? eqv? even? expt
 f32vector f64vector
 field floor-remainder fluid-let for-each include include-ci include-relative
 filepath  floor-quotient format instance?
 lambda let-syntax letrec-syntax list location
 if invoke invoke-special invoke-static try-catch letrec
 make-procedure map module-extends module-implements module-name module-static
 make mod module-compile-options modulo mod0
 not odd?
 path primitive-interface-method primitive-op1
 primitive-static-method primitive-throw primitive-virtual-method
 quasiquote quasisyntax quote quotient regex remainder report-syntax-error
 s8vector s16vector s32vector s64vector
 symbol-read-case
 syntax->expression syntax-body->expression
 sequence set! setter slot-ref slot-set! static-field string
 syntax syntax-case syntax-error syntax-rules
 this truncate-quotient truncate-remainder
 u8vector u16vector u32vector u64vector URI
 vector vector-append with-compile-options
 = < > <= >=)

(import (only gnu.kawa.reflect.Invoke
              invoke make
              (invokeSpecial invoke-special)
              (invokeStatic invoke-static)))
(import (only gnu.kawa.reflect.SlotGet
              field (staticField static-field) (slotRef slot-ref))
        (only gnu.kawa.reflect.SlotSet (set-field! slot-set!)))
(import (only gnu.kawa.reflect.Throw (primitiveThrow primitive-throw)))
(import (only kawa.standard.constant_fold (constant_fold constant-fold)))
(import (only kawa.standard.define_class
              (define_class define-class)
              (define_simple_class define-simple-class)))
(import (only kawa.standard.define_library
              (define_library define-library)))
(import (only kawa.standard.location location))
(import (only kawa.standard.syntax syntax (quasiSyntax quasisyntax)))
(import (only kawa.standard.syntax_case (syntax_case syntax-case)))
(import (only kawa.standard.prim_method
              (interface_method primitive-interface-method)
              (static_method primitive-static-method)
              (op1 primitive-op1)
              (virtual_method primitive-virtual-method)))
(import (only kawa.standard.MatchDef (matchDef !)))
(import (only kawa.standard.fluid_let (fluid_let fluid-let)))
(import (only kawa.standard.module_compile_options
              (module_compile_options module-compile-options)))
(import (only kawa.standard.module_name (module_name module-name)))
(import (only kawa.standard.module_extends (module_extends module-extends)))
(import (only kawa.standard.module_implements
              (module_implements module-implements)))
(import (only kawa.standard.module_static (module_static module-static)))
(import (only kawa.standard.thisRef (thisSyntax this)))
(import (only kawa.standard.with_compile_options
              (with_compile_options with-compile-options)))
(import (only (gnu kawa functions AddOp) + -))
(import (only (gnu kawa functions CallCC)
              (callcc call-with-current-continuation)
              (callcc call/cc)))
(import (only (gnu kawa functions DivideOp)
              div mod div0 mod0
              (floorQuotient floor-quotient)
              (modulo floor-remainder)
              (quotient truncate-quotient)
              (remainder truncate-remainder)))
(import (only (gnu kawa functions DivideOp) / modulo quotient remainder))
(import (only gnu.kawa.functions.Format format))
(import (only gnu.kawa.functions.MakeProcedure (makeProcedure make-procedure)))
(import (only gnu.kawa.functions.MultiplyOp *))
(import (only gnu.kawa.functions.Setter setter))
(import (only gnu.kawa.io.PrettyWriter
              (isSharing *print-circle*)
              (miserWidthLoc *print-miser-width*)
              (lineLengthLoc *print-right-margin*)))
(import (only gnu.kawa.lispexpr.BracketApply (instance $bracket-apply$)))
(import (only gnu.kawa.lispexpr.DefineNamespace
              (define_namespace define-namespace)
              (define_private_namespace define-private-namespace)
              (define_xml_namespace define-xml-namespace)))
(import (only gnu.kawa.lispexpr.LangObjType
              (listType list) (stringType string) (vectorType vector)
              (bitvectorType bitvector) (c16vectorType c16vector)
              (f32vectorType f32vector)  (f64vectorType f64vector)
              (s8vectorType s8vector)  (s16vectorType s16vector)
              (s32vectorType s32vector)  (s64vectorType s64vector)
              (u8vectorType u8vector)  (u16vectorType u16vector)
              (u32vectorType u32vector)  (u64vectorType u64vector)
              (u8vectorType bytevector)))
(import (only gnu.kawa.lispexpr.LispReader (symbolReadCase symbol-read-case)))
(import (only kawa.lang.Quote
              (plainQuote quote) (quasiQuote quasiquote)))
(import (only (kawa standard append) append))
(import (only (kawa standard begin) begin))
(import (rename (only (gnu kawa functions CallWithValues) callWithValues)
                (callWithValues call-with-values)))
(import (only gnu.kawa.functions.Expt expt))
(import (only kawa.standard.define_alias
              (define_alias define-alias)
              (define_private_alias define-private-alias)))
(import (rename (only (kawa standard IfFeature) condExpand)
                (condExpand cond-expand)))
;;(import (rename (only (kawa standard Include) include includeCi)
;;                (includeCi include-ci)))
(import (only kawa.standard.Include
              include
              (includeCi include-ci)
              (includeRelative include-relative)))
(import (rename (only (kawa standard let_syntax) let_syntax letrec_syntax)
                (let_syntax let-syntax) (letrec_syntax letrec-syntax)))
(import (only kawa.standard.Scheme
              apply forEach isOdd isEven isEq isEqv isEqual
              numEqu numGrt numGEq numLss numLEq map not
              (emptyStringLeft $<<$)
              (emptyStringRight $>>$)
              (instanceOf instance?)
              (forEach for-each)
              (isOdd odd?)
              (isEven even?)
              (isEq eq?) (isEqv eqv?) (isEqual equal?)
              (numEqu =) (numGrt >) (numGEq >=) (numLss <) (numLEq <=)))
(import (only (kawa standard SchemeCompilation) lambda))
(import (only gnu.kawa.functions.Convert as))
(import (rename (only (kawa standard set_b) set) (set set!)))
(import (rename (only (kawa standard syntax_error) syntax_error)
                (syntax_error syntax-error)))
(import (rename (only (kawa standard syntax_rules) syntax_rules)
                (syntax_rules syntax-rules)))
(import (rename (only (kawa standard vector_append) vectorAppend)
                (vectorAppend vector-append)))

(import (only gnu.kawa.lispexpr.GenArrayType (generalInstance array)))
(import (only gnu.kawa.lispexpr.LangObjType
              (argListType arglist)
              (argVectorType argvector)
              (constVectorType constant-vector)
              (dynamicType dynamic)
              (filepathType filepath)
              (pathType path)
              (regexType regex)
              (sequenceType sequence)
              (URIType URI)))
(import (only gnu.kawa.functions.ArrayRef (arrayRef array-ref)))
(import (only gnu.kawa.functions.ArraySet (arraySet array-set!)))

(import (only (kawa standard define) (defineRaw %define)))
(import (only kawa.standard.define_syntax
              (define_syntax %define-syntax)
              (define_rewrite_syntax define-rewrite-syntax)))
(import (only (kawa standard let) (let %let)))
(import (only (kawa standard set_b) (set set!)))
(import (only (kawa standard begin) begin))
(import (only (kawa standard SchemeCompilation) lambda mlambda))

(%define-syntax define-syntax
  (syntax-rules ($lookup$)
    ((define-syntax (($lookup$ part1 'part2) . pattern) . forms)
     ;; Should deprecate - incompatible with SRFI-72
     (%define-syntax ($lookup$ part1 'part2) (lambda pattern . forms)))
    ((define-syntax ($lookup$ part1 'part2) function)
     (%define-syntax ($lookup$ part1 'part2) function))
    ((define-syntax (name . pattern) . forms)
     ;; Should deprecate - incompatible with SRFI-72
     (%define-syntax name (lambda pattern . forms)))
    ((define-syntax name function)
     (%define-syntax name function))))

(%define-syntax define
  (syntax-rules (:: $lookup$)
    ((define ($lookup$ part1 'part2) :: type value)
     (%define ($lookup$ part1 'part2) 1 type value))
    ((define ($lookup$ part1 'part2) value)
     (%define ($lookup$ part1 'part2) 0 #!null value))
    ((define (name . formals) . body)
     (%define name 2 #t formals . body))
    ((define name :: type value)
     (%define name 1 type value))
    ((define name value)
     (%define name 0 #!null value))))

(%define-syntax define-private
  (syntax-rules (:: $lookup$)
    ((define-private ($lookup$ part1 'part2) :: type value)
     (%define ($lookup$ part1 'part2) 5 type value))
    ((define-private ($lookup$ part1 'part2) value)
     (%define ($lookup$ part1 'part2) 4 #!null value))
    ((define-private (name . formals) . body)
     (%define name 6 #t formals . body))
    ((define-private name :: type value)
     (%define name 5 type value))
    ((define-private name value)
     (%define name 4 #!null value))))

(%define-syntax define-constant
  (syntax-rules (:: $lookup$)
    ((define-constant ($lookup$ part1 'part2) :: type value)
     (%define ($lookup$ part1 'part2) 9 type value))
    ((define-constant ($lookup$ part1 'part2) value)
     (%define ($lookup$ part1 'part2) 8 #!null value))
    ((define-constant name :: type value)
     (%define name 9 type value))
    ((define-constant name value)
     (%define name 8 #!null value))))

(%define-syntax define-early-constant
  (syntax-rules (:: $lookup$)
    ((define-early-constant ($lookup$ part1 'part2) :: type value)
     (%define ($lookup$ part1 'part2) 25 type value))
    ((define-early-constant ($lookup$ part1 'part2) value)
     (%define ($lookup$ part1 'part2) 24 #!null value))
    ((define-early-constant name :: type value)
     (%define name 25 type value))
    ((define-early-constant name value)
     (%define name 24 #!null value))))

(%define-syntax define-variable
  (syntax-rules (:: $lookup$)
    ((define-variable name :: type)
     (define-variable name :: type #!undefined))
    ((define-variable name)
     (define-variable name #!undefined))
    ((define-variable ($lookup$ part1 'part2) :: type value)
     (%define ($lookup$ part1 'part2) 33 type value))
    ((define-variable ($lookup$ part1 'part2) value)
     (%define ($lookup$ part1 'part2) 32 #!null value))
    ((define-variable name :: type value)
     (%define name 33 type value))
    ((define-variable name value)
     (%define name 32 #!null value))))

(%define report-syntax-error 2 #!null (id #!rest (msg :: <Object[]>))
  (invoke-static <kawa.standard.syntax_error> 'error id msg))

(%define-syntax syntax->expression
  (syntax-rules ()
    ((syntax->expression x)
     (kawa.lang.SyntaxForms:rewrite x))))

(%define-syntax syntax-body->expression
  (syntax-rules ()
    ((syntax-body->expression x)
     (kawa.lang.SyntaxForms:rewriteBody x))))

(%define-syntax %if-and-x
 (lambda (x)
   (syntax-case x (? :: and)
    ((_ k then)
     (gnu.expr.ExitExp (syntax->expression #'then) #'k))
    ((_ k then test1 . rest)
     #`(if test1
         (%if-and-x k then . rest))))))

(define-rewrite-syntax if
  (lambda (x)
    (syntax-case x (? :: and)
      ((_ (and . tests) then else)
       (%let ((bl (gnu.expr.BlockExp)))
             (bl:setRunFinallyBlocks #f)
             (bl:setBody
              (gnu.expr.BeginExp
               (syntax->expression #`(%if-and-x #,bl then . tests))
               (syntax->expression #`else)))
             bl))
      ((_ (? . rest) then)
       #'(if (? . rest) then #!void))
      ((_ (and . rest) then)
       #'(if (and . rest) then #!void))
      ((_ (? pattern :: type init) then else)
       #`(#,gnu.kawa.reflect.TypeSwitch:typeSwitch
          init
          (mlambda (pattern :: type) then)
          (lambda (unused) else)))
      ((_ (? pattern init) then else)
       #`(#,gnu.kawa.reflect.TypeSwitch:typeSwitch
          init
          (mlambda (pattern) then)
          (lambda (unused) else)))
      ((_ test then)
       (make <gnu.expr.IfExp>
         (syntax->expression (syntax test))
         (syntax->expression (syntax then))
         #!null))
      ((_ test then else)
       (make <gnu.expr.IfExp>
         (syntax->expression (syntax test))
         (syntax->expression (syntax then))
         (syntax->expression (syntax else))))
      ((_ e1 e2 e3 . rest)
       (report-syntax-error #'rest
                            "too many expressions for 'if'"))
      ((_ . rest)
       (report-syntax-error #'rest
                            "too few expressions for 'if'")))))

(define-rewrite-syntax try-catch
  (lambda (x)
    (syntax-case x ()
		 ((_ try-part (var type . catch-body) ...)
		  (invoke-static <kawa.standard.try_catch> 'rewrite
				 (syntax try-part)
				 (syntax
				  #((((var :: type)) . catch-body) ...)))))))

(%define-syntax letrec
  (lambda (form)
    (%let ((out-bindings '()) (out-inits '()))
      (syntax-case form ()
	((_ bindings . body)
         ;; Simulate a (letrec ((process-binding (lambda ...) ...)) ...)
         ;; which we can't use in here, because that is what we're defining.
	 (%let ((process-binding #!undefined))
	       (set! process-binding
		     (lambda (b)
		       (syntax-case b ()
			 (() #!void)
			 (((name init) . rest)
			  (begin
			    (set! out-bindings
				  (make <pair> (syntax (name #!undefined)) out-bindings))
			    (set! out-inits (make <pair> (syntax (set! name init)) out-inits))
			    (process-binding (syntax rest))))
			 (((name :: type init) . rest)
			  (begin
			    (set! out-bindings (make <pair> (syntax (name :: type #!undefined)) out-bindings))
			    (set! out-inits (make <pair> (syntax (set! name init)) out-inits))
			    (process-binding (syntax rest))))
			 (((name) . rest)
			  (report-syntax-error b "missing initializion in letrec"))
			 (whatever
			  (report-syntax-error b "invalid bindings syntax in letrec")))))
	       (process-binding (syntax bindings))
	       (set! out-bindings (gnu.lists.LList:reverseInPlace out-bindings))
	       (set! out-inits (gnu.lists.LList:reverseInPlace out-inits))
	       #`(%let #,out-bindings #,@out-inits . body)))))))
