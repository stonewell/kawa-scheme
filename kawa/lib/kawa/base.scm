(module-name (kawa base))
(include "../scheme/base-exports")
(include "../scheme/char-exports")
(include "../scheme/complex-exports")
(include "../scheme/cxr-exports")
(include "../scheme/eval-exports")
(include "../scheme/file-exports")
(include "../scheme/inexact-exports")
(include "../scheme/lazy-exports")
(include "../scheme/process-context-exports")
(include "../scheme/time-exports")
(include "../scheme/write-exports")
(require <kawa.lib.enums>)
(require <kawa.lib.srfi.26>)

(export list->string string-append substring vector->string)
(export case-lambda read interaction-environment null-environment
        exact->inexact inexact->exact)
(export ! ? => $<<$ $>>$
        $bracket-apply$ $bracket-list$
        $construct$ $construct$:cmd $construct$:sh
        $construct$:PD $construct$:|<| $construct$:|>| $construct$:|>>|
        $construct$:|`|
        $entity$ $format$ $resolve-qname$
        $string$ $string-with-default-format$ $sprintf$
        $xml-attribute$ $xml-comment$ $xml-CDATA$ $xml-element$
        $xml-processing-instruction$ $xml-text$
        *print-base* *print-circle* *print-miser-width*
        *print-right-margin* *print-radix* *print-xml-indent*
        absolute-path? annotation arglist argvector
        array array? array-rank array-size array-start array-end
        array-fill! array-copy! array-index-ref array-index-share
        array-transform array-reshape array-flatten array->vector
        index-array build-array format-array shape share-array
        arithmetic-shift ash
        array array-ref array-set! as attributes
        begin-for-syntax
        bit-extract bitvector bitwise-not
        bitwise-and bitwise-ior bitwise-xor bitwise-if
        bitwise-bit-count bitwise-length bitwise-first-bit-set
        bitwise-bit-set? bitwise-copy-bit
        bitwise-bit-field bitwise-copy-bit-field
        bitwise-arithmetic-shift
        bitwise-arithmetic-shift-left
        bitwise-arithmetic-shift-right
        bitwise-rotate-bit-field bitwise-reverse-bit-field
        bound-identifier=?
        c16vector call-with-input-string call-with-output-string catch
        char-general-category char-title-case? char-titlecase
        copy-file create-directory
        class-methods children command-line-arguments command-parse
        compile-file constant-fold constant-vector cut cute current-path
        datum->syntax datum->syntax-object default-prompter
        define-alias define-autoload define-autoloads-from-file
        define-base-unit define-class define-constant
        define-early-constant define-enum define-for-syntax
        define-library define-macro define-namespace
        define-private define-private-alias define-private-namespace
        define-procedure define-record-type define-rewrite-syntax
        define-variable define-simple-class define-simple-constructor
        define-syntax-case define-unit define-xml-namespace
        defmacro disassemble
        div div0 mod mod0 div-and-mod div0-and-mod0
        duration dynamic
        eager environment-bound?
        error error-object? error-object-message error-object-irritants
        export module-export
        f32vector f32vector? f32vector-length
        f32vector-ref f32vector-set! f32vector->list
        f64vector f64vector? f64vector-length
        f64vector-ref f64vector-set! f64vector->list
        field file-error? filepath format free-identifier=?
        identifier? import include-relative
        input-prompt1 input-prompt2
        fluid-let
        force* force-output future
        integer-valued? rational-valued? real-valued?
        file-directory? file-readable? file-writable?
        filepath?
        force-output
        format
        generate-temporaries gentemp
        get-environment-variable get-environment-variables
        html
        input-port-column-number input-port-line-number
        input-port-prompter input-port-read-state
        instance? integer-length
        invoke invoke-static invoke-special
        lazy load load-relative location logop logtest
        logand logior logxor lognot logcount
        keyword? keyword->string string->keyword
        sinh cosh tanh asinh acosh atanh
        list->f32vector list->f64vector
        list->s8vector list->s16vector list->s32vector list->s64vector
        list->u8vector list->u16vector list->u32vector list->u64vector
        make make-array make-attribute make-element make-procedure
        make-f32vector make-f64vector
        make-s8vector make-s16vector make-s32vector make-s64vector
        make-u8vector make-u16vector make-u32vector make-u64vector
        map-values match
        module-class module-compile-options module-extends module-implements
        module-name module-static module-uri
        object
        path
        port-line port-column
        primitive-constructor primitive-get-field primitive-set-field
        primitive-get-static primitive-set-static
        primitive-array-new primitive-array-get
        primitive-array-set primitive-array-length
        primitive-interface-method primitive-op1
        primitive-static-method primitive-throw primitive-virtual-method
        procedure-property set-procedure-property! add-procedure-properties
        provide
        quote
        read-error? report-syntax-error require
        setter synchronized
        process-command-line-assignments rename-file system-tmpdir
        process-exit-wait process-exit-ok?
        ->pathname system
        make-process pipe-process
        make-record-type namespace-uri namespace-prefix
        make-quantity quantity->number quantity->unit regex runnable
        parse-format path?
        path-authority path-bytes path-data path-directory path-extension
        path-file path-fragment path-host path-last path-parent path-port
        path-query path-scheme path-user-info port-char-encoding
        promise-set-value! promise-set-alias! promise-set-exception!
        promise-set-thunk!
        receive record? record-accessor record-constructor record-modifier
        record-predicate record-type-descriptor record-type-field-names
        record-type-name
        repl resolve-uri resource-url reverse! reverse-list->string
        run-process
        s8vector s8vector? s8vector-length
        s8vector-ref s8vector-set! s8vector->list
        s16vector s16vector? s16vector-length
        s16vector-ref s16vector-set! s16vector->list
        s32vector s32vector? s32vector-length
        s32vector-ref s32vector-set! s32vector->list
        s64vector s64vector? s64vector-length
        s64vector-ref s64vector-set! s64vector->list
        scan scheme-implementation-version scheme-report-environment
        scheme-window sequence set-input-port-line-number! sleep
        set-input-port-prompter! set-port-line!
        slot-ref slot-set! srfi-13-string-for-each static-field
        string-append! string-any string-contains string-contains-right
        string-replace!
        string-titlecase
        string-capitalize
        string-capitalize! ;; ???
        string-concatenate string-concatenate-reverse string-count
        string-drop string-drop-right
        string-every string-filter string-fold string-fold-right
        string-for-each-index string-index string-index-right
        string-join string-map-index
        string-normalize-nfd string-normalize-nfkd
        string-normalize-nfc string-normalize-nfkc string-null?
        string-pad string-pad-right string-prefix? string-prefix-length
        string-remove string-repeat string-replicate
        string-skip string-skip-right string-suffix? string-suffix-length
        string-tabulate string-take string-take-right
        string-trim string-trim-right string-trim-both
        string-unfold string-unfold-right
        string->utf16 string->utf16le string->utf16be
        subtype?
        syntax-error syntax->expression syntax-body->expression
        syntax-column syntax-line syntax-object->datum syntax-source
        syntax quasisyntax
        syntax-case syntax->datum
        symbol-local-name symbol-namespace symbol-namespace-uri symbol-prefix
        symbol-read-case
        test-begin
        this throw tokenize-string-to-string-array tokenize-string-using-shell
        trace transcript-off transcript-on
        try-catch try-finally
        u8vector u8vector? u8vector-length
        u8vector-ref u8vector-set! u8vector->list
        u16vector u16vector? u16vector-length
        u16vector-ref u16vector-set! u16vector->list
        u32vector u32vector? u32vector-length
        u32vector-ref u32vector-set! u32vector->list
        u64vector u64vector? u64vector-length
        u64vector-ref u64vector-set! u64vector->list
        utf16->string utf16be->string utf16le->string
        unescaped-data unit untrace URI URI? values-append
        with-compile-options with-syntax
        )
(import (only kawa.istrings list->string string-append substring vector->string))
(require kawa.lib.DefineRecordType)
(import (rename kawa.lib.prim_imports (letrec letrec*)))
(import
   kawa.lib.arrays
   kawa.lib.bytevectors
   kawa.lib.case_syntax
   kawa.lib.characters
   kawa.lib.DefineRecordType
   kawa.lib.exceptions
   kawa.lib.files
   kawa.lib.keywords
   kawa.lib.lists
   kawa.lib.misc
   kawa.lib.misc_syntax
   kawa.lib.numbers
   kawa.lib.vectors
   kawa.lib.parameters
   kawa.lib.parameterize
   kawa.lib.ports
   kawa.lib.prim_imports
   kawa.lib.reflection
   kawa.lib.strings
   kawa.lib.strings_ext
   kawa.lib.std_syntax
   kawa.lib.syntax
   kawa.lib.system
   kawa.lib.trace
   kawa.lib.uniform
   kawa.lib.windows
   (scheme case-lambda)
   (scheme cxr)
   (scheme eval)
   (scheme file)
   (scheme lazy)
   ;(scheme load)
   (scheme process-context)
   (scheme read)
   (scheme repl)
   (scheme time)
   (scheme write)

   (only kawa.lib.files
         ($construct$:PD $construct$:|<|)
         ($construct$:set_PD $construct$:|>|)
         ($construct$:append_PD $construct$:|>>|))
   (only kawa.lib.system ($construct$:cmd $construct$:|`|))
   (only kawa.lib.numbers( bitwise-bit-field bit-extract))
   (only kawa.lib.srfi.8 receive)
   (rnrs arithmetic bitwise)
   (rnrs unicode)
   (only kawa.standard.define_autoload
         (define_autoload define-autoload)
         (define_autoloads_from_file define-autoloads-from-file))
   (only kawa.standard.define_unit
         (define_base_unit define-base-unit) (define_unit define-unit))
   (only kawa.standard.export export (module_export module-export))
   (only (scheme r5rs) exact->inexact inexact->exact)
   (only kawa.standard.load load (loadRelative load-relative))
   (only kawa.standard.object (objectSyntax object))
   (only kawa.standard.Scan scan)
   (only kawa.standard.SchemeCompilation repl)
   (only kawa.standard.syntax
         (quasiSyntax quasisyntax) syntax)

   (only kawa.lib.std_syntax
         identifier? with-syntax datum->syntax-object syntax-object->datum
         generate-temporaries datum->syntax syntax->datum)

   (only (rename (only (kawa standard syntax_case) syntax_case)
		 (syntax_case syntax-case))
	 syntax-case)

   (only kawa.standard.require require)
   (only kawa.standard.ImportFromLibrary (instance import))

   (only (rename (kawa standard define_class)
		 (define_simple_class define-simple-class)
		 (define_class define-class))
	 define-simple-class define-class)

   ;(kawa reflect)

   (only kawa.lib.numbers
         (bitwise-length integer-length))
   (only gnu.kawa.functions.BitwiseOp
         (and logand) (ior logior) (xor logxor) (not lognot)
         (ashift arithmetic-shift) (ashift ash))
   (only gnu.kawa.functions.ValuesMap (valuesMap map-values))
   (only gnu.kawa.functions.AppendValues (appendValues values-append))
   (only gnu.kawa.functions.DisplayFormat
         (outBase *print-base*) (outRadix *print-radix*))
   (only gnu.kawa.functions.ParseFormat (parseFormat parse-format))
   (only gnu.kawa.lispexpr.BracketApply (instance $bracket-apply$))
   (only gnu.kawa.lispexpr.LispLanguage
         (unitNamespace unit) (entityNamespace $entity$)
         (constructNamespace $construct$))
   (only gnu.kawa.lispexpr.MakeXmlElement (makeXml $xml-element$))
   (only gnu.kawa.lispexpr.ResolveNamespace (resolveQName $resolve-qname$))
   (only gnu.kawa.reflect.MakeAnnotation (instance annotation))
   (only gnu.kawa.reflect.ClassMethods (classMethods class-methods))
   (only gnu.kawa.xml.Attributes attributes)
   (only gnu.kawa.xml.Children children)
   (only gnu.kawa.xml.CommentConstructor (commentConstructor $xml-comment$))
   (only gnu.kawa.xml.MakeAttribute
         (makeAttributeS $xml-attribute$)
         (makeAttributeS make-attribute))
   (only gnu.kawa.xml.MakeCDATA (makeCDATA $xml-CDATA$))
   (only gnu.kawa.xml.MakeElement (makeElementS make-element))
   (only gnu.kawa.xml.MakeProcInst (makeProcInst $xml-processing-instruction$))
   (only gnu.kawa.xml.MakeText (makeText $xml-text$))
   (only gnu.kawa.xml.MakeUnescapedData (unescapedData unescaped-data))
   (only gnu.kawa.xml.XmlNamespace (HTML html))
   (only gnu.xml.XMLPrinter (indentLoc *print-xml-indent*))
   (only gnu.kawa.io.OutPort (charEncoding port-char-encoding))
   (only (kawa lib thread)
	 future sleep runnable)

   )

(define-alias command-line-arguments
  gnu.expr.ApplicationMainSupport:commandLineArguments)
(define-alias run-process gnu.kawa.functions.RunProcess:instance)

(define-alias input-prompt1 gnu.kawa.io.CheckConsole:prompt1)
(define-alias input-prompt2 gnu.kawa.io.CheckConsole:prompt2)
(define-alias default-prompter kawa.Shell:defaultPrompter)

(cond-expand (class-exists:kawa.DomTermBackend
              (import kawa.lib.kawa.domterm)
              (export domterm-load-stylesheet)))
