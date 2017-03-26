(import
 kawa.lib.prim_imports
 kawa.lib.syntax
 kawa.lib.std_syntax
 kawa.lib.strings
 kawa.lib.case_syntax
 kawa.lib.exceptions)

(define (string-concatenate slist)::string
  (let ((result (gnu.lists.FString)))
    (for-each (lambda (s) (result:append s)) slist)
    (gnu.lists.IString (result:toString))))

(define (string-concatenate-reverse slist
                                    #!optional
                                    (final-string ::string "")
                                    (end ::int (string-length final-string)))
  ::string
  (let ((result (gnu.lists.FString)))
    (result:append final-string 0
                   (gnu.lists.Strings:offsetByCodePoints final-string end 0 0))
    (for-each (lambda (s) (result:prepend s)) slist)
    (gnu.lists.IString (result:toString))))

(define (reverse-list->string (lst ::list)) ::string
  (let* ((len ::int (lst:size))
	 (result (gnu.lists.FString:alloc len)))
    (do ((i ::int 0 (+ i 1)))
	((>= i len) (gnu.lists.IString (result:toString)))
      (let ((pair ::pair lst))
        (result:prependCharacter (as int (as character pair:car)))
	(set! lst pair:cdr)))))

(define (string-join string-list
                     #!optional
                     (delimiter ::string " ") (grammar 'infix))
  ::string
  (let ((result (gnu.lists.FString))
        (seen ::boolean #f))
    (for-each (lambda (s)
                (if (or (eq? grammar 'prefix)
                        (and seen (not (eq? grammar 'suffix))))
                    (result:append delimiter))
                (result:append s)
                (if (eq? grammar 'suffix)
                    (result:append delimiter))
                (set! seen #t))
              string-list)
    (case grammar
      ((prefix suffix infix) #t)
      ((strict-infix)
       (if (not seen)
           (error "string-join: empty string-list with 'strict-infix option")))
      (else
       (error "string-join: grammar symbol must be one of 'prefix 'suffix 'infix 'strict-index")))
    (gnu.lists.IString (result:toString))))
