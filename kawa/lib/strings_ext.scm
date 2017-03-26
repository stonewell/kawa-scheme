(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.strings>)

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
