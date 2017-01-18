(module-name (kawa istrings))

(module-export $make$string$ list->string string-append substring)

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)

;;(import (only kawa.lib.kawa.mstrings (list->string list->mstring)))

(define ($make$string$ #!rest args ::object[]) :: <string>
  (let* ((n :: <int> args:length)
	 (str (gnu.lists.FString:alloc n)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n) (gnu.lists.IString str))
	(str:appendCharacter
         ((as gnu.text.Char (args i)):intValue)))))

(define (list->string (lst ::list)) ::string
  ;;(gnu.lists.IString ((list->mstring lst):toString)))
  (let* ((len ::int (lst:size))
	 (result (gnu.lists.FString:alloc len)))
    (do ((i ::int 0 (+ i 1)))
	((>= i len) (gnu.lists.IString result))
      (let ((pair ::pair lst))
        (result:appendCharacter (as int (as character pair:car)))
	(set! lst pair:cdr)))))

(define (string-append #!rest (args :: java.lang.CharSequence[])) :: gnu.lists.IString
  (let* ((buf (java.lang.StringBuilder))
         (n args:length))
    (let loop ((i::int 0))
      (cond ((< i n)
             (buf:append (args i))
             (loop (+ i 1)))))
    (gnu.lists.IString buf)))

(define (substring str::string start::int end::int)
  :: <string>
  (gnu.lists.IString:valueOf str start (- end start)))
