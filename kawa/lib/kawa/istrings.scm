(module-name (kawa istrings))

(module-export $make$string$ list->string string-append substring
               vector->string)

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)
(import (only kawa.mstrings
              (vector->string vector->mstring)
              (list->string list->mstring)))

(define ($make$string$ #!rest args ::object[]) :: <string>
  (let* ((n :: <int> args:length)
	 (str (gnu.lists.FString:alloc n)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n) (gnu.lists.IString str))
	(str:appendCharacter
         ((as gnu.text.Char (args i)):intValue)))))

(define (vector->string (vec ::vector)
                        #!optional (start ::int 0) (end ::int (vec:size)))
  (gnu.lists.IString (vector->mstring vec start end)))

(define (list->string (lst ::list)) ::string
  (gnu.lists.IString ((list->mstring lst):toString)))

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
