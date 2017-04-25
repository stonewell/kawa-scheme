(module-name (kawa mstrings))

(export list->string (rename mstring string) string-append string-map substring
        string-upcase string-downcase string-titlecase string-foldcase
        vector->string)

(require kawa.lib.prim_imports)
(require <kawa.lib.std_syntax>)
(import kawa.lib.strings)
(import kawa.lib.kawa.string-cursors)

(define (string-map proc str1::string #!rest rst::string[])::string
  (define nrst rst:length)
  (define n::int (+ nrst 1))
  (define cursors::string-cursor[] (string-cursor[] length: n))
  (define ends::string-cursor[] (string-cursor[] length: n))
  (define chs::gnu.text.Char[] (gnu.text.Char[] length: n))
  (define len1 (str1:length))
  (define result (gnu.lists.FString:alloc len1))
  (set! (cursors 0) 0)
  (set! (ends 0) len1)
  (do ((i ::int 1 (+ i 1))) ((>= i n))
    (let ((str ::string (rst (- i 1))))
      (set! (cursors i) 0)
      (set! (ends i) (str:length))))
  (let loop1 ()
    (let loop2 ((i::int 0))
      (cond ((= i n)
             (result:append (proc @chs))
             (loop1))
            (else
             (define curs-i (cursors i))
             (define end-i (ends i))
             (define str ::string (if (= i 0) str1 (rst (- i 1))))
             (cond ((string-cursor<? curs-i end-i)
                    (set! (chs i) (string-cursor-ref str curs-i))
                    (set! (cursors i) (string-cursor-next str curs-i))
                    (loop2 (+ i 1))))))))
  result)

(define (vector->string (vec ::vector)
                        #!optional (start ::int 0) (end ::int (vec:size)))
  ::string
  (let ((result (gnu.lists.FString:alloc (- end start))))
    (let loop ((i ::int start))
      (if (>= i end)
          result
          (let ((ch (vec i)))
            (if (java.lang.Character? ch)
                (result:append ((as java.lang.Character ch):charValue))
                (result:appendCharacter ((as gnu.text.Char ch):intValue)))
            (loop (+ i 1)))))))

(define (list->string (lst ::list)) ::string
  (let* ((len ::int (lst:size))
	 (result (gnu.lists.FString:alloc len)))
    (do ((i ::int 0 (+ i 1)))
	((>= i len) result)
      (let ((pair ::pair lst))
        (result:appendCharacter (as int (as character pair:car)))
	(set! lst pair:cdr)))))

(define (mstring #!rest args ::object[]) name: "string" :: <string>
  (let* ((n :: <int> args:length)
	 (str (gnu.lists.FString:alloc n)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n) str)
	(str:appendCharacter
         ((as gnu.text.Char (args i)):intValue)))))

(define (string-append #!rest (args :: java.lang.CharSequence[])) :: <gnu.lists.FString>
  (let ((str :: <gnu.lists.FString> (make <gnu.lists.FString>)))
    (invoke str 'addAllStrings args 0)
    str))

(define (substring str::<string> start::<int> end::<int>)
  :: <string>
  (string-copy str start end))

(define (string-upcase (str :: string)) :: string
  (gnu.lists.FString ((str:toString):toUpperCase java.util.Locale:ENGLISH)))

(define (string-downcase (str :: string)) :: string
  (gnu.lists.FString ((str:toString):toLowerCase java.util.Locale:ENGLISH)))

(define (string-titlecase (str :: string)) :: string
  (gnu.lists.FString (gnu.kawa.functions.UnicodeUtils:capitalize str)))

(define (string-foldcase (str :: string)) :: string
  (gnu.lists.FString (gnu.kawa.functions.UnicodeUtils:foldCase str)))
