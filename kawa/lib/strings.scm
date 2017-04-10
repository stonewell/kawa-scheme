(module-export string? make-string string-length
               string-ref string-set!
               char=? char<? char>? char<=? char>=?
               char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
               string=? string<? string>? string<=? string>=?
	       string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
               string-fold string-fold-right string->list
               string-copy string-copy!
               string-fill! string-upcase! string-downcase!
               string-capitalize string-capitalize!
               string-append! string-replace! string-replicate
               string-map string-map-index
               string-for-each string-for-each-index srfi-13-string-for-each
               string-tabulate string-unfold string-unfold-right
               string->utf8 string->utf16 string->utf16be string->utf16le)

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.lists>)
(import (kawa lib kawa string-cursors))

(define-syntax define-compare
  (syntax-rules ()
    ((_ NAME TYPE OP COMP2 PROPS ...)
     (define (NAME (str1 ::TYPE) (str2 ::TYPE)
                          #!rest (strs ::TYPE[]))
       ::boolean PROPS ...
       (and (OP (COMP2 str1 str2) 0)
            (let ((n ::int strs:length))
              (let loop ((i ::int 0) (prev ::TYPE str2))
                (or (= i n)
                    (let ((next (strs i)))
                      (and (OP (COMP2 prev next) 0)
                           (loop (+ i 1) next)))))))))))

(define-syntax with-start-end
  (syntax-rules ()
    ((_ str (start end supplied-end) (cstart cend) . body)
     (let* ((cstart (gnu.lists.Strings:offsetByCodePoints str start 0 0))
            (cend (cond ((not supplied-end) (str:length))
                        ((< end start)
                         (primitive-throw
                          (java.lang.StringIndexOutOfBoundsException)))
                        (else
                         (gnu.lists.Strings:offsetByCodePoints
                          str (- end start) cstart start)))))
       . body))))

(define (string? x) :: <boolean>
  (instance? x <string>))

(define (make-string #!optional (n ::int 0) (ch ::character #\Space))
  ::gnu.lists.FString
  (make <gnu.lists.FString> n (as int ch)))

(define (string-length str ::string) ::int
  (gnu.lists.Strings:sizeInCodePoints str))

(define (string-ref (str ::java.lang.CharSequence) (k ::int)) ::character
 (as character (gnu.lists.Strings:indexByCodePoints str k)))

(define (string-set! str::abstract-string k::int char::character)
  ::void
  (invoke str 'setCharacterAt
          (java.lang.Character:offsetByCodePoints str 0 k)
          (as int char)))

(define (%string-compare2 (str1 :: string) (str2 :: string)) ::int
  ((str1:toString):compareTo (str2:toString)))

(define-compare string<? string < %string-compare2)
(define-compare string=? string = %string-compare2)
(define-compare string>? string > %string-compare2)
(define-compare string<=? string <= %string-compare2)
(define-compare string>=? string >= %string-compare2)

(define (string->list (str ::string)
                      #!optional (start ::int 0) (end ::int 0 supplied-end))
  ::list
  (with-start-end 
   str (start end supplied-end) (cstart cend)
   (let loop ((result ::list '())
              (i ::string-cursor (as string-cursor cend)))
     (if (string-cursor<=? i (as string-cursor start))
         result
         (let ((prev (string-cursor-prev str i)))
           (loop (make <pair> (string-cursor-ref str prev) result) prev))))))

(define (string-copy (str ::java.lang.CharSequence)
                     #!optional
                     (start ::int 0)
                     (end ::int 0 supplied-end))
  ::gnu.lists.FString
  (with-start-end str (start end supplied-end) (istart iend)
                  (gnu.lists.FString str istart (- iend istart))))

(define (string-copy! (to ::gnu.lists.FString)
                      (at ::int)
                      (from ::java.lang.CharSequence)
                      #!optional
                      (start ::int 0)
                      (end ::int (gnu.lists.Strings:sizeInCodePoints from)))
  ::void
  (string-replace! to at (+ at (- end start)) from start end))

(define (string-replace! (dst ::gnu.lists.FString)
                         (dstart ::int)
                         (dend ::int)
                         (src ::java.lang.CharSequence)
                         #!optional
                         (sstart ::int 0)
                         (send ::int 0 supplied-send))
  ::void
  (with-start-end 
   src (sstart send supplied-send) (csstart csend)
   (with-start-end
    dst (dstart dend #t) (cdstart cdend)
    (dst:replace src csstart csend cdstart cdend))))

(define (string-fill! str ::abstract-string ch ::character
                      #!optional
                      (start ::int 0)
                      (end ::int -1))
  ::void
  (! cstart ::int (java.lang.Character:offsetByCodePoints str 0 start))
  (! send (if (>= end 0) end (gnu.lists.Strings:sizeInCodePoints str)))
  (cond ((? fstr::gnu.lists.FString str)
         (let ((cend (if (< end 0) (fstr:length)
                         (java.lang.Character:offsetByCodePoints str 0 end))))
           (fstr:delete cstart cend)
           (fstr:insertRepeated cstart (as int ch) (- send start))))
        (else
         ;; Can be O(n^2) if a bad mix of 1-char and 2-char characters.
         (let ((width ::int (if (> (as int ch) #xffff) 2 1)))
           (do ((to-do ::int (- send start) (- to-do 1))
                (pos ::int cstart (+ pos width)))
               ((<= to-do 0))
             (str:setCharacterAt pos (as int ch)))))))

(define (string-upcase! str ::abstract-string) ::string
  (gnu.lists.Strings:makeUpperCase str)
  str)

(define (string-downcase! (str :: <abstract-string>)) :: <string>
  (invoke-static <gnu.lists.Strings> 'makeLowerCase str)
  str)

(define (string-capitalize! (str :: <abstract-string>)) :: <string>
  (invoke-static <gnu.lists.Strings> 'makeCapitalize str)
  str)

(define (string-capitalize (str :: <string>)) :: <string> 
  (let ((copy :: <gnu.lists.FString> (string-copy str)))
    (invoke-static <gnu.lists.Strings> 'makeCapitalize copy)
    copy))

(define (%string-compare-ci2 (str1 :: string) (str2 :: string)) ::int
  ;; The obvious doesn't handle German SS correctly.
  ;;((str1:toString):compareToIgnoreCase (str2:toString)))
  (invoke ((gnu.kawa.functions.UnicodeUtils:foldCase str1):toString)
          'compareTo
          ((gnu.kawa.functions.UnicodeUtils:foldCase str2):toString)))

(define-compare string-ci<? string < %string-compare-ci2)
(define-compare string-ci=? string = %string-compare-ci2)
(define-compare string-ci>? string > %string-compare-ci2)
(define-compare string-ci<=? string <= %string-compare-ci2)
(define-compare string-ci>=? string >= %string-compare-ci2)

(define (%char-compare (c1 :: character) (c2 :: character)) ::int
  (let ((i1 (as int c1)) (i2 (as int c2)))
    (cond ((> i1 i2) 1) ((< i1 i2) -1) (else 0))))

(define-compare char=? character = %char-compare
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char<? character < %char-compare
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char>? character > %char-compare
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char<=? character <= %char-compare
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char>=? character >= %char-compare
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")

(define (%char-compare-ci (c1 :: character) (c2 :: character)) ::int
  (- (java.lang.Character:toUpperCase (as int c1))
     (java.lang.Character:toUpperCase (as int c2))))

(define-compare char-ci=? character = %char-compare-ci
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char-ci<? character < %char-compare-ci
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char-ci>? character > %char-compare-ci
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char-ci<=? character <= %char-compare-ci
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")
(define-compare char-ci>=? character >= %char-compare-ci
  validate-apply: "kawa.lib.compile_misc:charCompareValidateApply")

;; Hook for backward compatibility with SRFI-13's string-for-each,
;; which differs from the R7RS version.
(define (srfi-13-string-for-each proc str::string #!optional
                                 (start::int 0) (end::int -1))
  ::void
  validate-apply: "kawa.lib.compile_map:stringForEach1ValidateApply" 
  (let* ((cstart ::string-cursor
                 (string-cursor-next str
                                     (as string-cursor 0)
                                     start))
         (cend ::string-cursor
             (if (= end -1) (as string-cursor (str:length))
                 (string-cursor-next str cstart (- end start)))))
    (string-cursor-for-each proc str cstart cend)))
  
(define (string-for-each proc str1::string #!rest rst::object[])::void
  validate-apply: "kawa.lib.compile_map:stringForEachValidateApply"
  (define nrst rst:length)
  (cond ((= nrst 0)
         (string-cursor-for-each proc str1))
        ((and (< nrst 3) (not (instance? (rst 0) string)))
         (srfi-13-string-for-each proc str1 (rst 0)
                           (if (= nrst 2) (rst 1) -1)))
        (else
         (define n::int (+ nrst 1))
         (define cursors::string-cursor[] (string-cursor[] length: n))
         (define ends::string-cursor[] (string-cursor[] length: n))
         (define chs::gnu.text.Char[] (gnu.text.Char[] length: n))
         (set! (cursors 0) (string-cursor-start str1))
         (set! (ends 0) (string-cursor-end str1))
         (do ((i ::int 1 (+ i 1))) ((>= i n))
           (let ((str ::string (rst (- i 1))))
             (set! (cursors i) (string-cursor-start str))
             (set! (ends i) (string-cursor-end str))))
         (let loop1 ()
           (let loop2 ((i::int 0))
             (cond ((= i n)
                    (proc @chs)
                    (loop1))
                   (else
                    (define curs-i (cursors i))
                    (define end-i (ends i))
                    (define str ::string (if (= i 0) str1 (rst (- i 1))))
                    (cond ((string-cursor<? curs-i end-i)
                           (set! (chs i) (string-cursor-ref str curs-i))
                           (set! (cursors i) (string-cursor-next str curs-i))
                           (loop2 (+ i 1)))))))))))

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
             (let ((ch::character (proc @chs)))
               (result:appendCharacter (as int ch)))
             (loop1))
            (else
             (define curs-i (cursors i))
             (define end-i (ends i))
             (define str ::string (if (= i 0) str1 (rst (- i 1))))
             (cond ((string-cursor<? curs-i end-i)
                    (set! (chs i) (string-cursor-ref str curs-i))
                    (set! (cursors i) (string-cursor-next str curs-i))
                    (loop2 (+ i 1))))))))
  ;; FIXME should be IString
  result)

(define (string-for-each-index proc str::string #!optional
                                 (start::int 0) (end::int -1))
  ::void
  (let* ((cstart ::string-cursor
                 (string-cursor-next str
                                     (as string-cursor 0)
                                     start))
         (cend ::string-cursor
             (if (= end -1) (as string-cursor (str:length))
                 (string-cursor-next str cstart (- end start)))))
    (do ((cursor::string-cursor cstart
                                (string-cursor-next str cursor))
         (i ::int start (+ i 1)))
        ((string-cursor>=? cursor cend))
      (proc i))))

(define (string-fold kons knil str::string #!optional
                     (start::int 0) (end::int -1))
  (let* ((cstart ::string-cursor
                 (string-cursor-next str
                                     (as string-cursor 0)
                                     start))
         (cend ::string-cursor
             (if (= end -1) (as string-cursor (str:length))
                 (string-cursor-next str cstart (- end start))))
         (result knil))
    (do ((cursor::string-cursor cstart
                                (string-cursor-next str cursor)))
        ((string-cursor>=? cursor cend)
         result)
      (set! result (kons (string-cursor-ref str cursor) result)))))

(define (string-fold-right kons knil str::string #!optional
                     (start::int 0) (end::int 0 supplied-end))
  (with-start-end 
   str (start end supplied-end) (cstart cend)
   (let loop ((result knil)
              (i ::string-cursor (as string-cursor cend)))
     (if (string-cursor<=? i (as string-cursor start))
         result
         (let ((prev (string-cursor-prev str i)))
           (loop (kons (string-cursor-ref str prev) result) prev))))))

(define (string-map-index proc str::string #!optional
                          (start::int 0) (end::int -1))
  (let* ((cstart ::string-cursor
                 (string-cursor-next str
                                     (as string-cursor 0)
                                     start))
         (cend ::string-cursor
             (if (= end -1) (as string-cursor (str:length))
                 (string-cursor-next str cstart (- end start))))
         (result (gnu.lists.FString)))
    (do ((cursor::string-cursor cstart
                                (string-cursor-next str cursor))
         (i ::int start (+ i 1)))
        ((string-cursor>=? cursor cend)
         result)
      (result:append (proc i)))))

(define (string-append! str::gnu.lists.FString #!rest args :: object[]) ::void
  validate-apply: "kawa.lib.compile_misc:stringAppendToValidateApply"
  (let ((len args:length))
    (do ((i ::int 0 (+ i 1)))
      ((>= i len))
      (str:append (args i)))))

(define (string-replicate str::string
                          #!optional (from::int 0) (to::int 0 supplied-to)
                          (start::int 0) (end::int 0 supplied-end))
  ::string
  (let* ((end (if supplied-end end (gnu.lists.Strings:sizeInCodePoints str))))
    (gnu.lists.IString
     (gnu.lists.Strings:replicate from to supplied-to str start end supplied-end))))

(define (string->utf8
         (v ::string)
         #!optional
         (start ::int 0)
         (end ::int 0 supplied-end))
  ::bytevector
  (with-start-end
   v (start end supplied-end) (cstart cend)
   (gnu.lists.U8Vector
    (((v:toString):substring cstart cend):getBytes
     (cond-expand (java-7 java.nio.charset.StandardCharsets:UTF_8)
                  (else "UTF-8"))))))

(define (string->utf16
         (v ::string)
         #!optional
         (start ::int 0)
         (end ::int 0 supplied-end))
  ::bytevector  (with-start-end
   v (start end supplied-end) (cstart cend)
   (gnu.lists.U8Vector
    (gnu.lists.Strings:toUtf16 v cstart cend
                               (cond-expand (big-endian #t) (else #f))
                               #t))))

(define (string->utf16be
         (v ::string)
         #!optional
         (start ::int 0)
         (end ::int 0 supplied-end))
  ::bytevector  (with-start-end
   v (start end supplied-end) (cstart cend)
   (gnu.lists.U8Vector
    (gnu.lists.Strings:toUtf16 v cstart cend #t #f))))

(define (string->utf16le
         (v ::string)
         #!optional
         (start ::int 0)
         (end ::int 0 supplied-end))
  ::bytevector  (with-start-end
   v (start end supplied-end) (cstart cend)
   (gnu.lists.U8Vector
    (gnu.lists.Strings:toUtf16 v cstart cend #f #f))))

(define (string-tabulate proc (len ::int))
  (let ((result (gnu.lists.FString:alloc len)))
    (do ((i ::int 0 (+ i 1)))
        ((>= i len)
         (gnu.lists.IString result))
      (result:append (proc i)))))

(define (string-unfold p f g seed #!optional (base "") (make-final #!null))
  (let ((result (gnu.lists.FString)))
    (result:append base)
    (let recur ((seed seed))
      (cond ((p seed)
             (if (not (eq? make-final #!null))
                 (result:append (make-final seed))))
            (else
             (result:append (f seed))
             (recur (g seed)))))
    (gnu.lists.IString result)))

(define (string-unfold-right p f g seed #!optional (base "") (make-final #!null))
  (let ((result (gnu.lists.FString)))
    (result:prepend base)
    (let recur ((seed seed))
      (cond ((p seed)
             (if (not (eq? make-final #!null))
                 (result:prepend (make-final seed))))
            (else
             (result:prepend (f seed))
             (recur (g seed)))))
    (gnu.lists.IString result)))
