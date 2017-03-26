;; -*- coding: utf-8 -*-

(test-begin "text")

(test-equal #\space (integer->char 32))
(test-equal 5000 (char->integer (integer->char 5000)))
;; If strict:
;; (test-error (integer->char #\xD800)   )
(test-equal #t (char<? #\z #\ß))
(test-equal #f (char<? #\z #\Z))
(test-equal #t (string<? "z" "ß"))
(test-equal #t (string<? "z" "zz"))
(test-equal #f (string<? "z" "Z"))

(test-equal #f (string=? "Straße" "Strasse"))

(test-equal #\I (char-upcase #\i))
(test-equal #\i (char-downcase #\i))
(test-equal #\I (char-titlecase #\i))
(test-equal #\i (char-foldcase #\i))
(test-equal #\ß (char-upcase #\ß))
(test-equal #\ß (char-downcase #\ß))
(test-equal #\ß (char-titlecase #\ß))
(test-equal #\ß (char-foldcase #\ß))
(test-equal #\Σ (char-upcase #\Σ))
(test-equal #\σ (char-downcase #\Σ))
(test-equal #\Σ (char-titlecase #\Σ))
(test-equal #\σ (char-foldcase #\Σ))
(test-equal #\Σ (char-upcase #\ς))
(test-equal #\ς (char-downcase #\ς))
(test-equal #\Σ (char-titlecase #\ς))
(test-equal #\σ (char-foldcase #\ς))

(test-equal #t (char-alphabetic? #\a))
(test-equal #t (char-numeric? #\1))
(test-equal #t (char-whitespace? #\space))
(test-equal #t (char-whitespace? #\x00A0))
(test-equal #t (char-upper-case? #\Σ))
(test-equal #t (char-lower-case? #\σ))
(test-equal #t (char-lower-case? #\x00AA))
(test-equal #f (char-title-case? #\I))
(test-equal #t (char-title-case? #\x01C5))

(test-equal 'Ll (char-general-category #\a))
(test-equal 'Zs (char-general-category #\space))
(test-equal 'Cn (char-general-category #\x10FFFF))

(test-equal "HI" (string-upcase "Hi"))
(test-equal "hi" (string-downcase "Hi"))
(test-equal "hi" (string-foldcase "Hi"))
(test-equal "STRASSE" (string-upcase "Straße"))
(test-equal "straße" (string-downcase "Straße"))
(test-equal "strasse" (string-foldcase "Straße"))
(test-equal "strasse" (string-downcase "STRASSE"))
(test-equal "σ" (string-downcase "Σ"))
(test-equal "ΧΑΟΣ" (string-upcase "ΧΑΟΣ"))
(test-equal "χαος" (string-downcase "ΧΑΟΣ"))
(test-equal "χαοσς" (string-downcase "ΧΑΟΣΣ"))
(test-equal "χαος σ" (string-downcase "ΧΑΟΣ Σ"))
(test-equal "χαοσσ" (string-foldcase "ΧΑΟΣΣ"))
(test-equal "ΧΑΟΣ" (string-upcase "χαος"))
(test-equal "ΧΑΟΣ" (string-upcase "χαοσ"))
(test-equal "Knock Knock" (string-titlecase "kNock KNoCK"))
(test-equal "Who's There?" (string-titlecase "who's there?"))
(test-equal "R6rs" (string-titlecase "r6rs"))
(test-equal "R6rs" (string-titlecase "R6RS"))
(test-expect-fail 1)
(test-equal "If\xFB01; Flat Fire"
	    (string-titlecase "if\xFB01; \xFB02;at \xFB01;re"))

(test-equal #f (string-ci<? "z" "Z"))
(test-equal #t (string-ci=? "z" "Z"))
(test-equal #t (string-ci=? "Straße" "Strasse"))
(test-equal #t (string-ci=? "Straße" "STRASSE"))
(test-equal #t (string-ci=? "ΧΑΟΣ" "χαοσ"))

(cond-expand (string-normalize-unicode)
             (else
              (test-expect-fail 4)))
(test-equal "\x65;\x301;" (string-normalize-nfd "\xE9;"))
(test-equal "\xE9;" (string-normalize-nfc "\xE9;"))
(test-equal "\x65;\x301;" (string-normalize-nfd "\x65;\x301;"))
(test-equal "\xE9;" (string-normalize-nfc "\x65;\x301;"))

(define str1 "a😂b😼c")
(test-equal 5 (string-length str1))
(test-equal #\c (string-ref str1 4))
(test-equal #\😼 (string-ref str1 3))
(test-equal "😼bc😂" (str1 [3 2 4 1]))
(test-equal "😂b😼" (str1 [1 <: 4]))
(define str1lst '())
(string-for-each (lambda (x)
                   (set! str1lst (cons (char->integer x) str1lst)))
                 str1)
(test-equal '(97 128514 98 128572 99) (reverse str1lst))

(test-equal "😂b😼" (string-copy str1 1 4))
;; Test various cominations of replacing characters that are 1-char
;; or 2-char (i.e. surrogate pairs).
(define strx2 (string-copy str1 0))
(test-equal str1 strx2)
(test-equal "😼bc😂" (strx2 [3 2 4 1]))
(test-equal "😂b😼" (strx2 [1 <: 4]))
(string-set! strx2 3 #\y)
(test-equal "a😂byc" strx2)
(string-set! strx2 2 #\x)
(test-equal "a😂xyc" strx2)
(string-set! strx2 4 #\😂)
(test-equal "a😂xy😂" strx2)
(string-set! strx2 1 #\😼)
(test-equal "a😼xy😂" strx2)

(let ((str (make-string 3 #\😂)))
  (test-equal 3 (string-length str))
  (test-equal #\😂 (string-ref str 2)))

(test-equal "cdefab" (string-replicate "abcdef" 2 8))
(test-equal "efabcd" (string-replicate "abcdef" -2 4))
(test-equal "abcabca" (string-replicate "abc" 0 7))
(test-equal "y😂a😼xy" (string-replicate "a😼xy😂" 3 9))
(test-equal "y😂a😼" (string-replicate "a😼xy😂" -2 2))

(let ((str '()))
  (string-for-each (lambda (x y)
                     (set! str (cons (char->integer x) str))
                     (set! str (cons (char->integer y) str)))
                   str1 "ABC")
  (test-equal '(97 65 128514 66 98 67) (reverse str)))

(let ((str '()))
  ;; SRFI-13 extension
  (string-for-each (lambda (x)
                     (set! str (cons (char->integer x) str)))
                   str1 1 4)
  (test-equal '(128514 98 128572) (reverse str)))

(define (translate-space-to-newline str::string)::string
  (let ((result (make-string 0)))
    (string-for-each
     (lambda (ch)
       (string-append! result
                       (if (char=? ch #\Space) #\Newline ch)))
     str)
    result))
(test-equal "ab\ncd\nx"
            (translate-space-to-newline "ab cd x"))

(let ((str (make-string 3 #\😂)))
  (test-equal 3 (string-length str))
  (test-equal 6 (str:length))
  (string-replace! str 1 2 "abc")
  (test-equal "😂abc😂" str)
  (string-replace! str 5 5 str 3)
  (test-equal "😂abc😂c😂" str)
  (string-replace! str 0 2 "ABC" 1 2)
  (test-equal "Bbc😂c😂" str)
  (test-equal 6 (length str))
  (test-equal #\c (str 2))
  (test-equal #\x1f602 (str 3))
  (test-equal #\c (str 4)))

(test-equal "" (reverse-list->string '()))
(test-equal "cba" (reverse-list->string '(#\a #\b #\c)))
(test-equal "c😼b😂a" (reverse-list->string '(#\a #\😂 #\b #\😼 #\c)))

(test-equal "" (string-concatenate '()))
(test-equal "abcdef"
            (string-concatenate '("" "a" "bcd" "" "ef" "" "")))

(test-equal "" (string-concatenate-reverse '()))

(test-equal "efbcda"
            (string-concatenate-reverse
             '("" "a" "bcd" "" "ef" "" "")))

(test-equal "huh?"
            (string-concatenate-reverse '() "huh?"))

(test-equal "efbcdaxy"
            (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))

(test-equal "huh"
            (string-concatenate-reverse '() "huh?" 3))

(test-equal "efbcdax"
            (string-concatenate-reverse
             '("" "a" "bcd" "" "ef" "" "") "x" 1))

(test-equal "foo bar baz" (string-join '("foo" "bar" "baz")))
(test-equal "foobarbaz" (string-join '("foo" "bar" "baz") ""))
(test-equal "foo:bar:baz" (string-join '("foo" "bar" "baz") ":"))
(test-equal "foo:bar:baz:" (string-join '("foo" "bar" "baz") ":" 'suffix))
(test-equal "" (string-join '() ":"))
(test-equal "" (string-join '("") ":"))
(test-equal "" (string-join '()  ":" 'infix))
(test-error (string-join '()  ":" 'strict-infix))
(test-equal "A" (string-join '("A")  ":" 'strict-infix))
(test-equal "A:B" (string-join '("A" "B")  ":" 'strict-infix))
(test-equal "" (string-join '()  ":" 'suffix))
(test-equal ":" (string-join '("") ":" 'suffix))

(let ((str1 (string-copy "abcdef")))
  (test-equal "ef" (str1 [4 <:]))
  (test-equal "bfdc" (str1 [1 5 3 2]))
  (test-equal "cbd" ((str1 [1 5 3 2]) [3 0 2]))
  (test-equal "bcde" (str1 [1 <: 5]))
  (test-equal "edbd" (str1 [4 3 1 3]))
  (test-equal "bcd" (str1 [1 <=: 3]))
  (set! (str1 2) #\D)
  (test-equal "abDdef" str1)
  (test-equal "bDd" (str1 [1 <: 4]))
  (set! (str1 [2 <: 2]) (str1 [1 <: 4]))
  (test-equal "abbDdDdef" str1)
  (! str2 (str1 [8 2 4 0 5]))
  (! str3 (array-index-share str1 [8 2 4 0 5]))
  (test-equal "gnu.lists.FString" (invoke (invoke str1 'getClass) 'getName))
  (test-error (set! (str2 4) #\x))
  (set! (str3 4) #\x)
  (test-equal "fbdaD" str2)
  (test-equal "abbDdxdef" str1))

(test-begin "pretty-printing")

(import (kawa pprint))

(define (format-pretty form width)
  (fluid-let ((*print-right-margin* width))
    (! swr (java.io.StringWriter))
    (! out (gnu.kawa.io.OutPort swr #t #f))
    (out:setPrettyPrinting #t)
    (pprint form out)
    (out:close)
    (swr:toString)))

(define-syntax test-pretty-print
  (syntax-rules ()
    ((_ form width expected)
     (test-equal expected (format-pretty form width)))))

(define form-1
  '(define-private (foo fdsf add) (list b 23) (let ((xy (+ dadasd asdasd)) (xz 12)) (list b 22) ABCD (vector 42343 23423423 234324 989))))

(test-pretty-print form-1 30 &{
    &|(define-private (foo fdsf
    &|                 add)
    &|  (list b 23)
    &|  (let ((xy
    &|         (+ dadasd asdasd))
    &|        (xz 12))
    &|    (list b 22)
    &|    ABCD
    &|    (vector 42343 23423423
    &|     234324 989)))})

(test-pretty-print form-1 50 &{
    &|(define-private (foo fdsf add)
    &|  (list b 23)
    &|  (let ((xy (+ dadasd asdasd)) (xz 12))
    &|    (list b 22)
    &|    ABCD
    &|    (vector 42343 23423423 234324 989)))})

(define form-2
  '(if (equal? fdfds sdfsdf) (cond (aa (list bb)) ((null? cc) dd)) (vector xx sxasxs (+ 454 435) dsadd)))

(test-pretty-print form-2 20 &{
    &|(if (equal? fdfds
    &|     sdfsdf)
    &|    (cond (aa
    &|           (list
    &|            bb))
    &|          ((null?
    &|            cc)
    &|           dd))
    &|    (vector xx
    &|     sxasxs
    &|     (+ 454 435)
    &|     dsadd))})

(test-pretty-print form-2 40 &{
    &|(if (equal? fdfds sdfsdf)
    &|    (cond (aa (list bb))
    &|          ((null? cc) dd))
    &|    (vector xx sxasxs (+ 454 435)
    &|     dsadd))})

(test-pretty-print form-2 80 &{
    &|(if (equal? fdfds sdfsdf)
    &|    (cond (aa (list bb)) ((null? cc) dd))
    &|    (vector xx sxasxs (+ 454 435) dsadd))})

(test-pretty-print form-2 200 &{
    &|(if (equal? fdfds sdfsdf) (cond (aa (list bb)) ((null? cc) dd)) (vector xx sxasxs (+ 454 435) dsadd))})

(define form-3
  '(if (equal? xyz (list asdsads bccc)) (list xyz xyz) (list 987 xy)))

(test-pretty-print form-3 200 &{
    &|(if (equal? xyz (list asdsads bccc)) (list xyz xyz) (list 987 xy))})

(test-pretty-print form-3 40 &{
    &|(if (equal? xyz (list asdsads bccc))
    &|    (list xyz xyz)
    &|    (list 987 xy))})

(test-pretty-print form-3 30 &{
    &|(if (equal? xyz
    &|     (list asdsads bccc))
    &|    (list xyz xyz)
    &|    (list 987 xy))})

(test-end)

(test-end)
