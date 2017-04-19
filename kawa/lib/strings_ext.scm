(import
 kawa.lib.prim_imports
 kawa.lib.syntax
 kawa.lib.std_syntax
 kawa.lib.strings
 kawa.lib.case_syntax
 kawa.lib.exceptions
 kawa.lib.strings_syntax
 kawa.lib.kawa.string-cursors
 (rnrs unicode)
)

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

(define (string-count pred str::string #!optional
                      (start::int 0) (end::int 0 end-supplied))
  (let ((n ::int 0))
    (string-for-each-forwards
     (lambda (ch)
       (if (pred ch)
           (set! n (+ n 1))))
     str start end end-supplied)
    n))

(define (string-index str::string pred #!optional
                      (start::int 0) (end::int 0 end-supplied))
  (call/cc
   (lambda (exit)
     (let ((i ::int start))
       (string-for-each-forwards
        (lambda (ch)
          (if (pred ch)
              (exit i))
          (set! i (+ i 1)))
        str start end end-supplied))
     (exit #f))))

(define (string-index-right str::string pred #!optional
                            (start::int 0) (end::int 0 end-supplied))
  (call/cc
   (lambda (exit)
     (let ((i ::int start))
       (string-for-each-backwards
        (lambda (ch)
          (set! i (+ i 1))
          (if (pred ch)
              (exit (- (if end-supplied end (string-length str)) i))))
        str start end end-supplied))
     (exit #f))))

(define (string-skip str::string pred #!optional
                     (start::int 0) (end::int 0 end-supplied))
  (call/cc
   (lambda (exit)
     (let ((i ::int start))
       (string-for-each-forwards
        (lambda (ch)
          (if (not (pred ch))
              (exit i))
          (set! i (+ i 1)))
        str start end end-supplied))
     (exit #f))))

(define (string-skip-right str::string pred #!optional
                           (start::int 0) (end::int 0 end-supplied))
  (call/cc
   (lambda (exit)
     (let ((i ::int start))
       (string-for-each-backwards
        (lambda (ch)
          (set! i (+ i 1))
          (if (not (pred ch))
              (exit (- (if end-supplied end (string-length str)) i))))
        str start end end-supplied))
     (exit #f))))

(define (string-filter pred str::string #!optional
                       (start::int 0) (end::int 0 end-supplied))
  (let ((result (gnu.lists.FString)))
    (string-for-each-forwards
     (lambda (ch)
       (if (pred ch)
           (result:appendCharacter (as int ch))))
     str start end end-supplied)
    (gnu.lists.IString result)))

(define (string-remove pred str::string #!optional
                       (start::int 0) (end::int 0 end-supplied))
  (let ((result (gnu.lists.FString)))
    (string-for-each-forwards
     (lambda (ch)
       (if (not (pred ch))
           (result:appendCharacter (as int ch))))
     str start end end-supplied)
    (gnu.lists.IString result)))

(define (string-every pred str::string #!optional
                      (start::int 0) (end::int 0 end-supplied))
  (call/cc
   (lambda (exit)
     (let ((result #t))
       (string-for-each-forwards
        (lambda (ch)
          (set! result (pred ch))
          (if (not result)
              (exit #f)))
        str start end end-supplied)
       (exit result)))))

(define (string-any pred str::string #!optional
                    (start::int 0) (end::int 0 end-supplied))
  (call/cc
   (lambda (exit)
     (let ((result #t))
       (string-for-each-forwards
        (lambda (ch)
          (let ((result (pred ch)))
            (if result
                (exit result))))
        str start end end-supplied))
     (exit #f))))

(define (string-trim str::string
                     #!optional (pred char-whitespace?)
                     (start ::int 0)
                     (end ::int 0 end-supplied))
  (%string-trim #t #f str pred start end end-supplied))

(define (string-trim-right str::string
                     #!optional (pred char-whitespace?)
                     (start ::int 0)
                     (end ::int 0 end-supplied))
  (%string-trim #f #t str pred start end end-supplied))

(define (string-trim-both str ::string
                     #!optional (pred char-whitespace?)
                     (start ::int 0)
                     (end ::int 0 end-supplied))
  (%string-trim #t #t str pred start end end-supplied))

(define-private (%string-trim trim-left::boolean trim-right::boolean
                              str::string
                              pred start::int end::int end-supplied::boolean)
  (let* ((cstart ::string-cursor
                 (string-cursor-next str
                                     (as string-cursor 0)
                                     start))
         (cend ::string-cursor
               (if (not end-supplied) (as string-cursor (str:length))
                   (string-cursor-next str cstart (- end start))))
         (rstart ::string-cursor
                 (if trim-left
                     (let loop ((cursor::string-cursor cstart))
                       (cond ((string-cursor>=? cursor cend) cend)
                             ((pred (string-cursor-ref str cursor))
                              (loop (string-cursor-next str cursor)))
                             (else cursor)))
                     cstart))
         (rend ::string-cursor
               (if trim-right
                   (let loop ((cursor::string-cursor cend))
                     (if (string-cursor<=? cursor rstart) rstart
                         (let ((prev (string-cursor-prev str cursor)))
                           (if (pred (string-cursor-ref str prev))
                               (loop prev)
                               cursor))))
                   cend)))
    (gnu.lists.IString:valueOf (str:subSequence (as int rstart) (as int rend)))))

;; is string1 a prefix of string2 (using 16-bit char indexes)
(define-private (%string16-prefix? string1::string string2::string
                                   start1::int end1::int start2::int end2::int)
  (let loop ((i1 ::int start1) (i2 ::int start2))
    (cond ((= i1 end1) #t)
          ((= i2 end2) #f)
          ((char=? (string1:charAt i1) (string2:charAt i2))
           (loop (+ i1 1) (+ i2 1)))
          (else #f))))

(define (string-prefix? string1::string string2::string
                        #!optional
                        (start1::int 0) (end1::int 0 supplied-end1)
                        (start2::int 0) (end2::int 0 supplied-end2))::boolean
  (with-start-end string1 (start1 end1 supplied-end1) (cstart1 cend1)
    (with-start-end string2 (start2 end2 supplied-end2) (cstart2 cend2)
      (%string16-prefix? string1 string2 cstart1 cend1 cstart2 cend2))))

(define (string-suffix? string1::string string2::string
                        #!optional
                        (start1::int 0) (end1::int 0 supplied-end1)
                        (start2::int 0) (end2::int 0 supplied-end2))::boolean
  (with-start-end string1 (start1 end1 supplied-end1) (cstart1 cend1)
    (with-start-end string2 (start2 end2 supplied-end2) (cstart2 cend2)
      (let loop ((i1 ::int end1) (i2 ::int end2))
        (cond ((= i1 start1) #t)
              ((= i2 start2) #f)
              ((char=? (string1:charAt (- i1 1)) (string2:charAt (- i2 1)))
               (loop (- i1 1) (- i2 1)))
              (else #f))))))

(define (string-prefix-length string1::string string2::string
                              #!optional
                              (start1::int 0) (end1::int 0 supplied-end1)
                              (start2::int 0) (end2::int 0 supplied-end2))::int
  (with-start-end string1 (start1 end1 supplied-end1) (cstart1 cend1)
    (with-start-end string2 (start2 end2 supplied-end2) (cstart2 cend2)
      (let loop ((i1 ::int cstart1) (i2 ::int cstart2) (n ::int 0) (cx::int 0))
        (if (or (= i1 cend1) (= i2 cend2))
            n
            (let ((c1 ::int (string1:charAt i1)) (c2 ::int (string2:charAt i2)))
              (if (= c1 c2)
                  (loop (+ i1 1) (+ i2 1)
                        (+ n (if (and (> c1 #xDC00) (<= c1 #xDFFF)
                                      (> n 0)
                                      (> cx #xD800) (<= cx #xDBFF))
                                 0 1))
                        c1)
                  n)))))))

(define (string-suffix-length string1::string string2::string
                              #!optional
                              (start1::int 0) (end1::int 0 supplied-end1)
                              (start2::int 0) (end2::int 0 supplied-end2))::int
  (with-start-end string1 (start1 end1 supplied-end1) (cstart1 cend1)
    (with-start-end string2 (start2 end2 supplied-end2) (cstart2 cend2)
      (let loop ((i1 ::int cend1) (i2 ::int cend2) (n ::int 0) (cx::int 0))
        (if (or (= i1 cstart1) (= i2 cstart2))
            n
            (let ((c1 ::int (string1:charAt (- i1 1)))
                  (c2 ::int (string2:charAt (- i2 1))))
              (if (= c1 c2)
                  (loop (- i1 1) (- i2 1)
                        (+ n (if (and (> c1 #x8C00) (<= c1 #xDBFF)
                                      (> n 0)
                                      (> cx #xDC00) (<= cx #xDFFF))
                                 0 1))
                        c1)
                  n)))))))

(define (string-contains string1::string string2::string
                         #!optional
                         (start1::int 0) (end1::int 0 supplied-end1)
                         (start2::int 0) (end2::int 0 supplied-end2))
  (with-start-end string1 (start1 end1 supplied-end1) (cstart1 cend1)
    (with-start-end string2 (start2 end2 supplied-end2) (cstart2 cend2)
      (let ((limit (- cend1 (- cend2 cstart2))))
        (let loop ((i ::int cstart1) (r ::int start1))
          (cond ((> i limit) #f) 
                ((%string16-prefix? string2 string1 cstart2 cend2 cstart1 cend1)
                 r
                 (loop (+ i 1) (+ r 1))))))))) ;; FIXME adjust for surrogates
