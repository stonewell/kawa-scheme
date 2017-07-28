;; Some tests for abuse of keywords
(import (kawa arglist))
(define f1 'key1:)
(define f2 key1:)
;; Diagnostic: bad-keywords1.scm:4:12: warning - keyword should be quoted if not in argument position

(arglist 0 k1: 3 k2: 5 8)

(arglist 0 k1: 3 4 k2: 5 8)
;; Diagnostic: bad-keywords1.scm:9:20: warning - keyword separated from other keyword arguments

(arglist 0 k1: 'k2: 5 8)
(arglist 0 k1: k2: 5 8)
;; Diagnostic: bad-keywords1.scm:13:16: warning - missing value after unquoted keyword

(arglist 0 k1: 3 'k2:)
(arglist 0 k1: 3 k2:)
;; Diagnostic: bad-keywords1.scm:17:18: warning - missing value after unquoted keyword

(arglist k1: k2: 5 9)
;; Diagnostic: bad-keywords1.scm:20:14: warning - missing value after unquoted keyword

(arglist 5 6 k2:)
;; Diagnostic: bad-keywords1.scm:23:14: warning - missing value after unquoted keyword
