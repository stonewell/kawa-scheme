(module-name (kawa istrings))

(module-export string-append)

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)

(define (string-append #!rest (args :: java.lang.CharSequence[])) :: gnu.lists.IString
  (let* ((buf (java.lang.StringBuilder))
         (n args:length))
    (let loop ((i::int 0))
      (cond ((< i n)
             (buf:append (args i))
             (loop (+ i 1)))))
    (gnu.lists.IString buf)))
