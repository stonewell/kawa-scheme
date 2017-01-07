(module-name (kawa mstrings))

(module-export string-append)

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)

(define (string-append #!rest (args :: java.lang.CharSequence[])) :: <gnu.lists.FString>
  (let ((str :: <gnu.lists.FString> (make <gnu.lists.FString>)))
    (invoke str 'addAllStrings args 0)
    str))
