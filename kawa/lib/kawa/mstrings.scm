(module-name (kawa mstrings))

(module-export string-append substring)

(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)
(import kawa.lib.strings)

(define (string-append #!rest (args :: java.lang.CharSequence[])) :: <gnu.lists.FString>
  (let ((str :: <gnu.lists.FString> (make <gnu.lists.FString>)))
    (invoke str 'addAllStrings args 0)
    str))

(define (substring str::<string> start::<int> end::<int>)
  :: <string>
  (string-copy str start end))
