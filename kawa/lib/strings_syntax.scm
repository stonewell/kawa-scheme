(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(import (kawa lib kawa string-cursors))

(define-syntax string-for-each-forwards
  (syntax-rules ()
    ((_ proc str start end end-supplied)
     (let* ((s ::string str)
            (cstart ::string-cursor
                    (string-cursor-next s
                                        (as string-cursor 0)
                                        start))
            (cend ::string-cursor
                  (if (not end-supplied) (as string-cursor (s:length))
                      (string-cursor-next s cstart (- end start)))))
       (string-cursor-for-each proc s cstart cend)))))

(define-syntax string-for-each-backwards
  (syntax-rules ()
    ((_ proc str start end end-supplied)
     (let* ((s ::string str)
            (cstart ::string-cursor
                    (string-cursor-next s
                                        (as string-cursor 0)
                                        start))
            (cend ::string-cursor
                  (if (not end-supplied) (as string-cursor (s:length))
                      (string-cursor-next s cstart (- end start)))))
       (let loop ((cursor::string-cursor cend))
         (cond ((string-cursor>? cursor cstart)
                (let ((prev (string-cursor-prev s cursor)))
                  (proc (string-cursor-ref s prev))
                  (loop prev)))))))))
