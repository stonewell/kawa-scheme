;; Support for Emacs Lisp.
;; This will be moved to some other package, but for now there
;; are messy order-dependencies (gnu.* is made before kawa.lib).

(define (emacs:read #!optional (port (current-input-port)))
  (let ((lexer
	 ((primitive-constructor <gnu.elisp.ELispReader> (<input-port>))
	  port)))
    ((primitive-virtual-method <gnu.elisp.ELispReader> "readObject"
			       <object> ())
     lexer)))
