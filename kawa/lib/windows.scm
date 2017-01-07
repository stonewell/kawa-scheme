(require <kawa.lib.prim_imports>)
(require <kawa.lib.std_syntax>)
(require kawa.lib.misc)

(define (scheme-window #!optional share) :: <void>
  (let* ((language :: <gnu.expr.Language>
		 (kawa.standard.Scheme:getInstance))
	 (env :: <gnu.mapping.Environment>
	      (if share (interaction-environment)
		  (invoke language 'getNewEnvironment))))
    (make <kawa.GuiConsole> language env share)))
