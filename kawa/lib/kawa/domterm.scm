(module-name (kawa domterm))

(require <kawa.lib.prim_imports>)

(define (domterm-load-stylesheet (styles::string) #!optional (name::string "Kawa")) ::void
  (kawa.DomTermBackend:loadStyleSheet name styles))
