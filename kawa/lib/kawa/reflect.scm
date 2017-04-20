(define-library (kawa reflect)
  (export invoke
	  invoke-static
	  invoke-special
	  field
	  static-field
          set-field!
          set-static-field!
	  make
	  instance?
	  as
	  primitive-throw
          primitive-get-field primitive-set-field
          primitive-get-static primitive-set-static
          primitive-array-new primitive-array-get
          primitive-array-set primitive-array-length
          primitive-constructor
	  )
  (import
   kawa.lib.prim_imports
   kawa.lib.reflection

   (only (gnu kawa reflect SlotSet)
         (set$Mnfield$Ex set-field!)
         (set$Mnstatic$Mnfield$Ex set-static-field!))
   (only kawa.standard.Scheme (instanceOf instance?))

   (only (gnu kawa functions Convert)
	 as)
   ))
