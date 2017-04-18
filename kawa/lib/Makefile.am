## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

# NOTE if you update this list, also update build.xml.
java_SCM_ALWAYS1 =\
  prim_imports.scm \
  std_syntax.scm \
  reflection.scm \
  syntax.scm \
  lists.scm \
  case_syntax.scm \
  DefineRecordType.scm \
  ExceptionClasses.scm \
  exceptions.scm \
  kawa/expressions.scm \
  compile_misc.scm compile_map.scm \
  thread.scm \
  characters.scm \
  keywords.scm \
  numbers.scm \
  strings_syntax.scm strings.scm \
  parameters.scm \
  parameterize.scm \
  ports.scm \
  files.scm \
  misc.scm \
  misc_syntax.scm \
  vectors.scm \
  uniform.scm \
  bytevectors.scm \
  arrays.scm \
  system.scm \
  kawa/istrings.scm kawa/mstrings.scm \
  kawa/string-cursors.scm kawa/arglist.scm \
  kawa/process-keywords.scm \
  kawa/hashtable.scm kawa/regex.scm \
  scheme/base.scm scheme/case-lambda.scm scheme/char.scm \
  scheme/complex.scm scheme/cxr.scm scheme/eval.scm scheme/file.scm \
  scheme/inexact.scm scheme/lazy.scm scheme/load.scm \
  scheme/process-context.scm scheme/read.scm scheme/repl.scm scheme/time.scm \
  scheme/write.scm scheme/r5rs.scm \
  trace.scm
# The following files may import (scheme base).
java_SCM_ALWAYS2 = enums.scm srfi/8.scm srfi/26.scm srfi/95.scm \
  rnrs/hashtables.scm rnrs/lists.scm rnrs/arithmetic/bitwise.scm \
  rnrs/unicode.scm rnrs/sorting.scm rnrs/programs.scm strings_ext.scm \
  kawa/base.scm
# The following files may import (kawa base).
java_SCM_ALWAYS3 = kawa/quaternions.scm kawa/pprint.scm kawa/rotations.scm \
  kawa/null-5.scm kawa/reflect.scm 

scm_WITH_SWING = windows.scm kawa/swing.scm
scm_WITH_AWT = kawa/pictures.scm 
scm_WITH_XQUERY = xquery/Xutils.scm
scm_WITH_DOMTERM = kawa/domterm.scm
EXTRA_DIST = $(java_SCM_ALWAYS1) $(java_SCM_ALWAYS2) $(java_SCM_ALWAYS3) \
  $(scm_WITH_AWT) $(scm_WITH_SWING) $(scm_WITH_XQUERY) $(scm_WITH_DOMTERM) \
  scheme/base-exports scheme/char-exports scheme/complex-exports \
  scheme/cxr-exports scheme/eval-exports scheme/file-exports \
  scheme/inexact-exports scheme/lazy-exports \
  scheme/process-context-exports scheme/time-exports scheme/write-exports

java_SCM1 = $(java_SCM_ALWAYS1)
java_SCM2 = $(java_SCM_ALWAYS2)
java_SCM3 = $(java_SCM_ALWAYS3)
if WITH_AWT
java_SCM2 += $(scm_WITH_AWT)
if WITH_SWING
java_SCM2 += $(scm_WITH_SWING)
endif
endif
if WITH_DOMTERM
java_SCM2 += $(scm_WITH_DOMTERM)
endif
if ENABLE_XML
if ENABLE_XQUERY
java_SCM2 += $(scm_WITH_XQUERY)
endif
endif

scm-classes1.stamp: $(java_SCM1)
	$(KAWA_COMPILER) --module-static-run $(SCM_COMPILE_FLAGS) -C $?
	echo timestamp > scm-classes1.stamp
	echo timestamp > $(TO_TOPDIR)/kawa-jar.stamp
scm-classes2.stamp: $(java_SCM2) | scm-classes1.stamp
	$(KAWA_COMPILER) --module-static-run $(SCM_COMPILE_FLAGS) -C $?
	echo timestamp > scm-classes2.stamp
	echo timestamp > $(TO_TOPDIR)/kawa-jar.stamp
scm-classes3.stamp: $(java_SCM3) | scm-classes2.stamp
	$(KAWA_COMPILER) --module-static-run $(SCM_COMPILE_FLAGS) -C $?
	echo timestamp > scm-classes3.stamp
	echo timestamp > $(TO_TOPDIR)/kawa-jar.stamp

all: Makefile scm-classes1.stamp scm-classes2.stamp scm-classes3.stamp

PACKAGE_FNAME = kawa-lib
SCM_COMPILE_FLAGS = --warn-undefined-variable --warn-unknown-member --warn-as-error
EXTRA_CLEAN = */*.class */*/*.class
