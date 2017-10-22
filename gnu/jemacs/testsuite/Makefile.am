KAWALIB = ../../..
KAWA = ../../../bin/@KAWA_SCRIPT@
DOTEST = $(KAWA) --elisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

EXTRA_DIST = lang-test.el
CLEANFILES = *.log

check:
	$(DOTEST) -f "$(srcdir)/lang-test.el" -e '(test-report)'

all:
