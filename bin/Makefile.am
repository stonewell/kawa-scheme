if ENABLE_KAWA_FRONTEND
bin_PROGRAMS = kawa cgi-servlet
else
bin_PROGRAMS = cgi-servlet
endif

READLINE_INCLUDE_PATH =
READLINE_LIB_PATH =
kawa_EXTRA_LIBS = -lncurses  # For Linux
#kawa_EXTRA_LIBS = -lcurses -lsocket # For Solaris2
AM_CPPFLAGS = $(READLINE_INCLUDE_PATH)
kawa_LDADD = $(READLINE_LIB_PATH) -lreadline $(LIBHISTORY) $(kawa_EXTRA_LIBS)

kawa_SOURCES = kawa.c kawapath.c
cgi_servlet_SOURCES = cgi-servlet.c kawapath.c

kawa$(EXEEXT): $(kawa_OBJECTS)
	$(CCLD) -o kawa$(EXEEXT) $(kawa_LDFLAGS) $(kawa_OBJECTS) $(kawa_LDADD) $(LIBS)

cgi-servlet$(EXEEXT): $(cgi_servlet_OBJECTS)
	$(CCLD) -o cgi-servlet$(EXEEXT) $(cgi_servlet_LDFLAGS) $(cgi_servlet_OBJECTS) $(LIBS)

KAWAJAR = kawa.jar
KAWALIB = $(datadir)/kawa/lib/$(KAWAJAR)
CDEFS = -DJAVA='"$(JAVA)"' -DKAWALIB='"$(KAWALIB)"' -DKAWAJAR='"$(KAWAJAR)"' -DEXTRAPATH='"@conf_classpath@"'
DEFS = $(CDEFS)

CLEANFILES = kawa$(EXEEXT) tmp *.stamp

binPROGRAMS_INSTALL = $(INSTALL_PROGRAM)

install-binPROGRAMS: $(bin_PROGRAMS)
	@$(NORMAL_INSTALL)
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
if ENABLE_KAWA_FRONTEND
	$(INSTALL_PROGRAM_ENV) $(binPROGRAMS_INSTALL) kawa$(EXEEXT) $(DESTDIR)$(bindir)/kawa$(EXEEXT)
endif
if ENABLE_SERVLET
	$(INSTALL_PROGRAM_ENV) $(binPROGRAMS_INSTALL) cgi-servlet$(EXEEXT) $(DESTDIR)$(bindir)/cgi-servlet$(EXEEXT)
endif
