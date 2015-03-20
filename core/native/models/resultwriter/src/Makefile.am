lib_LTLIBRARIES = libresultwriter.la
libresultwriter_la_SOURCES = resultwriter.f90

libresultwriter_la_LIBADD = @LTLIBOBJS@
libresultwriter_la_LDFLAGS = -version-info 0:0:0  -lcta
INCLUDES = -I$(top_srcdir)/include
#AM_FCFLAGS = -I$(top_srcdir)/src/cta_f90/generated
AM_FCFLAGS = -I$(top_builddir)/src/cta_f90/generated

