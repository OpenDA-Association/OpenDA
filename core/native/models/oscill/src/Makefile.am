lib_LTLIBRARIES = liboscill.la
liboscill_la_SOURCES = ode.f90 oscill.f90 c_oscill.c
                      
liboscill_la_LIBADD = @LTLIBOBJS@
liboscill_la_LDFLAGS = -version-info 0:0:0 -lcta

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_FFLAGS = -I$(top_srcdir)/include
AM_FCFLAGS = -I$(top_srcdir)/include

