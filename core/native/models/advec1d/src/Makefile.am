lib_LTLIBRARIES = libadvec1d.la


libadvec1d_la_SOURCES = advec_model.f cta_model.f
libadvec1d_la_LIBADD = @LTLIBOBJS@
libadvec1d_la_LDFLAGS = -version-info 0:0:0 -lcta

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_FFLAGS = -I$(top_srcdir)/include

