#bin_PROGRAMS = pollute2d_worker pollute2d_ww
#pollute2d_worker_SOURCES = pollute2d_worker.f90
#pollute2d_worker_LDADD = -lpollute2d -lcta 

#pollute2d_ww_SOURCES = pollute2d_ww.f90
#pollute2d_ww_LDADD = -lpollute2d -lcta 

lib_LTLIBRARIES = libpollute2d.la
libpollute2d_la_SOURCES =pollute2d.f90 
                      
libpollute2d_la_LIBADD = @LTLIBOBJS@
libpollute2d_la_LDFLAGS = -version-info 0:0:0 -lcta

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_FFLAGS = -I$(top_srcdir)/include
AM_FCFLAGS = -I$(top_srcdir)/include

