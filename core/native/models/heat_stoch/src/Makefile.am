lib_LTLIBRARIES = libheat_stoch.la
libheat_stoch_la_SOURCES = interpolate_bound_params.f usr_obs.f \
                          modelstep.f \
                          interpolate_noise_params.f cta_model.f \
                          Linear_FullState_Step.f

libheat_stoch_la_LIBADD = @LTLIBOBJS@
libheat_stoch_la_LDFLAGS = -version-info 0:0:0  -lcta
AM_CPPFLAGS = -I$(top_srcdir)/include
AM_FFLAGS = -I$(top_srcdir)/include

