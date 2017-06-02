lib_LTLIBRARIES = libopendabridge.la
libopendabridge_la_SOURCES = \
jni_cta_utils.cpp \
org_openda_costa_CtaObject.cpp \
org_openda_costa_CtaObservationDescriptions.cpp \
org_openda_costa_CtaOpenDaModel.cpp \
org_openda_costa_CtaRelationTable.cpp \
org_openda_costa_CtaStochObserver.cpp \
org_openda_costa_CtaTime.cpp \
org_openda_costa_CtaTreeVector.cpp \
org_openda_costa_CtaVector.cpp \
org_openda_costa_CtaParallel.cpp \
org_openda_costa_CtaModelState.cpp \
org_openda_costa_CtaArray.cpp \
org_openda_costa_CtaUtils.cpp \
jni_cta_CtaObsdescr_NativeToJava.c \
org_openda_costa_CtaInitialize.cpp \
org_openda_resultwriters_NativeResultWriter.cpp \
org_openda_resultwriters_NetcdfResultWriterNative.cpp

libopendabridge_la_LIBADD = @LTLIBOBJS@
libopendabridge_la_LDFLAGS = -no-undefined -version-info 0:0:0  -lcta ${LAPACK_LIBS} ${BLAS_LIBS} ${NETCDF_LIBS}

AM_CPPFLAGS =    -I$(srcdir)/openda_include -I$(top_srcdir)/include

