AUTOMAKE_OPTIONS = subdir-objects


lib_LTLIBRARIES = libsangoma.la
libsangoma_la_SOURCES = \
Fortran/sangoma_base.F90            \
Ewpf/user_base.f90           \
Ewpf/random_d.f90           \
Ewpf/equal_weights_step.f90 \
Ewpf/gen_random.f90         \
Ewpf/proposal_step.f90      \
Ewpf/quicksort.f90          \
Ewpf/sangoma_base.f90       \
Ewpf/subroutines_for_EWPF.f90 \
openda/oda_EWPF_wrapper.f90

#Ewpf/proposal_step.f90  \
#user_base.f90
#user_defined_callback_routines.f90

libsangoma_la_LIBADD = @LTLIBOBJS@ @LTL_BLAS_ADD@ @LTL_LAPACK_ADD@
libsangoma_la_LDFLAGS = -version-info 0:0:0 ${BLASLIBS} ${LAPACK_LIBS} 

AM_FCFLAGS=  -ffree-line-length-none  -I$(top_srcdir)/include

