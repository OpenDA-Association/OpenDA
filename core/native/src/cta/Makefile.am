lib_LTLIBRARIES = libcta.la
libcta_la_SOURCES =    \
cta_mem.c              \
cta_datatypes.c        \
cta_matrix_blas.c      \
cta_time.c             \
cta_datetime.c         \
cta_defaults.c         \
cta_method.c           \
cta_tree.c             \
cta_file.c             \
cta_model.c            \
cta_flush.c            \
cta_obsdescr.c         \
cta_util_statistics.c  \
cta_functions.c        \
cta_vector.c           \
cta_handles.c          \
cta_treevector.c       \
cta_initialise.c       \
cta_sobs.c             \
cta_xml.c              \
cta_interface.c        \
f_cta_utils.c          \
cta_matrix.c           \
cta_string.c           \
cta_model_utilities.c  \
cta_modbuild_sp.c      \
cta_modbuild_par.c     \
cta_pack.c             \
cta_obsdescr_table.c   \
cta_metainfo.c         \
cta_reltable.c         \
cta_vector_blas.c      \
cta_util_sort.c        \
cta_util_methods.c     \
cta_par.c              \
cta_model_factory.c    \
cta_sobs_netcdf.c      \
cta_obsdescr_netcdf.c  \
cta_sobs_combine.c     \
cta_obsdescr_combine.c \
cta_sobs_factory.c     \
cta_message.c          \
cta_obsdescr_user.c    \
cta_sobs_user.c        \
cta_sobs_maori.c       \
cta_obsdescr_maori.c   \
cta_array.c            \
cta_resultwriter.c

libcta_la_LIBADD = @LTLIBOBJS@ @LTL_BLAS_ADD@ @LTL_LAPACK_ADD@
libcta_la_LDFLAGS = -version-info 0:0:0 ${BLASLIBS} ${LAPACK_LIBS}

AM_CPPFLAGS = -I$(top_srcdir)/include
