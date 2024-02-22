include(CheckIncludeFile)
include(FindMPI)

add_library(cta SHARED
  cta_array.c
  cta_datatypes.c
  cta_datetime.c
  cta_defaults.c
  cta_file.c
  cta_flush.c
  cta_functions.c
  cta_handles.c
  cta_initialise.c
  cta_interface.c
  cta_matrix_blas.c
  cta_matrix.c
  cta_mem.c
  cta_message.c
  cta_metainfo.c
  cta_method.c
  cta_modbuild_par.c
  cta_modbuild_sp.c
  cta_model_factory.c
  cta_model_utilities.c
  cta_model.c
  cta_obsdescr_combine.c
  cta_obsdescr_maori.c
  cta_obsdescr_netcdf.c
  cta_obsdescr_table.c
  cta_obsdescr_user.c
  cta_obsdescr.c
  cta_pack.c
  cta_par.c
  cta_reltable.c
  cta_resultwriter.c
  cta_sobs_combine.c
  cta_sobs_factory.c
  cta_sobs_maori.c
  cta_sobs_netcdf.c
  cta_sobs_user.c
  cta_sobs.c
  cta_string.c
  cta_time.c
  cta_tree.c
  cta_treevector.c
  cta_util_methods.c
  cta_util_sort.c
  cta_util_statistics.c
  cta_vector_blas.c
  cta_vector.c
  cta_xml.c
  f_cta_defaults.c
  f_cta_utils.c
)
set_target_properties(cta PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
)

# Set preprocessor definitions.
if(MPI_FOUND)
  target_compile_definitions(cta PRIVATE USE_MPI)
endif()

CHECK_INCLUDE_FILE("dlfcn.h" HAVE_DLFCN_H)
if(HAVE_DLFCN_H)
  target_compile_definitions(cta PRIVATE HAVE_DLFCN_H)
endif()

# Because we define NetCDF as a required dependency, we should always set
# HAVE_LIBNETCDF.
target_compile_definitions(cta PRIVATE HAVE_LIBNETCDF)

if(WIN32)
  # Set CTALIB preprocessor define so that the appropriate dllexport attributes
  # are set under Windows.
  target_compile_definitions(cta PRIVATE CTALIB)
  # Call BLAS and LAPACK subroutines with capitalised names.
  target_compile_definitions(cta PRIVATE FTN_CAPITAL)
endif()

target_include_directories(cta PRIVATE
  ../include
  ${LIBXML2_INCLUDE_DIR}
  ${MPI_C_INCLUDE_DIRS}
  ${NETCDF_INCLUDE_DIRS}
)
if(WIN32)
  target_include_directories(cta PRIVATE ${PTHREADS_WIN32_INCLUDE_DIRS})
endif()

target_link_libraries(cta PRIVATE blas
  lapack
  LibXml2::LibXml2
  MPI::MPI_C
  NetCDF::NetCDF
)
if(WIN32)
  target_link_libraries(cta PRIVATE pthreads-win32::pthreads-win32)
endif()
