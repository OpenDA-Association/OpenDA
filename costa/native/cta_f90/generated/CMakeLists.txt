cmake_minimum_required(VERSION 3.9.1)
enable_language(Fortran)

set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/lib)
set(SOURCES
cta_f90.f90             cta_f90_interface.f90      cta_f90_model_utilities.f90  cta_f90_time.f90
cta_f90_array.f90       cta_f90_matrix.f90         cta_f90_obsdescr.f90         cta_f90_tree.f90
cta_f90_datatypes.f90   cta_f90_mem.f90            cta_f90_pack.f90             cta_f90_treevector.f90
cta_f90_datetime.f90    cta_f90_message.f90        cta_f90_par.f90              cta_f90_util_methods.f90
cta_f90_file.f90        cta_f90_metainfo.f90       cta_f90_parameters.f90       cta_f90_util_sort.f90
cta_f90_flush_mod.f90   cta_f90_method.f90         cta_f90_reltable.f90         cta_f90_util_statistics.f90
cta_f90_functions.f90   cta_f90_modbuild_par.f90   cta_f90_resultwriter.f90     cta_f90_vector.f90
cta_f90_handles.f90     cta_f90_model.f90          cta_f90_sobs.f90             cta_f90_xml.f90
cta_f90_initialise.f90  cta_f90_model_factory.f90  cta_f90_string.f90
)

add_library(cta_f90 SHARED ${SOURCES})
target_include_directories(cta_f90 PRIVATE ../include)
target_link_libraries(cta_f90 PRIVATE cta)

