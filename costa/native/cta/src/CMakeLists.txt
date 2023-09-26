cmake_minimum_required(VERSION 3.9.1)
# Check for the presence of dlfcn.h
include(CheckIncludeFile)
include(FindMPI)

if(MPI_FOUND)
  message("Found MPI: ${MPI_FOUND}\n")
  add_definitions(-DUSE_MPI)
endif()

# Perform the check
CHECK_INCLUDE_FILE("dlfcn.h" HAVE_DLFCN_H)
if(HAVE_DLFCN_H)
    add_definitions(-DHAVE_DLFCN_H)
endif()

find_package(LibXml2 REQUIRED)
find_package(SQLite3 REQUIRED)

string(REPLACE ":" ";" LIBRARY_DIRS $ENV{LD_LIBRARY_PATH})
find_library (NETCDF_LIBRARIES libnetcdf.so PATHS ${LIBRARY_DIRS})

set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/lib)
set(SOURCES
  cta_array.c       cta_matrix_blas.c      cta_obsdescr_combine.c  cta_sobs_combine.c  cta_util_sort.c
  cta_datatypes.c   cta_mem.c              cta_obsdescr_maori.c    cta_sobs_factory.c  cta_util_sqlite3.c
  cta_datetime.c    cta_message.c          cta_obsdescr_netcdf.c   cta_sobs_maori.c    cta_util_statistics.c
  cta_defaults.c    cta_metainfo.c         cta_obsdescr_sqlite3.c  cta_sobs_netcdf.c   cta_vector.c
  cta_file.c        cta_method.c           cta_obsdescr_table.c    cta_sobs_sqlite3.c  cta_vector_blas.c
  cta_flush.c       cta_modbuild_par.c     cta_obsdescr_user.c     cta_sobs_user.c     cta_xml.c
  cta_functions.c   cta_modbuild_sp.c      cta_pack.c              cta_string.c        f_cta_defaults.c
  cta_handles.c     cta_model.c            cta_par.c               cta_time.c          f_cta_utils.c
  cta_initialise.c  cta_model_factory.c    cta_reltable.c          cta_tree.c
  cta_interface.c   cta_model_utilities.c  cta_resultwriter.c      cta_treevector.c
  cta_matrix.c      cta_obsdescr.c         cta_sobs.c              cta_util_methods.c 
)

add_library(cta SHARED ${SOURCES})
file(GLOB HEADER_FILES
  "../include/*.h"
)

set(CTA_PUBLIC_HEADERS ${HEADER_FILES})

target_include_directories(cta PRIVATE ../include)
target_include_directories(cta PRIVATE ${CMAKE_SOURCE_DIR}/external)
target_include_directories(cta PRIVATE ${LIBXML2_INCLUDE_DIR})
target_include_directories(cta PRIVATE ${SQLite3_INCLUDE_DIRS}) 

target_link_libraries(cta PRIVATE ${LIBXML2_LIBRARIES})
target_link_libraries(cta PRIVATE ${SQLite3_LIBRARIES})
target_link_libraries(cta PRIVATE ${NETCDF_LIBRARIES})

target_link_libraries(cta PRIVATE blas)
target_link_libraries(cta PRIVATE lapack)


