cmake_minimum_required(VERSION 3.22.1)

project(openda
  LANGUAGES C CXX Fortran
)

option(BUILD_MODELS "Build example models" ON)

message(STATUS "------------------------------------------------------------")
message(STATUS "Build configuration:")
message(STATUS "    Build example models  ${BUILD_MODELS}")
message(STATUS "    CMAKE_INSTALL_PREFIX  ${CMAKE_INSTALL_PREFIX}")
if(DEFINED JAVA_HOME)
  message(STATUS "    JAVA_HOME             ${JAVA_HOME}")
else()
  message(STATUS "    JAVA_HOME             find automatically")
endif()
if(DEFINED LibXml2_ROOT)
  message(STATUS "    LibXml2_ROOT          ${LibXml2_ROOT}")
else()
  message(STATUS "    LibXml2_ROOT          find automatically")
endif()
if(DEFINED NetCDF_ROOT)
  message(STATUS "    NetCDF_ROOT           ${NetCDF_ROOT}")
else()
  message(STATUS "    NetCDF_ROOT           find automatically")
endif()
# On Windows, we need to find transitive dependencies of NetCDF and LibXml2 too,
# so that we can install the entire tree of DLL dependencies.
if(WIN32)
  message(STATUS "    PTHREADS_WIN32_ROOT   ${PTHREADS_WIN32_ROOT}")
endif()
message(STATUS "------------------------------------------------------------")

# Find JNI, LibXML2 and NetCDF.
find_package(JNI REQUIRED)
find_package(LibXml2 REQUIRED)
add_subdirectory(external/netcdf)
if(WIN32)
  # For Windows, we need Pthreads-win32.
  add_subdirectory(external/pthreads-win32)
endif()

# Build BLAS and LAPACK from sources.
add_subdirectory(external/blas)
add_subdirectory(external/lapack)

add_subdirectory(cta/src)
add_subdirectory(cta_f90/generated)
add_subdirectory(bridge/src)

if(${BUILD_MODELS})
  add_subdirectory(models)
endif()

# Install libraries.
install(TARGETS
  blas
  lapack
  cta
  cta_f90
  opendabridge
)
if(${BUILD_MODELS})
  install(TARGETS
    advec1d
    heat_stoch
    oscill
    pollute2d
    resultwriter
  )
endif()

# Install public headers.
file(GLOB CTA_PUBLIC_HEADERS
    "cta/include/*.h"
)
install(FILES ${CTA_PUBLIC_HEADERS}
    DESTINATION include
)
