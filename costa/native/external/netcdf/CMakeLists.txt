if(DEFINED NetCDF_ROOT)
  if (NOT EXISTS ${NetCDF_ROOT})
    message(FATAL_ERROR "NetCDF root directory \"${NetCDF_ROOT}\" does not exist.")
  endif()

  set(NETCDF_INCLUDE_DIRS "${NetCDF_ROOT}/include")
  # Set path to the appropriate file depending on the platform.
  if(UNIX)
    set(NETCDF_LIBRARY "${NetCDF_ROOT}/lib/libnetcdf.so")
  elseif(WIN32)
    set(NETCDF_DLL "${NetCDF_ROOT}/bin/netcdf.dll")
    set(NETCDF_LIBRARY "${NetCDF_ROOT}/lib/netcdf.lib")
  else()
    message(FATAL_ERROR "Unsupported platform.")
  endif()

  # Check whether the files exist.
  if(NOT EXISTS ${NETCDF_LIBRARY})
    message(FATAL_ERROR "NetCDF library \"${NETCDF_LIBRARY}\" does not exist.")
  endif()
  if (WIN32 AND NOT EXISTS ${NETCDF_DLL})
    message(FATAL_ERROR "NetCDF DLL \"${NETCDF_DLL}\" does not exist.")
  endif()
else()
  if(NOT UNIX)
    message(FATAL_ERROR "NetCDF_ROOT must be defined on non-Unix platforms.")
  endif()
  # Find NetCDF library in the default places.
  find_library(NETCDF_LIBRARY
    NAMES netcdf libnetcdf
    HINTS ENV LD_LIBRARY_PATH
    REQUIRED
  )
  # Find include directory by finding "netcdf.h", then stripping off the
  # filename. It might be found near the library, or in a standard location.
  get_filename_component(NETCDF_LIBRARY_DIR ${NETCDF_LIBRARY} DIRECTORY)
  string(REPLACE "/lib" "/include" NETCDF_POSSIBLE_INCLUDE_DIR ${NETCDF_LIBRARY_DIR})
  find_file(NETCDF_INCLUDE_FILE
    NAMES netcdf.h
    HINTS ${NETCDF_POSSIBLE_INCLUDE_DIR}
    REQUIRED)
  get_filename_component(NETCDF_INCLUDE_DIRS ${NETCDF_INCLUDE_FILE} DIRECTORY)
endif()

message(STATUS "Found NetCDF: ${NETCDF_LIBRARY}")

# Create imported target.
add_library(NetCDF::NetCDF SHARED IMPORTED GLOBAL)
if(UNIX)
  set_target_properties(NetCDF::NetCDF PROPERTIES
    IMPORTED_LOCATION ${NETCDF_LIBRARY}
  )
elseif(WIN32)
  set_target_properties(NetCDF::NetCDF PROPERTIES
    IMPORTED_IMPLIB ${NETCDF_LIBRARY}
    IMPORTED_LOCATION ${NETCDF_DLL}
  )
endif()

# Make include directories available to parent scope.
set(NETCDF_INCLUDE_DIRS ${NETCDF_INCLUDE_DIRS} PARENT_SCOPE)
