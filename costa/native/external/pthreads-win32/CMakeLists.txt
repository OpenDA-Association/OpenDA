if(NOT WIN32)
  message(FATAL_ERROR "pthreads-win32 is only available on Windows.")
endif()

set(PTHREADS_WIN32_LIBRARY "${PTHREADS_WIN32_ROOT}/lib/pthreadVC2.lib")
set(PTHREADS_WIN32_DLL "${PTHREADS_WIN32_ROOT}/bin/pthreadVC2.dll")

# Check whether the files exist.
if(NOT EXISTS ${PTHREADS_WIN32_LIBRARY})
  message(FATAL_ERROR "pthreads-win32 library \"${PTHREADS_WIN32_LIBRARY}\" does not exist.")
endif()
if (WIN32 AND NOT EXISTS ${PTHREADS_WIN32_DLL})
  message(FATAL_ERROR "pthreads-win32 DLL \"${PTHREADS_WIN32_DLL}\" does not exist.")
endif()

# Create imported target.
add_library(pthreads-win32::pthreads-win32 SHARED IMPORTED GLOBAL)
set_target_properties(pthreads-win32::pthreads-win32 PROPERTIES
  IMPORTED_IMPLIB ${PTHREADS_WIN32_LIBRARY}
  IMPORTED_LOCATION ${PTHREADS_WIN32_DLL}
)

# Make include directories available to parent scope.
set(PTHREADS_WIN32_INCLUDE_DIRS "${PTHREADS_WIN32_ROOT}/include" PARENT_SCOPE)

message(STATUS "Found pthreads-win32: ${PTHREADS_WIN32_LIBRARY}")
