#-----------------------------------------------------------------------------
# SZIP Config file for compiling against SZIP install directory
#-----------------------------------------------------------------------------

GET_FILENAME_COMPONENT (SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
GET_FILENAME_COMPONENT(_IMPORT_PREFIX "${SELF_DIR}" PATH)
GET_FILENAME_COMPONENT(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
IF (NOT WIN32)
  GET_FILENAME_COMPONENT(_IMPORT_PREFIX "${_IMPORT_PREFIX}" PATH)
ENDIF (NOT WIN32)

GET_FILENAME_COMPONENT (SZIP_INCLUDE_DIRS "${_IMPORT_PREFIX}/include")

#-----------------------------------------------------------------------------
# Version Strings
#-----------------------------------------------------------------------------
SET (SZIP_VERSION_STRING 2.1)
SET (SZIP_VERSION_MAJOR  2)
SET (SZIP_VERSION_MINOR  1)

#-----------------------------------------------------------------------------
# Don't include targets if this file is being picked up by another
# project which has already build SZIP as a subproject
#-----------------------------------------------------------------------------
IF (NOT TARGET "szip")
  INCLUDE (${SELF_DIR}/szip-targets.cmake)
  SET (SZIP_LIBRARIES "szip")
ENDIF (NOT TARGET "szip")
