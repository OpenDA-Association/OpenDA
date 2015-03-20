# Cmake Package & Configuration file support.
# Based on code from 'Mastering Cmake'

set (PACKAGE_VERSION "")
if (NOT "${PACKAGE_FIND_VERSION}" VERSION_GREATER "")
	set (PACKAGE_VERSION_COMPATIBLE 1) # Compatible with older.
	if ("${PACKAGE_FIND_VERSION}" VERSION_EQUAL "")
		set(PACKAGE_VERSION_EXACT 1) # exact match for this version.
	endif()
endif()
