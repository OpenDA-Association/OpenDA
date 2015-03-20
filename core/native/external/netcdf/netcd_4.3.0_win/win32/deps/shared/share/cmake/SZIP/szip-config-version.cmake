#-----------------------------------------------------------------------------
# SZIP Version file for install directory
#-----------------------------------------------------------------------------

SET (PACKAGE_VERSION 2.1)

IF ("${PACKAGE_FIND_VERSION_MAJOR}" EQUAL 2)

  # exact match for version 2.1
  IF ("${PACKAGE_FIND_VERSION_MINOR}" EQUAL 1)

    # compatible with any version 2.1.x
    SET (PACKAGE_VERSION_COMPATIBLE 1) 
    
    IF ("${PACKAGE_FIND_VERSION_PATCH}" EQUAL )
      SET (PACKAGE_VERSION_EXACT 1)    

      IF ("${PACKAGE_FIND_VERSION_TWEAK}" EQUAL )
        # not using this yet
      ENDIF ("${PACKAGE_FIND_VERSION_TWEAK}" EQUAL )
      
    ENDIF ("${PACKAGE_FIND_VERSION_PATCH}" EQUAL )
    
  ENDIF ("${PACKAGE_FIND_VERSION_MINOR}" EQUAL 1)
ENDIF ("${PACKAGE_FIND_VERSION_MAJOR}" EQUAL 2)


