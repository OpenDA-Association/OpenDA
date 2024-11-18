add_library(oscillf90 STATIC
  ode.f90
  oscill.f90
)
target_include_directories(oscillf90 PRIVATE
  "${CMAKE_SOURCE_DIR}/cta_f90/include"
)
set_property(TARGET oscillf90 PROPERTY POSITION_INDEPENDENT_CODE ON)

add_library(oscill SHARED c_oscill.c)
target_link_libraries(oscill PRIVATE cta oscillf90)
target_include_directories(oscill PRIVATE
  "${CMAKE_SOURCE_DIR}/cta/include"
  ${LIBXML2_INCLUDE_DIR}
)
if(WIN32)
  # Add a "lib" prefix, as some of the tests expect the library to be named
  # liboscill.dll, like its Linux equivalent liboscill.so.
  set_target_properties(oscill PROPERTIES PREFIX "lib")
  # Call Fortran subroutines with capitalised names.
  target_compile_definitions(oscill PRIVATE FTN_CAPITAL)
endif()
