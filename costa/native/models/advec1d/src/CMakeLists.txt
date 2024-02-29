add_library(advec1d SHARED
  advec_model.f
  cta_model.f
)
target_link_libraries(advec1d PRIVATE cta)
target_include_directories(advec1d PRIVATE "${CMAKE_SOURCE_DIR}/cta_f90/include")

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  set_source_files_properties(cta_model.f PROPERTIES
    COMPILE_FLAGS -fallow-argument-mismatch
  )
endif()
