include(FindMPI)

add_library(pollute2d SHARED
  pollute2d.f90
)
target_link_libraries(pollute2d PRIVATE
  cta
  MPI::MPI_Fortran
)
target_include_directories(pollute2d PRIVATE
  "${CMAKE_SOURCE_DIR}/cta_f90/include"
  ${MPI_C_INCLUDE_DIRS}
)

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  set_source_files_properties(pollute2d.f90 PROPERTIES
    COMPILE_FLAGS -fallow-argument-mismatch
  )
endif()
