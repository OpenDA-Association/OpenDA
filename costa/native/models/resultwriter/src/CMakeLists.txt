add_library(resultwriter SHARED
  resultwriter.f90
)
target_link_libraries(resultwriter PRIVATE cta)

# Add modules from cta_f90 to the include path.
add_dependencies(resultwriter cta_f90)
target_include_directories(resultwriter PRIVATE
  "${CMAKE_BINARY_DIR}/cta_f90/generated"
)
