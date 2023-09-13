cmake_minimum_required(VERSION 3.9.1)

project(openda)

add_subdirectory(external/blas)
add_subdirectory(external/lapack)

add_subdirectory(cta/src)
add_subdirectory(cta_f90/generated)
add_subdirectory(bridge/src)

file(GLOB CTA_PUBLIC_HEADERS
    "cta/include/*.h"
)

install(TARGETS cta cta_f90 opendabridge 
    LIBRARY DESTINATION lib
)

install(FILES ${CTA_PUBLIC_HEADERS}
    DESTINATION include
)
