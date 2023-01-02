cmake_minimum_required(VERSION 3.9.1)

project(openda)
#add_subdirectory(libxml)
add_subdirectory(external/blas)
add_subdirectory(external/lapack)

add_subdirectory(cta/src)
add_subdirectory(cta_f90/generated)
add_subdirectory(openda/bridge/src)

