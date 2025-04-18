add_library(heat_stoch SHARED
  cta_model.f
  interpolate_bound_params.f
  interpolate_noise_params.f
  Linear_FullState_Step.f
  modelstep.f
  usr_obs.f
)
target_link_libraries(heat_stoch PRIVATE blas lapack cta)
target_include_directories(heat_stoch PRIVATE "${CMAKE_SOURCE_DIR}/cta_f90/include")

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  set_source_files_properties(cta_model.f usr_obs.f PROPERTIES
    COMPILE_FLAGS -fallow-argument-mismatch
  )
endif()
