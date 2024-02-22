set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/lib)

add_library(opendabridge SHARED
  jni_cta_CtaObsdescr_NativeToJava.c
  jni_cta_utils.cpp
  org_openda_costa_CtaArray.cpp
  org_openda_costa_CtaInitialize.cpp
  org_openda_costa_CtaModelState.cpp
  org_openda_costa_CtaObject.cpp
  org_openda_costa_CtaObservationDescriptions.cpp
  org_openda_costa_CtaOpenDaModel.cpp
  org_openda_costa_CtaParallel.cpp
  org_openda_costa_CtaRelationTable.cpp
  org_openda_costa_CtaStochObserver.cpp
  org_openda_costa_CtaTime.cpp
  org_openda_costa_CtaTreeVector.cpp
  org_openda_costa_CtaUtils.cpp
  org_openda_costa_CtaVector.cpp
  org_openda_resultwriters_NativeResultWriter.cpp
  org_openda_resultwriters_NetcdfResultWriterNative.cpp
)
set_target_properties(opendabridge PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
)

target_include_directories(opendabridge PRIVATE
  ../include
  ${CMAKE_SOURCE_DIR}/cta/include
  ${LIBXML2_INCLUDE_DIR}
  ${JNI_INCLUDE_DIRS}
)

target_link_libraries(opendabridge PRIVATE cta)
