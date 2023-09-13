cmake_minimum_required(VERSION 3.9.1)
find_package(LibXml2 REQUIRED)
find_package(JNI REQUIRED)

set(LIBRARY_OUTPUT_PATH  ${CMAKE_BINARY_DIR}/lib)
set(SOURCES
  jni_cta_utils.cpp                                org_openda_costa_CtaRelationTable.cpp
  org_openda_costa_CtaArray.cpp                    org_openda_costa_CtaStochObserver.cpp
  org_openda_costa_CtaInitialize.cpp               org_openda_costa_CtaTime.cpp
  org_openda_costa_CtaModelState.cpp               org_openda_costa_CtaTreeVector.cpp
  org_openda_costa_CtaObject.cpp                   org_openda_costa_CtaUtils.cpp
  org_openda_costa_CtaObservationDescriptions.cpp  org_openda_costa_CtaVector.cpp
  org_openda_costa_CtaOpenDaModel.cpp              org_openda_resultwriters_NativeResultWriter.cpp
  org_openda_costa_CtaParallel.cpp                 org_openda_resultwriters_NetcdfResultWriterNative.cpp
)

add_library(opendabridge SHARED ${SOURCES})


target_include_directories(opendabridge PUBLIC ../include)
target_include_directories(opendabridge PUBLIC ${CMAKE_SOURCE_DIR}/cta/include)
target_include_directories(opendabridge PUBLIC ${CMAKE_SOURCE_DIR}/external)
target_include_directories(opendabridge PUBLIC ${LIBXML2_INCLUDE_DIR}) 
target_include_directories(opendabridge PUBLIC ${JNI_INCLUDE_DIRS})
#target_link_libraries(cta INTERFACE libxml)

