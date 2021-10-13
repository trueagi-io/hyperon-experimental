project(hyperon_capi C)

cmake_minimum_required(VERSION 3.0)
set(CMAKE_BUILD_TYPE Debug)

set(CAPI_SOURCE_DIR "${CMAKE_SOURCE_DIR}")
set(CAPI_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/../target/debug")
# TODO: we should get the debug/release switch from outside
set(CAPI_LIBRARY_DIR "${CMAKE_SOURCE_DIR}/../target/debug")

enable_testing()
add_custom_target(test_verbose COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

execute_process(COMMAND cbindgen
	-c "${CAPI_SOURCE_DIR}/cbindgen.toml"
	-o "${CAPI_INCLUDE_DIR}/hyperon.h"
	WORKING_DIRECTORY ${CAPI_SOURCE_DIR})

include_directories(${CAPI_INCLUDE_DIR})
link_directories(${CAPI_LIBRARY_DIR})

add_subdirectory(tests)
