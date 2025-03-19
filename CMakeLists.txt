# Minimal version supported by hyperonc
cmake_minimum_required(VERSION 3.24)

project(hyperon)

include(ExternalProject)

option(GIT "Adds git features to hyperon library; requires OpenSSL and Zlib" ON)

set(HYPERONC_INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/hyperonc-install")
message(STATUS "HYPERONC_INSTALL_PREFIX = ${HYPERONC_INSTALL_PREFIX}")

get_property(IS_MULTI_CONFIG GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(IS_MULTI_CONFIG)
    message(STATUS "Multi-config generator")
    set(BUILD_CONFIG "--config $<CONFIG>")
else()
    message(STATUS "Single-config generator")
    set(NONEMPTY_BUILD_CONFIG $<IF:$<STREQUAL:"$<CONFIG>","">,Debug,$<CONFIG>>)
    set(BUILD_TYPE "-DCMAKE_BUILD_TYPE=${NONEMPTY_BUILD_CONFIG}")
endif()

add_custom_target(show-build-type-vars COMMAND ${CMAKE_COMMAND} -E echo
    "CONFIG:$<CONFIG> NONEMPTY_BUILD_CONFIG:${NONEMPTY_BUILD_CONFIG} BUILD_CONFIG:${BUILD_CONFIG} BUILD_TYPE:${BUILD_TYPE}")

ExternalProject_Add(
    hyperonc
    BUILD_ALWAYS 1
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/c"
    CMAKE_ARGS
    -DGIT=${GIT}
    "-DCMAKE_INSTALL_PREFIX=${HYPERONC_INSTALL_PREFIX}"
    "-DCARGO_ARGS=${CARGO_ARGS}"
    "-DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=${CMAKE_CURRENT_SOURCE_DIR}/conan_provider.cmake"
    ${BUILD_TYPE}
    BUILD_COMMAND ${CMAKE_COMMAND} --build <BINARY_DIR> ${BUILD_CONFIG}
)

ExternalProject_Get_Property(hyperonc BINARY_DIR)
set(HYPERONC_BINARY_DIR "${BINARY_DIR}")
message(STATUS "HYPERONC_BINARY_DIR = ${HYPERONC_BINARY_DIR}")

ExternalProject_Add(
    hyperonpy
    BUILD_ALWAYS 1
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/python"
    DEPENDS hyperonc
    CMAKE_ARGS
    -DGIT=${GIT}
    "-DHYPERONC_INSTALL_PREFIX=${HYPERONC_INSTALL_PREFIX}"
    "-DCMAKE_LIBRARY_OUTPUT_DIRECTORY=${CMAKE_CURRENT_SOURCE_DIR}/python"
    "-DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=${CMAKE_CURRENT_SOURCE_DIR}/conan_provider.cmake"
    ${BUILD_TYPE}
    BUILD_COMMAND ${CMAKE_COMMAND} --build <BINARY_DIR> ${BUILD_CONFIG}
    INSTALL_COMMAND ${CMAKE_COMMAND} -E echo "Skipping install step."
)

ExternalProject_Get_Property(hyperonpy BINARY_DIR)
set(HYPERONPY_BINARY_DIR "${BINARY_DIR}")
message(STATUS "HYPERONPY_BINARY_DIR = ${HYPERONPY_BINARY_DIR}")

enable_testing()

list(APPEND CTEST_ARGS --no-tests=error --output-on-failure)
if(IS_MULTI_CONFIG)
    list(APPEND CTEST_ARGS -C $<CONFIG>)
endif()

add_custom_target(check
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir "${HYPERONC_BINARY_DIR}" ${CTEST_ARGS}
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir "${HYPERONPY_BINARY_DIR}" ${CTEST_ARGS}
)
