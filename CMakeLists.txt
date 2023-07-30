# Minimal version supported by hyperonc
cmake_minimum_required(VERSION 3.15)

project(hyperon)

include(ExternalProject)

set(HYPERONC_INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/hyperonc-prefix")
message(STATUS "HYPERONC_INSTALL_PREFIX = ${HYPERONC_INSTALL_PREFIX}")

set(IS_RELEASE_BUILD $<IF:$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>,$<CONFIG:MinSizeRel>>,1,0>)
set(BUILD_CONFIGURATION $<IF:${IS_RELEASE_BUILD},Release,Debug>)

ExternalProject_Add(
    hyperonc
    BUILD_ALWAYS 1
    PREFIX "${HYPERONC_INSTALL_PREFIX}"
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/c"
    CMAKE_ARGS
    "-DCMAKE_INSTALL_PREFIX=${HYPERONC_INSTALL_PREFIX}"
    "-DCMAKE_BUILD_TYPE=${BUILD_CONFIGURATION}"
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
    "-DHYPERONC_INSTALL_PREFIX=${HYPERONC_INSTALL_PREFIX}"
    "-DCMAKE_LIBRARY_OUTPUT_DIRECTORY=${CMAKE_CURRENT_SOURCE_DIR}/python"
    "-DCMAKE_BUILD_TYPE=${BUILD_CONFIGURATION}"
    INSTALL_COMMAND cmake -E echo "Skipping install step."
)

ExternalProject_Get_Property(hyperonpy BINARY_DIR)
set(HYPERONPY_BINARY_DIR "${BINARY_DIR}")
message(STATUS "HYPERONPY_BINARY_DIR = ${HYPERONPY_BINARY_DIR}")

enable_testing()
add_custom_target(check
    COMMAND cd "${HYPERONC_BINARY_DIR}" && ${CMAKE_CTEST_COMMAND} --no-tests=error --output-on-failure
    COMMAND cd "${HYPERONPY_BINARY_DIR}" && ${CMAKE_CTEST_COMMAND} --no-tests=error --output-on-failure)
