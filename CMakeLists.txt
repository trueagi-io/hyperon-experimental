# Minimal version supported by hyperonc
cmake_minimum_required(VERSION 3.24)

project(hyperon)

include(ExternalProject)

option(GIT "Adds git features to hyperon library; requires OpenSSL and Zlib" ON)

set(HYPERONC_INSTALL_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/hyperonc-install")
message(STATUS "HYPERONC_INSTALL_PREFIX = ${HYPERONC_INSTALL_PREFIX}")

set(IS_RELEASE_BUILD $<IF:$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>,$<CONFIG:MinSizeRel>>,1,0>)
set(BUILD_CONFIGURATION $<IF:${IS_RELEASE_BUILD},Release,Debug>)

ExternalProject_Add(
    hyperonc
    BUILD_ALWAYS 1
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/c"
    CMAKE_ARGS
    -DGIT=${GIT}
    "-DCMAKE_INSTALL_PREFIX=${HYPERONC_INSTALL_PREFIX}"
    "-DCMAKE_BUILD_TYPE=${BUILD_CONFIGURATION}"
    "-DCARGO_ARGS=${CARGO_ARGS}"
    "-DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=${CMAKE_CURRENT_SOURCE_DIR}/conan_provider.cmake"
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
    "-DCMAKE_BUILD_TYPE=${BUILD_CONFIGURATION}"
    "-DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=${CMAKE_CURRENT_SOURCE_DIR}/conan_provider.cmake"
    INSTALL_COMMAND cmake -E echo "Skipping install step."
)

ExternalProject_Get_Property(hyperonpy BINARY_DIR)
set(HYPERONPY_BINARY_DIR "${BINARY_DIR}")
message(STATUS "HYPERONPY_BINARY_DIR = ${HYPERONPY_BINARY_DIR}")

enable_testing()

message(CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR})
message(PYTHONPATH=${PYTHONPATH})

if(WIN32)
    add_custom_target(check
       #COMMAND cd "${HYPERONC_BINARY_DIR}" && ${CMAKE_CTEST_COMMAND} --no-tests=error --output-on-failure -C ${BUILD_CONFIGURATION}
       COMMAND cd "${HYPERONPY_BINARY_DIR}" && ${CMAKE_CTEST_COMMAND} --no-tests=error --output-on-failure -C ${BUILD_CONFIGURATION})
else()
    add_custom_target(check
       COMMAND cd "${HYPERONC_BINARY_DIR}" && ${CMAKE_CTEST_COMMAND} --no-tests=error --output-on-failure
       COMMAND cd "${HYPERONPY_BINARY_DIR}" && ${CMAKE_CTEST_COMMAND} --no-tests=error --output-on-failure)
endif()

