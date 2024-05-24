cmake_minimum_required(VERSION 3.19)
project(hyperonpy)

enable_testing()
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

# Fix behavior of CMAKE_CXX_STANDARD when targeting macOS.
if (POLICY CMP0025)
    cmake_policy(SET CMP0025 NEW)
endif ()
set(CMAKE_CXX_STANDARD 11)

# The default value ("FIRST") prefers the installation with the highest
# version. "ONLY" sticks to a virtualenv even when its version is smaller
# which is usually expected by an user.
if (NOT DEFINED Python3_FIND_VIRTUALENV)
    set(Python3_FIND_VIRTUALENV "ONLY")
endif()
# Development.Embed is not supported by cibuildwheel environment
find_package(Python3 3.7 REQUIRED COMPONENTS Interpreter Development.Module)
message(STATUS "Python native modules installation path (Python3_SITEARCH): ${Python3_SITEARCH}")
message(STATUS "Python modules installation path (Python3_SITELIB): ${Python3_SITELIB}")

execute_process(
    # --build is required to build dependencies from source under cibuildwheel
    # environment
    COMMAND conan install --build -- ${CMAKE_CURRENT_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
include(${CMAKE_CURRENT_BINARY_DIR}/conan_paths.cmake)

find_package(pybind11 REQUIRED)
find_package(optional-lite REQUIRED)
include_directories(${nonstd_INCLUDE_DIRS})
find_package(hyperonc REQUIRED HINTS ${HYPERONC_INSTALL_PREFIX})
include_directories(${hyperonc_INCLUDE_DIRS})

pybind11_add_module(hyperonpy MODULE ./hyperonpy.cpp)
target_link_libraries(hyperonpy PRIVATE "${hyperonc_STATIC_LIBRARY}")

set(PYTHONPATH "${CMAKE_CURRENT_SOURCE_DIR}")
add_subdirectory(tests)
