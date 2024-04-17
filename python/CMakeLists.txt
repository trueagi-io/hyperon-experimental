# version supported by Ubuntu 18.04
cmake_minimum_required(VERSION 3.10.2)
project(hyperonpy)

enable_testing()
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

# Fix behavior of CMAKE_CXX_STANDARD when targeting macOS.
if (POLICY CMP0025)
    cmake_policy(SET CMP0025 NEW)
endif ()
set(CMAKE_CXX_STANDARD 11)

if (${CMAKE_VERSION} GREATER_EQUAL "3.12")
    # The default value ("FIRST") prefers the installation with the highest
    # version. "ONLY" sticks to a virtualenv even when its version is smaller
    # which is usually expected by an user.
    if (NOT DEFINED Python3_FIND_VIRTUALENV)
        set(Python3_FIND_VIRTUALENV "ONLY")
    endif()
    if (${CMAKE_VERSION} GREATER_EQUAL "3.18")
        # Development.Embed is not supported by cibuildwheel environment
        find_package(Python3 3.7 REQUIRED COMPONENTS Interpreter Development.Module)
    else()
        find_package(Python3 3.7 REQUIRED COMPONENTS Interpreter Development)
    endif()
else() # support Ubuntu 18.04
    # pybind11 config looks for Python path again when deprecated PythonInterp
    # and PythonLibs packages are used. It should find the same version though
    # because PYTHON_EXECUTABLE is already set.
    find_package(PythonInterp 3.7 REQUIRED)
    find_package(PythonLibs 3.7 REQUIRED)
    # Python installation path should be modified via Python environment.
    # Following way of getting paths seems to be working (for Ubuntu as well).
    execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_path('platlib'), end='')" OUTPUT_VARIABLE Python3_SITEARCH)
    execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_path('purelib'), end='')" OUTPUT_VARIABLE Python3_SITELIB)
    set(Python3_EXECUTABLE ${PYTHON_EXECUTABLE})
endif()
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

find_library(GIT2_LIBRARY NAMES git2)
find_library(ICONV_LIBRARY NAMES iconv)
find_library(Z_LIBRARY NAMES z)

pybind11_add_module(hyperonpy MODULE ./hyperonpy.cpp)
target_link_libraries(hyperonpy PRIVATE "${hyperonc_STATIC_LIBRARY}" ${GIT2_LIBRARY} ${ICONV_LIBRARY} ${Z_LIBRARY})

set(PYTHONPATH "${CMAKE_CURRENT_SOURCE_DIR}")
add_subdirectory(tests)
