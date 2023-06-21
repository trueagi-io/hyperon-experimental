if (${CMAKE_VERSION} GREATER_EQUAL "3.12")
    # The default value ("FIRST") prefers the installation with the highest
    # version. "ONLY" sticks to a virtualenv even when its version is smaller
    # which is usually expected by an user.
    if (NOT DEFINED Python3_FIND_VIRTUALENV)
        set(Python3_FIND_VIRTUALENV "ONLY")
    endif()
    find_package(Python3 3.6 REQUIRED COMPONENTS Interpreter Development)
else() # support Ubuntu 18.04
    # pybind11 config looks for Python path again when deprecated PythonInterp
    # and PythonLibs packages are used. It should find the same version though
    # because PYTHON_EXECUTABLE is already set.
    find_package(PythonInterp 3.6 REQUIRED)
    find_package(PythonLibs 3.6 REQUIRED)
    # Python installation path should be modified via Python environment.
    # Following way of getting paths seems to be working (for Ubuntu as well).
    execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_path('platlib'), end='')" OUTPUT_VARIABLE Python3_SITEARCH)
    execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_path('purelib'), end='')" OUTPUT_VARIABLE Python3_SITELIB)
    set(Python3_EXECUTABLE ${PYTHON_EXECUTABLE})
endif()
message(STATUS "Python native modules installation path (Python3_SITEARCH): ${Python3_SITEARCH}")
message(STATUS "Python modules installation path (Python3_SITELIB): ${Python3_SITELIB}")

set(HYPERONPY_DIR ${CMAKE_CURRENT_BINARY_DIR})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in ${CMAKE_CURRENT_SOURCE_DIR}/setup.py)

execute_process(
    COMMAND conan install --build missing ${CMAKE_CURRENT_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
include(${CMAKE_CURRENT_BINARY_DIR}/conan_paths.cmake)
find_package(pybind11 REQUIRED)

pybind11_add_module(hyperonpy SHARED hyperonpy.cpp)
target_link_libraries(hyperonpy PRIVATE hyperonc-static)

set(PYTHONPATH "${CMAKE_CURRENT_BINARY_DIR}:${CMAKE_CURRENT_SOURCE_DIR}")
add_subdirectory(tests)

INSTALL(TARGETS hyperonpy DESTINATION "${Python3_SITEARCH}")
INSTALL(DIRECTORY hyperon DESTINATION "${Python3_SITELIB}")
