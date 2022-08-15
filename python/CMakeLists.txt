execute_process(
    COMMAND conan install --build missing ${CMAKE_CURRENT_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
# TODO: look into a way to use `cmake` Conan generator
# and somehow import pybind11_add_module of pybind11
include(${CMAKE_CURRENT_BINARY_DIR}/conan_paths.cmake)
find_package(pybind11 REQUIRED)

find_package(PythonInterp 3.6 REQUIRED)
# Python installation path should be modified via Python environment.
# Following way of getting paths seems to be working (for Ubuntu as well)
# TODO: Starting from cmake 3.12 FindPython3 module can be used instead:
# variable naming is compatible.
execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_path('platlib'), end='')" OUTPUT_VARIABLE Python3_SITEARCH)
execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_path('purelib'), end='')" OUTPUT_VARIABLE Python3_SITELIB)
message(STATUS "Python3_SITEARCH: ${Python3_SITEARCH}")
message(STATUS "Python3_SITELIB: ${Python3_SITELIB}")
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR})

pybind11_add_module(hyperonpy hyperonpy.cpp)
target_link_libraries(hyperonpy PRIVATE hyperonc)

set(PYTHONPATH "${CMAKE_CURRENT_BINARY_DIR}:${CMAKE_CURRENT_SOURCE_DIR}")
add_subdirectory(tests)

INSTALL(TARGETS hyperonpy DESTINATION "${Python3_SITEARCH}")
INSTALL(DIRECTORY hyperon DESTINATION "${Python3_SITELIB}")
