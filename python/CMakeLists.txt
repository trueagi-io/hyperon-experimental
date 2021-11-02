# CC=gcc is required as conan cannot detect version properly
# see https://github.com/conan-io/conan/issues/4322
execute_process(
	COMMAND env CC=gcc conan install --build missing ${CMAKE_CURRENT_SOURCE_DIR}
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
# TODO: look into a way to use `cmake` Conan generator
# and somehow import pybind11_add_module of pybind11
include(${CMAKE_CURRENT_BINARY_DIR}/conan_paths.cmake)
find_package(pybind11 REQUIRED)

find_package(PythonInterp REQUIRED)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR})
include(FindPythonInstallPrefix)

pybind11_add_module(hyperonpy hyperonpy.cpp)
target_link_libraries(hyperonpy PRIVATE hyperonc)

#add_subdirectory(tests)
