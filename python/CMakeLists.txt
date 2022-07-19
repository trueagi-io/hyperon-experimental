execute_process(
	COMMAND conan install --build missing ${CMAKE_CURRENT_SOURCE_DIR}
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

set(PYTHONPATH "${CMAKE_CURRENT_BINARY_DIR}:${CMAKE_CURRENT_SOURCE_DIR}")
add_subdirectory(tests)

INSTALL(TARGETS hyperonpy DESTINATION "${PYTHON_INSTALL_PREFIX}")
INSTALL(DIRECTORY hyperon DESTINATION "${PYTHON_INSTALL_PREFIX}")
