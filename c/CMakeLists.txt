project(hyperonc C)
# minimum version required by conan TARGETS feature
cmake_minimum_required(VERSION 3.1.2)

# CC=gcc is required as conan cannot detect version properly
# see https://github.com/conan-io/conan/issues/4322
execute_process(COMMAND env CC=gcc conan install --build missing ${CMAKE_CURRENT_SOURCE_DIR})
include(${CMAKE_CURRENT_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

set(TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/cargo)
execute_process(COMMAND mkdir -p ${TARGET_DIR})
add_custom_target(build-hyperonc ALL
	COMMAND cargo build
		$<$<CONFIG:Release>:--release>
		--target-dir ${TARGET_DIR}
	COMMAND cbindgen
		-c "${CMAKE_CURRENT_SOURCE_DIR}/cbindgen.toml"
		-o ${TARGET_DIR}/hyperon.h
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

set(HYPERONC_FILE ${CMAKE_SHARED_LIBRARY_PREFIX}hyperonc${CMAKE_SHARED_LIBRARY_SUFFIX})
add_library(hyperonc UNKNOWN IMPORTED)
set_target_properties(hyperonc PROPERTIES
	IMPORTED_LOCATION_NOCONFIG ${TARGET_DIR}/debug/${HYPERONC_FILE}
	IMPORTED_LOCATION_DEBUG ${TARGET_DIR}/debug/${HYPERONC_FILE}
	IMPORTED_LOCATION_RELEASE ${TARGET_DIR}/release/${HYPERONC_FILE}
	INTERFACE_INCLUDE_DIRECTORIES "${TARGET_DIR}")
add_dependencies(hyperonc build-hyperonc)

enable_testing()
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

add_subdirectory(tests)
