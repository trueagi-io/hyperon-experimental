execute_process(
	COMMAND conan install --build missing ${CMAKE_CURRENT_SOURCE_DIR}
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
include(${CMAKE_CURRENT_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

set(RUST_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/cargo)
execute_process(COMMAND mkdir -p ${RUST_BUILD_DIR}/hyperon)
add_custom_target(build-hyperonc ALL
	COMMAND cargo build
		$<$<CONFIG:Release>:--release>
		--target-dir ${RUST_BUILD_DIR}
	COMMAND cbindgen
		-c "${CMAKE_CURRENT_SOURCE_DIR}/cbindgen.toml"
		-o ${RUST_BUILD_DIR}/hyperon/hyperon.h
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

set(HYPERONC_FILE ${CMAKE_SHARED_LIBRARY_PREFIX}hyperonc${CMAKE_SHARED_LIBRARY_SUFFIX})
add_library(hyperonc SHARED IMPORTED GLOBAL)
set_target_properties(hyperonc PROPERTIES
	IMPORTED_LOCATION_NOCONFIG ${RUST_BUILD_DIR}/debug/${HYPERONC_FILE}
	IMPORTED_LOCATION_DEBUG ${RUST_BUILD_DIR}/debug/${HYPERONC_FILE}
	IMPORTED_LOCATION_RELEASE ${RUST_BUILD_DIR}/release/${HYPERONC_FILE}
	INTERFACE_INCLUDE_DIRECTORIES "${RUST_BUILD_DIR}"
	# required to import hyperonc by name not by relative path
	IMPORTED_NO_SONAME TRUE
	)
add_dependencies(hyperonc build-hyperonc)

add_subdirectory(tests)
