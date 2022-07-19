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

set(HYPERONC_LIB ${CMAKE_SHARED_LIBRARY_PREFIX}hyperonc${CMAKE_SHARED_LIBRARY_SUFFIX})
set(HYPERONC_INCLUDE_DIR ${RUST_BUILD_DIR}/hyperon)
if($<CONFIG:Release>)
	set(HYPERONC_LIB_FILE ${RUST_BUILD_DIR}/release/${HYPERONC_LIB})
else()
	set(HYPERONC_LIB_FILE ${RUST_BUILD_DIR}/debug/${HYPERONC_LIB})
endif()

add_library(hyperonc SHARED IMPORTED GLOBAL)
set_target_properties(hyperonc PROPERTIES
	IMPORTED_LOCATION ${HYPERONC_LIB_FILE}
	INTERFACE_INCLUDE_DIRECTORIES "${RUST_BUILD_DIR}"
	# required to import hyperonc by name not by relative path
	IMPORTED_NO_SONAME TRUE
	)
add_dependencies(hyperonc build-hyperonc)

add_subdirectory(tests)

install(FILES ${HYPERONC_LIB_FILE} DESTINATION lib)
install(DIRECTORY ${HYPERONC_INCLUDE_DIR} DESTINATION include)
if(UNIX AND NOT APPLE)
	install(CODE
		"message(STATUS \"Executing ldconfig\")
		execute_process(COMMAND ldconfig)
		message(STATUS \"Executing ldconfig ... done\")
	")
endif()
