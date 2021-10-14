set(TEST_SOURCES
	check_capi.c
	int_gnd.c
)
add_executable(check_capi ${TEST_SOURCES})
add_dependencies(check_capi capi_header)
target_link_libraries(check_capi hyperon_capi ${CONAN_LIBS})
add_test(NAME capi COMMAND check_capi)

