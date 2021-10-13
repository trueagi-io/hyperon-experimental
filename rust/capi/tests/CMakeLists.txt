find_package(check REQUIRED)

set(TEST_SOURCES
	check_capi.c
	int_gnd.c
)
add_executable(check_capi ${TEST_SOURCES})
target_link_libraries(check_capi hyperon_capi check)
add_test(NAME capi COMMAND check_capi)

