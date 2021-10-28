set(TEST_SOURCES
	int_gnd.c
	util.c
)
add_executable(check_atom check_atom.c ${TEST_SOURCES})
add_dependencies(check_atom capi_header)
target_link_libraries(check_atom hyperon_capi ${CONAN_LIBS})
add_test(NAME check_atom COMMAND check_atom)

add_executable(check_grounding_space check_grounding_space.c ${TEST_SOURCES})
add_dependencies(check_grounding_space capi_header)
target_link_libraries(check_grounding_space hyperon_capi ${CONAN_LIBS})
add_test(NAME check_grounding_space COMMAND check_grounding_space)

add_executable(check_sexpr_space check_sexpr_space.c ${TEST_SOURCES})
add_dependencies(check_sexpr_space capi_header)
target_link_libraries(check_sexpr_space hyperon_capi ${CONAN_LIBS})
add_test(NAME check_sexpr_space COMMAND check_sexpr_space)
