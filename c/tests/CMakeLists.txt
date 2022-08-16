set(TEST_SOURCES
    test.c
    int_gnd.c
    util.c
)
add_executable(check_atom check_atom.c ${TEST_SOURCES})
target_link_libraries(check_atom hyperonc CONAN_PKG::libcheck)
add_test(NAME check_atom COMMAND check_atom)

add_executable(check_grounding_space check_grounding_space.c ${TEST_SOURCES})
target_link_libraries(check_grounding_space hyperonc CONAN_PKG::libcheck)
add_test(NAME check_grounding_space COMMAND check_grounding_space)

add_executable(check_sexpr_space check_sexpr_space.c ${TEST_SOURCES})
target_link_libraries(check_sexpr_space hyperonc CONAN_PKG::libcheck)
add_test(NAME check_sexpr_space COMMAND check_sexpr_space)

add_executable(check_types check_types.c ${TEST_SOURCES})
target_link_libraries(check_types hyperonc CONAN_PKG::libcheck)
add_test(NAME check_types COMMAND check_types)
