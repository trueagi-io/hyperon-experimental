set(TEST_SOURCES
    test.c
    int_gnd.c
    c_space.c
    util.c
)
add_executable(check_atom check_atom.c ${TEST_SOURCES})
target_link_libraries(check_atom hyperonc CONAN_PKG::libcheck)
add_test(NAME check_atom COMMAND check_atom)

add_executable(check_space check_space.c ${TEST_SOURCES})
target_link_libraries(check_space hyperonc CONAN_PKG::libcheck)
add_test(NAME check_space COMMAND check_space)

add_executable(check_sexpr_parser check_sexpr_parser.c ${TEST_SOURCES})
target_link_libraries(check_sexpr_parser hyperonc CONAN_PKG::libcheck)
add_test(NAME check_sexpr_parser COMMAND check_sexpr_parser)

add_executable(check_types check_types.c ${TEST_SOURCES})
target_link_libraries(check_types hyperonc CONAN_PKG::libcheck)
add_test(NAME check_types COMMAND check_types)
