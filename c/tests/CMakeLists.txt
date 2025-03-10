find_package(Check REQUIRED)

set(TEST_SOURCES
    test.c
    int_gnd.c
    c_space.c
    util.c
)

macro(ADD_TESTS)
    set(TEST_NAME "${ARGV0}")
    add_executable(${TEST_NAME} ${TEST_NAME}.c ${TEST_SOURCES})
    target_link_libraries(${TEST_NAME} hyperonc-shared Check::check)
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
    set_property(TEST ${TEST_NAME}
        APPEND PROPERTY ENVIRONMENT PATH=$<SHELL_PATH:${HYPERONC_TARGET_DIR}>)
endmacro()

ADD_TESTS("check_atom")
ADD_TESTS("check_space")
ADD_TESTS("check_sexpr_parser")
ADD_TESTS("check_types")
ADD_TESTS("check_runner")
