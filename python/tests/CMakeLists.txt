set(TEST_ENVIRONMENT
    "PYTHONPATH=${PYTHONPATH}")

macro(ADD_TESTS)
    set(TEST_NAME "${ARGV0}")
    add_test(NAME ${TEST_NAME} COMMAND ${PYTHON_EXECUTABLE} -m unittest ${CMAKE_CURRENT_SOURCE_DIR}/${ARGV0}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    set_property(TEST ${TEST_NAME}
        APPEND PROPERTY ENVIRONMENT ${TEST_ENVIRONMENT})
endmacro()

ADD_TESTS("test_atom.py")
ADD_TESTS("test_grounding_space.py")
ADD_TESTS("test_examples.py")
ADD_TESTS("test_minecraft.py")
ADD_TESTS("test_minelogy.py")
ADD_TESTS("test_unification.py")
ADD_TESTS("test_atom_type.py")
ADD_TESTS("test_grounded_type.py")
ADD_TESTS("test_nondeterm.py")
ADD_TESTS("test_pln_tv.py")
ADD_TESTS("test_run_metta.py")
ADD_TESTS("test_metta.py")
ADD_TESTS("test_list_definition.py")
