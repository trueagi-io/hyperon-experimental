find_program(NOSETESTS_EXECUTABLE nosetests REQUIRED)

set(TEST_ENVIRONMENT
    "PYTHONPATH=${PYTHONPATH}")

macro(ADD_NOSETESTS)
    set(TEST_NAME "${ARGV0}")
    add_test(NAME ${TEST_NAME} COMMAND ${NOSETESTS_EXECUTABLE}
        ${CMAKE_CURRENT_SOURCE_DIR}/${ARGV0})
    set_property(TEST ${TEST_NAME}
        APPEND PROPERTY ENVIRONMENT ${TEST_ENVIRONMENT})
endmacro()

ADD_NOSETESTS("test_atom.py")
ADD_NOSETESTS("test_grounding_space.py")
ADD_NOSETESTS("test_examples.py")
ADD_NOSETESTS("test_minecraft.py")
ADD_NOSETESTS("test_minelogy.py")
ADD_NOSETESTS("test_unification.py")
ADD_NOSETESTS("test_atom_type.py")
ADD_NOSETESTS("test_grounded_type.py")
