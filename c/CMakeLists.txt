execute_process(
    COMMAND conan install --build missing ${CMAKE_CURRENT_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
include(${CMAKE_CURRENT_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

# RUST_TARGET_DIR is used as a target dir for carg. Cargo automatically adds
# `debug/release` suffix to the value of --target-dir argument so we need to
# calculate actual path to compiled library using $<CONFIG:...> cmake generator
# expression. Resulting shared library is moved under RUST_TARGET_DIR after
# compilation because generator expressions cannot be used in install() and
# set_target_properties() functions.
set(RUST_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/cargo)
# Need to list release configurations explicitly here because $<CONFIG:Debug>
# is FALSE by default (when CMAKE_BUILD_TYPE is unspecified).
set(IS_RELEASE_BUILD $<IF:$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>,$<CONFIG:MinSizeRel>>,1,0>)
set(RUST_BUILD_SUFFIX $<IF:${IS_RELEASE_BUILD},release,debug>)
add_custom_target(show-build-type-vars COMMAND ${CMAKE_COMMAND} -E echo
    "CMAKE_BUILD_TYPE:${BUILD_TYPE} IS_RELEASE_BUILD=${IS_RELEASE_BUILD} RUST_BUILD_SUFFIX=${RUST_BUILD_SUFFIX}")

set(HYPERONC_LIB_FILE ${CMAKE_SHARED_LIBRARY_PREFIX}hyperonc${CMAKE_SHARED_LIBRARY_SUFFIX})
set(HYPERONC_LIB_PATH ${RUST_TARGET_DIR}/${HYPERONC_LIB_FILE})
set(HYPERONC_INCLUDE_DIR ${RUST_TARGET_DIR}/hyperon)

add_custom_target(build-hyperonc ALL
    COMMAND cargo build
        $<${IS_RELEASE_BUILD}:--release>
        --target-dir ${RUST_TARGET_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy
        ${RUST_TARGET_DIR}/${RUST_BUILD_SUFFIX}/${HYPERONC_LIB_FILE}
        ${HYPERONC_LIB_PATH}

    COMMAND ${CMAKE_COMMAND} -E make_directory ${HYPERONC_INCLUDE_DIR}
    COMMAND cbindgen
        -c "${CMAKE_CURRENT_SOURCE_DIR}/cbindgen.toml"
        -o ${HYPERONC_INCLUDE_DIR}/hyperon.h

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_library(hyperonc SHARED IMPORTED GLOBAL)
# cmake checks INTERFACE_INCLUDE_DIRECTORIES contains existing path
execute_process(COMMAND mkdir -p ${RUST_TARGET_DIR})
set_target_properties(hyperonc PROPERTIES
    IMPORTED_LOCATION "${HYPERONC_LIB_PATH}"
    INTERFACE_INCLUDE_DIRECTORIES "${RUST_TARGET_DIR}"
    # required to import hyperonc by name not by relative path
    IMPORTED_NO_SONAME TRUE
    )
add_dependencies(hyperonc build-hyperonc)

add_subdirectory(tests)

install(FILES ${HYPERONC_LIB_PATH} DESTINATION lib)
install(DIRECTORY ${HYPERONC_INCLUDE_DIR} DESTINATION include)
