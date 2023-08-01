# Minimal version which allows building dependencies from sources
cmake_minimum_required(VERSION 3.15)
project(hyperonc)

enable_testing()
option(BUILD_SHARED_LIBS "Build shared library" ON)
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

execute_process(
    # --build is required to build dependencies from source under cibuildwheel
    # environment
    COMMAND cmake -E env CC= CXX= conan install --build -- ${CMAKE_CURRENT_SOURCE_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
include(${CMAKE_CURRENT_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

# HYPERONC_TARGET_DIR is used as a target dir for cargo. Cargo automatically adds
# `debug/release` suffix to the value of --target-dir argument so we need to
# calculate actual path to compiled library using $<CONFIG:...> cmake generator
# expression. Resulting shared library is moved under HYPERONC_TARGET_DIR after
# compilation because generator expressions cannot be used in install() and
# set_target_properties() functions.
set(HYPERONC_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/cargo)

# Need to list release configurations explicitly here because $<CONFIG:Debug>
# is FALSE by default (when CMAKE_BUILD_TYPE is unspecified).
set(IS_RELEASE_BUILD $<IF:$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>,$<CONFIG:MinSizeRel>>,1,0>)
set(RUST_BUILD_SUFFIX $<IF:${IS_RELEASE_BUILD},release,debug>)
add_custom_target(show-build-type-vars COMMAND ${CMAKE_COMMAND} -E echo
    "CMAKE_BUILD_TYPE:${BUILD_TYPE} IS_RELEASE_BUILD=${IS_RELEASE_BUILD} RUST_BUILD_SUFFIX=${RUST_BUILD_SUFFIX}")

set(HYPERONC_STATIC_LIB_FILE ${CMAKE_STATIC_LIBRARY_PREFIX}hyperonc${CMAKE_STATIC_LIBRARY_SUFFIX})
set(HYPERONC_STATIC_LIB_PATH ${HYPERONC_TARGET_DIR}/${HYPERONC_STATIC_LIB_FILE})
set(HYPERONC_INCLUDE_DIR ${HYPERONC_TARGET_DIR}/hyperon)

add_custom_target(build-hyperonc ALL
    COMMAND cargo build
        $<${IS_RELEASE_BUILD}:--release>
        --target-dir ${HYPERONC_TARGET_DIR}

    COMMAND ${CMAKE_COMMAND} -E copy
    ${HYPERONC_TARGET_DIR}/${RUST_BUILD_SUFFIX}/${HYPERONC_STATIC_LIB_FILE}
        ${HYPERONC_STATIC_LIB_PATH}

    COMMAND ${CMAKE_COMMAND} -E make_directory ${HYPERONC_INCLUDE_DIR}
    COMMAND cbindgen
        -c "${CMAKE_CURRENT_SOURCE_DIR}/cbindgen.toml"
        -o ${HYPERONC_INCLUDE_DIR}/hyperon.h

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

# cmake checks INTERFACE_INCLUDE_DIRECTORIES contains existing path
execute_process(COMMAND mkdir -p ${HYPERONC_TARGET_DIR})

add_library(hyperonc-static STATIC IMPORTED GLOBAL)
set_target_properties(hyperonc-static PROPERTIES
    IMPORTED_LOCATION "${HYPERONC_STATIC_LIB_PATH}"
    INTERFACE_INCLUDE_DIRECTORIES "${HYPERONC_TARGET_DIR}"
    )
add_dependencies(hyperonc-static build-hyperonc)

add_subdirectory(tests)

set(BINARY_INSTALL_PATH "lib/hyperonc")
set(INCLUDE_INSTALL_PATH "include/hyperonc")
set(CONFIG_INSTALL_PATH "lib/cmake/hyperonc")
set(STATIC_LIBRARY_INSTALL_PATH "${BINARY_INSTALL_PATH}/${HYPERONC_STATIC_LIB_FILE}")

include(CMakePackageConfigHelpers)

if(BUILD_SHARED_LIBS)
    set(HYPERONC_SHARED_LIB_FILE ${CMAKE_SHARED_LIBRARY_PREFIX}hyperonc${CMAKE_SHARED_LIBRARY_SUFFIX})
    set(HYPERONC_SHARED_LIB_PATH ${HYPERONC_TARGET_DIR}/${HYPERONC_SHARED_LIB_FILE})
    set(SHARED_LIBRARY_INSTALL_PATH "${BINARY_INSTALL_PATH}/${HYPERONC_SHARED_LIB_FILE}")

    add_custom_target(copy-hyperonc-shared ALL
        COMMAND ${CMAKE_COMMAND} -E copy
        ${HYPERONC_TARGET_DIR}/${RUST_BUILD_SUFFIX}/${HYPERONC_SHARED_LIB_FILE}
            ${HYPERONC_SHARED_LIB_PATH}

        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    add_dependencies(copy-hyperonc-shared build-hyperonc)

    add_library(hyperonc-shared SHARED IMPORTED GLOBAL)
    set_target_properties(hyperonc-shared PROPERTIES
        IMPORTED_LOCATION "${HYPERONC_SHARED_LIB_PATH}"
        INTERFACE_INCLUDE_DIRECTORIES "${HYPERONC_TARGET_DIR}"
        # required to import hyperonc-shared by name not by relative path
        IMPORTED_NO_SONAME TRUE
        )
    add_dependencies(hyperonc-shared copy-hyperonc-shared build-hyperonc)

    install(FILES "${HYPERONC_SHARED_LIB_PATH}"
        DESTINATION "${BINARY_INSTALL_PATH}")

    configure_package_config_file("hyperonc-config-shared.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/hyperonc-config.cmake"
        INSTALL_DESTINATION "${CONFIG_INSTALL_PATH}"
        PATH_VARS INCLUDE_INSTALL_PATH SHARED_LIBRARY_INSTALL_PATH
        STATIC_LIBRARY_INSTALL_PATH
    )
else(BUILD_SHARED_LIBS)
    configure_package_config_file("hyperonc-config.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/hyperonc-config.cmake"
        INSTALL_DESTINATION "${CONFIG_INSTALL_PATH}"
        PATH_VARS INCLUDE_INSTALL_PATH STATIC_LIBRARY_INSTALL_PATH
    )
endif(BUILD_SHARED_LIBS)

install(FILES
    "${HYPERONC_STATIC_LIB_PATH}"
    DESTINATION "${BINARY_INSTALL_PATH}")
install(DIRECTORY "${HYPERONC_INCLUDE_DIR}"
    DESTINATION "${INCLUDE_INSTALL_PATH}")
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/hyperonc-config.cmake"
    DESTINATION "${CONFIG_INSTALL_PATH}")

add_test(NAME rust_c_api COMMAND cargo test --target-dir ${HYPERONC_TARGET_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
