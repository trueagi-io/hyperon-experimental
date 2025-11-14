# Minimal version which allows building dependencies from sources
cmake_minimum_required(VERSION 3.19)
project(hyperonc)

option(GIT "Adds git features to hyperon library; requires OpenSSL and Zlib" ON)

enable_testing()
option(BUILD_SHARED_LIBS "Build shared library" ON)
get_property(IS_MULTI_CONFIG GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
list(APPEND CTEST_ARGS --output-on-failure)
if(IS_MULTI_CONFIG)
    list(APPEND CTEST_ARGS -C $<CONFIG>)
endif()
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} ${CTEST_ARGS})

# HYPERONC_TARGET_DIR is used as a target dir for cargo. Cargo automatically adds
# `debug/release` suffix to the value of --target-dir argument so we need to
# calculate actual path to compiled library using $<CONFIG:...> cmake generator
# expression. Resulting shared library is moved under HYPERONC_TARGET_DIR after
# compilation because generator expressions cannot be used in install() and
# set_target_properties() functions.
set(HYPERONC_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/cargo)
# cmake checks INTERFACE_INCLUDE_DIRECTORIES contains existing path
file(MAKE_DIRECTORY "${HYPERONC_TARGET_DIR}")

# Need to list release configurations explicitly here because $<CONFIG:Debug>
# is FALSE by default (when config is unspecified).
set(IS_RELEASE_BUILD $<IF:$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>,$<CONFIG:MinSizeRel>>,1,0>)
set(RUST_BUILD_SUFFIX $<IF:${IS_RELEASE_BUILD},release,debug>)

set(HYPERONC_STATIC_LIB_FILE ${CMAKE_STATIC_LIBRARY_PREFIX}hyperonc${CMAKE_STATIC_LIBRARY_SUFFIX})
set(HYPERONC_STATIC_LIB_PATH ${HYPERONC_TARGET_DIR}/${HYPERONC_STATIC_LIB_FILE})
set(HYPERONC_INCLUDE_DIR ${HYPERONC_TARGET_DIR}/hyperon)
separate_arguments(CARGO_ARGS_LIST NATIVE_COMMAND ${CARGO_ARGS})

if(GIT)
    set(GIT_FEATURE --features hyperon/git)
endif()

add_custom_target(build-hyperonc ALL
    COMMAND cargo build
        ${CARGO_ARGS_LIST}
        $<${IS_RELEASE_BUILD}:--release>
        ${GIT_FEATURE}
        --target-dir ${HYPERONC_TARGET_DIR}

    COMMAND ${CMAKE_COMMAND} -E copy
    ${HYPERONC_TARGET_DIR}/${RUST_BUILD_SUFFIX}/${HYPERONC_STATIC_LIB_FILE}
        ${HYPERONC_STATIC_LIB_PATH}

    COMMAND ${CMAKE_COMMAND} -E make_directory ${HYPERONC_INCLUDE_DIR}
    COMMAND cbindgen
        -c "${CMAKE_CURRENT_SOURCE_DIR}/cbindgen.toml"
        -o ${HYPERONC_INCLUDE_DIR}/hyperon.h

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

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
    if(GIT)
        find_package(OpenSSL REQUIRED)
        find_package(ZLIB REQUIRED)
    endif(GIT)

    set(HYPERONC_SHARED_LIB_FILE ${CMAKE_SHARED_LIBRARY_PREFIX}hyperonc${CMAKE_SHARED_LIBRARY_SUFFIX})
    set(HYPERONC_SHARED_LIB_PATH ${HYPERONC_TARGET_DIR}/${HYPERONC_SHARED_LIB_FILE})
    set(SHARED_LIBRARY_INSTALL_PATH "${BINARY_INSTALL_PATH}/${HYPERONC_SHARED_LIB_FILE}")

    add_custom_target(copy-hyperonc-shared ALL
        COMMAND ${CMAKE_COMMAND} -E copy
        ${HYPERONC_TARGET_DIR}/${RUST_BUILD_SUFFIX}/${HYPERONC_SHARED_LIB_FILE}
            ${HYPERONC_SHARED_LIB_PATH}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        BYPRODUCTS ${HYPERONC_SHARED_LIB_PATH})

    add_dependencies(copy-hyperonc-shared build-hyperonc)

    if (WIN32)
        set(HYPERONC_IMPLIB_FILE ${HYPERONC_SHARED_LIB_FILE}${CMAKE_LINK_LIBRARY_SUFFIX})
        set(HYPERONC_IMPLIB_PATH ${HYPERONC_TARGET_DIR}/${HYPERONC_IMPLIB_FILE})
        set(IMPLIB_INSTALL_PATH "${BINARY_INSTALL_PATH}/${HYPERONC_IMPLIB_FILE}")

        add_custom_target(copy-hyperonc-implib ALL
            COMMAND ${CMAKE_COMMAND} -E copy
            ${HYPERONC_TARGET_DIR}/${RUST_BUILD_SUFFIX}/${HYPERONC_IMPLIB_FILE}
                ${HYPERONC_IMPLIB_PATH}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

        add_dependencies(copy-hyperonc-implib build-hyperonc)
    endif(WIN32)

    add_library(hyperonc-shared SHARED IMPORTED GLOBAL)
    set_target_properties(hyperonc-shared PROPERTIES
        IMPORTED_LOCATION "${HYPERONC_SHARED_LIB_PATH}"
        # required by Windows
        IMPORTED_IMPLIB "${HYPERONC_IMPLIB_PATH}"
        INTERFACE_INCLUDE_DIRECTORIES "${HYPERONC_TARGET_DIR}"
        # required to import hyperonc-shared by name not by relative path
        IMPORTED_NO_SONAME TRUE
        )
    if (WIN32)
        # These libraries are required to be linked under Windows platform as
        # they are dependencies of the Rust standard library.
        # See https://doc.rust-lang.org/reference/linkage.html#r-link.staticlib
        target_link_libraries(hyperonc-shared INTERFACE wsock32 ws2_32 userenv ntdll bcrypt winhttp Rpcrt4)
    endif(WIN32)
    if(GIT)
        target_link_libraries(hyperonc-shared INTERFACE OpenSSL::SSL OpenSSL::Crypto ZLIB::ZLIB)
    endif(GIT)

    add_dependencies(hyperonc-shared copy-hyperonc-shared)
    if (WIN32)
        add_dependencies(hyperonc-shared copy-hyperonc-implib)
    endif(WIN32)

    install(FILES "${HYPERONC_SHARED_LIB_PATH}"
        DESTINATION "${BINARY_INSTALL_PATH}")
    if (WIN32)
        install(FILES "${HYPERONC_IMPLIB_PATH}"
            DESTINATION "${BINARY_INSTALL_PATH}")
    endif(WIN32)

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

add_test(NAME rust_c_api
    COMMAND cargo test ${CARGO_ARGS_LIST} --target-dir ${HYPERONC_TARGET_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})