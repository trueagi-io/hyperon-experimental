cmake_minimum_required(VERSION 3.19)
project(hyperonpy)

option(GIT "Adds git features to hyperon library; requires OpenSSL and Zlib" ON)

enable_testing()

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

foreach( OUTPUTCONFIG ${CMAKE_CONFIGURATION_TYPES} )
    string( TOUPPER ${OUTPUTCONFIG} OUTPUTCONFIG )
    set( CMAKE_LIBRARY_OUTPUT_DIRECTORY_${OUTPUTCONFIG} ${CMAKE_CURRENT_SOURCE_DIR} )
endforeach( OUTPUTCONFIG CMAKE_CONFIGURATION_TYPES )

# Fix behavior of CMAKE_CXX_STANDARD when targeting macOS.
if (POLICY CMP0025)
    cmake_policy(SET CMP0025 NEW)
endif ()
if (MSVC)
    # MSVC doesn't support C++11 standard, so lowest possible is C++14
    set(CMAKE_CXX_STANDARD 14)
else ()
    # cibuildwheel images support only C++11 for some of the platforms
    set(CMAKE_CXX_STANDARD 11)
endif ()
message(STATUS "CMAKE_CXX_STANDARD = ${CMAKE_CXX_STANDARD}")

# The default value ("FIRST") prefers the installation with the highest
# version. "ONLY" sticks to a virtualenv even when its version is smaller
# which is usually expected by an user.
if(WIN32)
    if (NOT DEFINED Python_FIND_VIRTUALENV)
        set(Python_FIND_VIRTUALENV "ONLY")
    endif()
    # Development.Embed is not supported by cibuildwheel environment
    find_package(Python 3.7 REQUIRED COMPONENTS Interpreter Development.Module)
    message(STATUS "Python native modules installation path (Python_SITEARCH): ${Python_SITEARCH}")
    message(STATUS "Python modules installation path (Python_SITELIB): ${Python_SITELIB}")
else()
    if (NOT DEFINED Python3_FIND_VIRTUALENV)
        set(Python3_FIND_VIRTUALENV "ONLY")
    endif()
    # Development.Embed is not supported by cibuildwheel environment
    find_package(Python3 3.7 REQUIRED COMPONENTS Interpreter Development.Module)
    message(STATUS "Python native modules installation path (Python3_SITEARCH): ${Python3_SITEARCH}")
    message(STATUS "Python modules installation path (Python3_SITELIB): ${Python3_SITELIB}")
endif()
# Development.Embed is not supported by cibuildwheel environment
find_package(Python3 3.7 REQUIRED COMPONENTS Interpreter Development.Module)
message(STATUS "Python native modules installation path (Python3_SITEARCH): ${Python3_SITEARCH}")
message(STATUS "Python modules installation path (Python3_SITELIB): ${Python3_SITELIB}")

if(GIT)
    find_package(OpenSSL REQUIRED)
    find_package(ZLIB REQUIRED)
endif()

find_package(pybind11 REQUIRED)
find_package(optional-lite REQUIRED)
include_directories(${optional-lite_INCLUDE_DIRS})
message(optional-lite_INCLUDE_DIRS=${optional-lite_INCLUDE_DIRS})
set(HYPERONC_INSTALL_PREFIX "../build/hyperonc-install")
find_package(hyperonc REQUIRED HINTS ${HYPERONC_INSTALL_PREFIX})
include_directories(${hyperonc_INCLUDE_DIRS})

if(APPLE)
    # macOS specific libraries
    set(PLATFORM_LIBRARIES "-framework CoreFoundation" "-framework Security")
endif()

pybind11_add_module(hyperonpy MODULE ./hyperonpy.cpp)
target_link_libraries(hyperonpy PRIVATE "${hyperonc_STATIC_LIBRARY}" nonstd::optional-lite)
if(GIT)
    target_link_libraries(hyperonpy PUBLIC ${PLATFORM_LIBRARIES} OpenSSL::SSL OpenSSL::Crypto ZLIB::ZLIB)
endif()

if(WIN32)
    target_link_libraries(hyperonpy PRIVATE wsock32 ws2_32 userenv ntdll bcrypt)
endif()

get_target_property(PYTHONPATH hyperonpy LIBRARY_OUTPUT_DIRECTORY)
message(PYTHONPATH=${PYTHONPATH})
if(WIN32)
    set(IS_RELEASE_BUILD $<IF:$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>,$<CONFIG:MinSizeRel>>,1,0>)
    set(BUILD_CONFIGURATION $<IF:${IS_RELEASE_BUILD},Release,Debug>)
    set(PYTHONPATH "${PYTHONPATH}/${BUILD_CONFIGURATION}/" "${PYTHONPATH}")
else()
    set(PYTHONPATH "${CMAKE_CURRENT_SOURCE_DIR}")
endif()
add_subdirectory(tests)
