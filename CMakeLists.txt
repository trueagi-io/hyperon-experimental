project(hyperon)
# minimum version required by conan TARGETS feature
cmake_minimum_required(VERSION 3.1.2)

enable_testing()
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure)

add_subdirectory(c)
add_subdirectory(python)

