add_library(hyperoncpp SHARED Atom.cpp)
target_link_libraries(hyperoncpp PRIVATE hyperonc)
target_include_directories(hyperoncpp INTERFACE
	$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/c/cargo>
	#$<INSTALL_INTERFACE:include/mylib>  # <prefix>/include/mylib
)
