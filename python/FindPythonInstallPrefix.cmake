IF (NOT DEFINED PYTHON_INSTALL_PREFIX)
	FILE(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/scripts)
	# Unpack Python install destination detection script into project
	# binary dir.
	#
	# This is a hack due to the distutils in debian/ubuntu's python3 being misconfigured
	# see discussion https://github.com/opencog/atomspace/issues/1782
	#
	# If the bug is fixed, most of this script could be replaced by:
	#
	# from distutils.sysconfig import get_python_lib; print(get_python_lib(plat_specific=True, prefix=prefix))
	#
	# However, using this would not respect a python virtual environments, so in a way this is better!
	FILE(WRITE ${PROJECT_BINARY_DIR}/scripts/get_python_lib.py
		"import sys\n"
		"import sysconfig\n"
		"import site\n"
		"\n"
		"if __name__ == '__main__':\n"
		"    prefix = sys.argv[1]\n"
		"\n"
		"    # use sites if the prefix is recognized and the sites module is available\n"
		"    # (virtualenv is missing getsitepackages())\n"
		"    if hasattr(site, 'getsitepackages'):\n"
		"        paths = [p for p in site.getsitepackages() if p.startswith(prefix)]\n"
		"        if len(paths) == 1:\n"
		"            print(paths[0], end='')\n"
		"            exit(0)\n"
		"    \n"
		"    # use sysconfig platlib as the fall back\n"
		"    print(sysconfig.get_paths()['platlib'], end='')\n"
		)

	# Find python destination dir for python bindings
	# because it may differ on each operating system.
	EXECUTE_PROCESS(
		COMMAND ${PYTHON_EXECUTABLE} "${PROJECT_BINARY_DIR}/scripts/get_python_lib.py" "${CMAKE_INSTALL_PREFIX}"
		OUTPUT_VARIABLE PYTHON_INSTALL_PREFIX)

	IF ("${PYTHON_INSTALL_PREFIX}" STREQUAL "")
		MESSAGE(FATAL_ERROR "Python install prefix not found")
	ELSE()
		MESSAGE(STATUS "Python install prefix found: ${PYTHON_INSTALL_PREFIX}" )
	ENDIF()
ENDIF()

MESSAGE(STATUS "Python install prefix dir: ${PYTHON_INSTALL_PREFIX}" )

