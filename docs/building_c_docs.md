
### Building C Documentation

1. `doxygen` must be installed.  Depending on your platform it may be easiest to use a package manager.
    - using apt: `sudo apt-get install doxygen`
    - using homebrew (Mac): `brew install doxygen`
    - installing from source: [https://www.doxygen.nl/manual/install.html]

2. Build the Hyperon C library, following the instructions here: [https://github.com/trueagi-io/hyperon-experimental?tab=readme-ov-file#c-and-python-api]

3. Set CMake environment variables.  Relative to the `build` directory, run the following:
    - `export CMAKE_CURRENT_SOURCE_DIR=../c/`
    - `export HYPERONC_INCLUDE_DIR=./hyperonc-install/include/hyperonc/hyperon/`

4. Invoke `doxygen` using the following command: `doxygen ../c/hyperonc.doxyfile`

The top page for the rendered HTML results will be written to `./html/index.html`, and latex results will be similarly written to `./latex/index.tex`