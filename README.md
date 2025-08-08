![CI](https://github.com/trueagi-io/hyperon-experimental/actions/workflows/ci-auto.yml/badge.svg)

# Overview

OpenCog Hyperon is a substantially revised, novel version of OpenCog - which is currently at an active
pre-alpha stage of development and experimentation. One of the focuses in the Hyperon design is a successor
to the OpenCog Classic Atomese language with clear semantics supporting meta-language features,
different types of inference, etc. What we have landed on is an "Atomese 2" language called MeTTa (Meta Type Talk).

In order to get familiar with MeTTa one can visit [MeTTa website](https://metta-lang.dev)
and watch video with different [MeTTa examples explained](https://singularitynet.zoom.us/rec/share/VqHmU37XtbS7VnKY474tkTvvTglsgOIfsI-21MXWxVm_in7U3tGPcfjjiE0P_15R.yUwPdCzEONSUx1EL?startTime=1650636238000).
The examples of MeTTa programs can be found in [./python/tests/scripts](./python/tests/scripts) directory.
Please look at the [Python unit tests](./python/tests) to understand how one can use MeTTa from Python.
More complex usage scenarios are located at [MeTTa examples repo](https://github.com/trueagi-io/metta-examples).
A lot of different materials can be found on [OpenCog wiki server](https://wiki.opencog.org/w/Hyperon).
Also see [MeTTa specification](https://wiki.opencog.org/w/File:MeTTa_Specification.pdf).

If you want to contribute the project please see the [contributing guide](./docs/CONTRIBUTING.md) first.
If you find troubles with the installation, see the [Troubleshooting](#troubleshooting) section below.
For development related instructions see the [development guide](./docs/DEVELOPMENT.md).

# Using the latest release version

It is the most simple way of getting MeTTa interpreter especially if you are a Python developer.
The following command installs the latest release version from PyPi package repository:
```
python3 -m pip install hyperon
```
If you encountered externally-managed-environment error then create and activate a Python virtual environment. One can use venv, virtualenv or conda virtual environment (please see Python's [Creating Virtual Environments](https://packaging.python.org/en/latest/tutorials/installing-packages/#creating-virtual-environments) or Anaconda's [Creating an environment from an environment.yml file](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#creating-an-environment-from-an-environment-yml-file) instructions, [environment.yaml](https://github.com/trueagi-io/hyperon-experimental/blob/main/environment.yaml) for conda). One more option is using --break-system-packages flag with Pip: python3 -m pip install hyperon --break-system-packages.

Another way is using released Docker image:
```
docker run -ti trueagi/hyperon:latest
```

After installing package or starting Docker container run MeTTa Python based
interpreter:
```
metta-py
```

Using Docker you can also run Rust REPL:
```
metta-repl
``` 

# Using latest development version

## Docker

A docker image can be used as a ready to run stable and predictable development
environment. Docker 26.0.0 or greater version is required to build image
manually.

Build Docker image from a local copy of the repo running:
```
docker build -t trueagi/hyperon .
```

Or build it without local copy of the repo running:
```
docker build \
    --build-arg BUILDKIT_CONTEXT_KEEP_GIT_DIR=1 \
    -t trueagi/hyperon \
    http://github.com/trueagi-io/hyperon-experimental.git#main
```

Use `--target build` option to create an image which keeps the full build
environment and can be used for developing interpreter:
```
docker build --target build -t trueagi/hyperon .
```

If the docker image doesn't work, please raise an
[issue](https://github.com/trueagi-io/hyperon-experimental/issues).

## Manual installation

### Prerequisites

* Install the latest stable Rust, see [Rust installation
page](https://www.rust-lang.org/tools/install). Make sure your
`PATH` variable includes `$HOME/.cargo/bin` directory after installing
Rust (see the Notes at the installation page).

  Requirements for building C and Python API
  * Python3 and Python3-dev (3.8 or later)
  * Pip (23.1.2 or later)
  * GCC (7.5 or later)
  * CMake (3.24 or later)

  To support Git based modules (enabled by default):
  * OpenSSL library
  * Zlib library

* Install cbindgen:
```
cargo install --force cbindgen
```

* Install Conan and make default Conan profile:
```
python3 -m pip install conan==2.16.1
conan profile detect --force
```

* Upgrade Pip to the required version:
```
python3 -m pip install pip==23.1.2
```

# Build and run

## Rust library and REPL

Build and test the Rust binaries:
```
cargo test
```

The experimental features can be enabled by editing
[Cargo.toml](./lib/Cargo.toml) file before compilation or by using `--features`
[command line option](https://doc.rust-lang.org/cargo/reference/features.html#command-line-feature-options).
See comments in the `[features]` section of the file for the features
descriptions.

Run examples:
```
cargo run --example sorted_list
```

Run Rust REPL:
```
cargo run --bin metta-repl
```
You can also find executable at `./target/debug/metta-repl`.

To enable logging during running tests or examples export `RUST_LOG`
environment variable:
```
RUST_LOG=hyperon=debug cargo test
```

Running benchmarks requires nightly toolchain so they can be run using:
```
cargo +nightly bench --features benchmark
```

Generate docs:
```
cargo doc --no-deps
```
Docs can be found at `./target/doc/hyperon/index.html`.

## C and Python API

Create build directory and initialize CMake project:
```
mkdir -p build
cd build
cmake ..
```

Commands for building and testing project depend on a type of a configuration
generator which is used by CMake. `cmake --help` command lists supported
generators at the end of its output. Default generator is marked by `*`. One
can select the configuration generator by specifying `-G <generator>` CMake
option.

### Single configuration generators like "Unix Makefiles"

For this kind of generators the build type is selected at the CMake project
initialization stage. To use release build type add
`-DCMAKE_BUILD_TYPE=Release` CMake flag to the command above. To change the
build type the whole project configuration should be regenerated.

Build and run tests commands:
```
cmake --build .
cmake --build . --target check
```

### Multiple configuration generator like "Visual Studio ..."

This kind of generators allows generating configuration once and select it each
time when making build or running tests. One can add `--config` parameter to
select configuration. By default `Debug` configuration is built.

Keep in mind that one cannot select the library used by `metta` command even in
case of multiple configuration generator. `metta` command uses the last
configuration built.

Build and run tests commands:
```
cmake --build . [--config <Release|Debug>]
cmake --build . --target check [--config <Release|Debug>]
```

## Running Python and MeTTa examples

In order to run examples you need to install the Python module. Please ensure
you built [C and Python API](#c-and-python-api) first. Then execute the
following command in the top directory of repository:
```
python3 -m pip install -e ./python[dev]
```

After this one can run unit tests within `python` directory using `pytest`:
```
pytest ./tests
```

One can run MeTTa script from command line:
```
metta-py ./tests/scripts/<name>.metta
```

Run REPL:
```
cargo run --features python --bin metta-repl
```
You can also find executable at `./target/debug/metta-repl`.

Running the REPL with Python support in a Python virtual environment like PyEnv or Conda requires additional configuration.  See [troubleshooting](#rust-repl-cannot-load-python-library)

### Logger

You can enable logging by prefixing the MeTTa command line by

```
RUST_LOG=hyperon[::COMPONENT]*=LEVEL
```

where
- `[::COMPONENT]*` is a, possibly empty, sequence of modules and
  submodules of hyperon, such as `::metta`, `::metta::runner`,
  `::common`, `::common::multitrie`, etc.
- `LEVEL` is the log level.  Possible log levels are: `error`, `warn`,
  `info`, `debug` and `trace`.

For example, to log all hyperon messages at the `debug` level and
below, while running `script.metta`, you may type:

```
RUST_LOG=hyperon=debug metta-py script.metta
```

Or, to log all hyperon messages at the `trace` level and below,
restricted to module `metta` and submodule `types`, you may type:

```
RUST_LOG=hyperon::metta::types=trace metta-py script.metta
```

By default all log messages are directed to stderr.

## Troubleshooting

### Conan unable to find library in remotes

If you see the following error from Conan:
```
ERROR: Package 'libcheck/0.15.2' not resolved: Unable to find 'libcheck/0.15.2' in remotes
```

Make sure your Conan remote repository is updated to the latest URL:
```
conan remote update conancenter --url="https://center2.conan.io"
```

### Conan claims it cannot find out the version of the C compiler

If you see the following `cmake` output:
```
ERROR: Not able to automatically detect '/usr/bin/cc' version
ERROR: Unable to find a working compiler
```
Try to create the default Conan profile manually:
```
conan profile detect --force
```
If it doesn't help, then try to manually add `compiler`, `compiler.version` and
`compiler.libcxx` values in the default Conan profile
(`~/.conan2/profiles/default`).
For example:
```
compiler=gcc
compiler.version=7
compiler.libcxx=libstdc++
```

### Rust compiler shows errors

Please ensure you are using the latest stable version:
```
rustup update stable
```

### Importing hyperon Python module fails

If importing the hyperon module in Python
```python
import hyperon
```

returns the error:
```
ModuleNotFoundError: No module named 'hyperonpy'
```

Please ensure you have installed the Python module, see
[Running Python and MeTTa examples](#running-python-and-metta-examples).

### Rust REPL cannot load Python library

The REPL needs a path to the libpython library in the current environment.  This can be done one of two ways:

#### On Linux

##### Use `patchelf` on resulting REPL binary to link it with `libpython.so`
```
ldd target/debug/metta-repl | grep libpython ; to find <libpython-name>
patchelf --replace-needed <libpython-name> <path-to-libpython-in-virtual-env> target/debug/metta-repl
```
This must be redone each time the repl is rebuilt, e.g. with `cargo build`.

##### Set the `LD_LIBRARY_PATH` environment variable prior to launching `metta-repl`
```
export LD_LIBRARY_PATH=<path-to-libpython-directory-in-virtual-env>
```

#### On Mac OS
##### Use `install_name_tool` to change the REPL binary's link path for `libpython`
```
otool -L target/debug/metta-repl | grep libpython ; to find <libpython-name>
install_name_tool -change <libpython-name> <path-to-libpython-in-virtual-env> target/debug/metta-repl
```
This must be redone each time the repl is rebuilt, e.g. with `cargo build`.

##### Set the `DYLD_FALLBACK_LIBRARY_PATH` environment variable prior to launching `metta-repl`
```
export DYLD_FALLBACK_LIBRARY_PATH=<path-to-libpython-directory-in-virtual-env>
```
This can be done in your `~/.bashrc` file if you don't want to do it each time you launch the REPL.

For more information about linking `libpython`, see [#432](https://github.com/trueagi-io/hyperon-experimental/issues/432).

# Development

## Structure of the codebase

Main library `libhyperon.rlib` is written in Rust language, it contains core
API which can be used from other Rust projects. Source code of the library is
divided on three crates located under [./hyperon-common](./hyperon-common),
[./hyperon-atom](./hyperon-atom) and [./lib](./lib) directories. It is a plain
Rust project which can be built and tested using Cargo tool.

[./hyperon-common](./hyperon-common) crate contains different utility
structures which are not directly related to the MeTTa and Hyperon. For example
different collection and reference implementations.
[./hyperon-atom](./hyperon-atom) crate contains core API which can be imported by
third-party built-in modules providers. [./lib](./lib) crate contains MeTTa
atomspace and interpreter implementations.

In order to provide API for platforms and languages other than Rust there is a
C API export library `libhyperonc`. Source code of the library is located under
[./c](./c) directory. The library contains Rust C API bindings and depends on
`libhyperon.rlib` library. Native library is compiled using Cargo, C headers
are generated using cbindgen tool.

Source code of the Python integration library is located under
[./python](./python) directory. It contains two main parts. First part is a
native Python library `libhyperonpy` which is written using
[pybind11](https://github.com/pybind/pybind11), it converts Python API calls
into C API calls and vice versa. Second part is a Python library `hyperon`
which uses `libhyperonpy` as a proxy for a C API calls.

All components which depend on `libhyperonc` are built using
[CMake](https://cmake.org/) build tool in order to manage dependencies
automatically.

The diagram below demonstrates main components and dependencies between them:
![Diagram of the structure](./docs/assets/structure.svg)
[Source code of the diagram](./docs/assets/structure.plantuml)

## Language support for IDEs

Different IDEs may require different tweaks to support the languages
used in the codebase. The language servers which we use
for development are:
- [rust-analyzer](https://github.com/rust-lang/rust-analyzer);
- [clangd](https://clangd.llvm.org/installation), generate compile
  commands for the `clangd` using `cmake` variable:
  ```
  cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=Y ..
  ```
- [Python LSP server](https://github.com/python-lsp/python-lsp-server#installation).

