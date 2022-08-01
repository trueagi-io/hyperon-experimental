![CI](https://github.com/trueagi-io/hyperon-experimental/actions/workflows/ci.yml/badge.svg)

# Overview

This is a reimplementation from scratch of the C++ Hyperon prototype in the Rust
programming language. This project replaces the [previous
prototype](https://github.com/trueagi-io/hyperon/tree/master).
See [MeTTa scripts](./python/tests/scripts) and [Python examples](./python/tests) to become familiar with Hyperon features.

If you find troubles with the installation, see the [Troubleshooting](#troubleshooting) section below.

## Docker

A docker image can be used to run a reproducible environment. See instructions
inside the [Dockerfile](./.github/Dockerfile).
If the docker image doesn't work, please
raise an
[issue](https://github.com/trueagi-io/hyperon-experimental/issues).


# Prerequisites

Install latest stable Rust, see [Rust installation
page](https://www.rust-lang.org/tools/install).

# Hyperon library

Build and test the library:
```
cd ./lib
cargo build
cargo test
```

To enable logging during tests execute:
```
RUST_LOG=hyperon=debug cargo test
```

Generate docs:
```
cd ./lib
cargo doc --no-deps
```
Docs can be found at `./lib/target/doc/hyperon/index.html`.

# C and Python API

Prerequisites (must be executed in the top directory of the repository):
```
cargo install --force cbindgen
python -m pip install conan==1.47
python -m pip install -e ./python[dev]
```

Setup build:
```
mkdir -p build
cd build
cmake ..
```
To run release build use `-DCMAKE_BUILD_TYPE=Release` cmake flag.

Build and run tests:
```
make
make check
```

Install libraries:
```
make install
```
Note: after installation you will need re-install Python library to apply
changes in Python files.

# Running Python and MeTTa examples from command line

In order to run examples you need either install Python library or
add Python libraries into the `PYTHONPATH` after compilation:
```
cd build
export PYTHONPATH=$PYTHONPATH:`pwd`/python:`pwd`/../python
```

Run MeTTa script from command line:
```
cd python\tests
python metta.py ./scripts/<name>.metta
```

# Language support for IDEs [optional]

Different IDEs may require different tweaks to support the languages
used in the codebase. The language servers which we use
for development are:
- [Rust Language Server](https://github.com/rust-lang/rls#setup);
- [clangd](https://clangd.llvm.org/installation), generate compile
  commands for the `clangd` using `cmake` variable:
  ```
  cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=Y ..
  ```
- [Python LSP server](https://github.com/python-lsp/python-lsp-server#installation).

# Troubleshooting

## Conan claims it cannot find out the version of the C compiler

If you see the following `cmake` output:
```
ERROR: Not able to automatically detect '/usr/bin/cc' version
ERROR: Unable to find a working compiler
WARN: Remotes registry file missing, creating default one in /root/.conan/remotes.json
ERROR: libcheck/0.15.2: 'settings.compiler' value not defined
```
Try to create the default Conan profile manually:
```
conan profile new --detect default
```
If it doesn't help, then try to manually add `compiler`, `compiler.version` and
`compiler.libcxx` values in the default Conan profile
(`~/.conan/profiles/default`).
For example:
```
conan profile update settings.compiler=gcc default
conan profile update settings.compiler.version=7 default
conan profile update settings.compiler.libcxx=libstdc++ default
```

## Rust compiler shows errors

Please ensure you are using the latest stable version:
```
rustup update stable
```
