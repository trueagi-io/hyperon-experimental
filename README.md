![CI](https://github.com/trueagi-io/hyperon-experimental/actions/workflows/ci.yml/badge.svg)

# Overview

This is reimplementation of the [C++ Hyperon prototype](https://github.com/trueagi-io/hyperon) from scratch in Rust programming language.
The goal of the project is to replace the previous prototype but work is in progress so some features are absent.
Please see [Python examples](https://github.com/trueagi-io/hyperon/tree/master/python/tests) of previous version to become familiar with Hyperon features.

# Prerequisites

Install Rust v1.55, see [Rust installation
page](https://www.rust-lang.org/tools/install)

# Hyperon library

Build and test the library:
```
cd ./lib
cargo build
cargo test
```

# C, Python, C++ API

Prerequisites:
```
pip install conan nosetests
cargo install cbindgen
```

Setup build:
```
mkdir -p build
cd build
cmake ..
```

Build and run tests:
```
make
make check
```

To run release build use following instead of `cmake ..`:
```
cmake -DCMAKE_BUILD_TYPE=Release ..
```

# Setup IDE

See [Rust Language Server](https://github.com/rust-lang/rls) page.

In order to use clangd server generate compile commands using cmake var:
```
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=Y ..
```
