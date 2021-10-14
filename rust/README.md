# Prerequisites

Install Rust v1.55, see [Rust installation
page](https://www.rust-lang.org/tools/install)

# Build project and run tests

```
cargo build
cargo test
```

# Test C API

Prerequisites:
```
pip install conan
cargo install cbindgen
```

Setup build:
```
mkdir -p target/capi
cd target/capi
conan install ../../capi
cmake ../../capi
```

Build and run tests:
```
make
make test_verbose
```

# Setup IDE

See [Rust Language Server](https://github.com/rust-lang/rls) page.
