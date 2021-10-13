# Prerequisites

Install Rust v1.55, see [Rust installation
page](https://www.rust-lang.org/tools/install)

# Build project and run tests

```
cargo build
cargo test
```

# Test C API

```
mkdir target/capi
cd target/capi
cmake ../../capi
make
make test_verbose
```
