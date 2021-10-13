# Prerequisites

Install Rust v1.55 or higher:
```
apt-get install rust
```

# Build project and run tests

```
cargo test
```

# Build and try C API

```
cbindgen -c cbindgen.toml -o hyperon.h
gcc -o chyperon chyperon.c ./target/debug/libhyperon.a -lpthread -ldl -lm
./chyperon
```
