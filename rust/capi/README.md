# Try C API

```
cbindgen -c cbindgen.toml -o hyperon.h
gcc -o chyperon chyperon.c ../target/debug/libhyperon_capi.a -lpthread -ldl -lm
./chyperon
```
