# Howto Build and Publish Documentation

## Building Rust Documentation

Run the following command under the root directory of the repository:
```
cargo doc --no-deps
```
Docs can be found at `./target/doc/hyperon/index.html`.

## Building C Documentation

1. `doxygen` must be installed. Depending on your platform it may be easiest to use a package manager.
    - using apt: `sudo apt-get install doxygen`
    - using homebrew (Mac): `brew install doxygen`
    - installing from source: [https://www.doxygen.nl/manual/install.html]

2. Build the Hyperon C library, following the instructions here: [https://github.com/trueagi-io/hyperon-experimental?tab=readme-ov-file#c-and-python-api]

3. Set CMake environment variables. Relative to the `build` directory, run the following:
    - `export CMAKE_CURRENT_SOURCE_DIR=../c/`
    - `export HYPERONC_INCLUDE_DIR=./hyperonc-install/include/hyperonc/hyperon/`

4. Invoke `doxygen` using the following command: `doxygen ../c/hyperonc.doxyfile`

The top page for the rendered HTML results will be written to `./html/index.html`, and latex results will be similarly written to `./latex/index.tex`

## Building MeTTa Standard Library Documentation

Run the following command under the root directory of the repository:
```
cargo run --release --bin metta-repl ./mkdocs.metta
```

## Building Python Documentation

Install `mkdocs` and dependencies.
```
pip install mkdocs-material==9.7.0 \
    mkdocs-minify-plugin==0.8.0 \
    mkdocstrings[python]==0.26.1
```

Run the following command under the root directory of the repository:
```
mkdocs build
```

## Publishing the Documentation

Make all of three steps above. Then copy Rust and C API documentation under the
common site tree:
```
cp -R ./target/doc ./site/rust
cp -R ./build/html ./site/c
```

Publish `./site` directory as `gh-pages` branch of the repository:
```
ghp-import -m 'Deploy documentation' \
    --push \
    --force \
    --no-history \
    --remote=<git remote repo> \
    --branch=gh-pages \
    ./site
```
