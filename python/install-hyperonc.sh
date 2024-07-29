#!/bin/sh

HYPERONC_URL=https://github.com/trueagi-io/hyperon-experimental.git
HYPERONC_REV=main
while getopts 'u:r:' opt; do
    case "$opt" in
        u)
            HYPERONC_URL="$OPTARG"
            ;;
        r)
            HYPERONC_REV="$OPTARG"
            ;;
        ?|h)
            echo "Usage: $(basename $0) [-u hyperonc_repo_url] [-r hyperonc_revision]"
            echo "-u hyperonc_repo_url    Git repo URL to get hyperonc source code"
            echo "-r hyperonc_revision    Revision of hyperonc to get from Git"
            exit 1
            ;;
    esac
done

echo "hyperonc repository URL: $HYPERONC_URL"
echo "hyperonc revision: $HYPERONC_REV"

# This is to build subunit from Conan on CentOS based manylinux images.
if test "$AUDITWHEEL_POLICY" = "manylinux2014"; then
    yum install -y perl-devel openssl-devel zlib-devel
fi

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
export PATH="${PATH}:${HOME}/.cargo/bin"
cargo install cbindgen

python3 -m pip install cmake==3.24 conan==2.5.0 pip==23.1.2
PATH="${PATH}:${HOME}/.local/bin"
conan profile detect --force

mkdir -p ${HOME}/hyperonc
cd ${HOME}/hyperonc
git init
git remote add origin $HYPERONC_URL
git fetch --depth=1 origin $HYPERONC_REV
git reset --hard FETCH_HEAD

mkdir -p ${HOME}/hyperonc/c/build
cd ${HOME}/hyperonc/c/build

# Rust doesn't support building shared libraries under musllinux environment, so musllinux is currently unsupported
CMAKE_ARGS="$CMAKE_ARGS -DBUILD_SHARED_LIBS=ON"
# Local prefix is used to support MacOSX Apple Silicon GitHub actions environment.
CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${HOME}/.local"
CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_BUILD_TYPE=Release"
CMAKE_ARGS="$CMAKE_ARGS -DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=${HOME}/hyperonc/conan_provider.cmake"
echo "hyperonc CMake arguments: $CMAKE_ARGS"
cmake $CMAKE_ARGS ..
make
make check
make install
