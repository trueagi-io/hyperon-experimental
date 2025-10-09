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

if test "$AUDITWHEEL_POLICY" = "manylinux2014"; then
    yum install -y perl-devel openssl-devel zlib-devel libatomic
fi

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
export PATH="${PATH}:${HOME}/.cargo/bin"
cargo install cbindgen

python3 -m pip install cmake==3.24 conan==2.19.1 pip==23.1.2
PATH="${PATH}:${HOME}/.local/bin"
conan profile detect --force

# protobuf-compiler (v3) is required by Das
OS=$(uname)
ARCH=$(uname -m)
echo "machine OS: $OS"
echo "machine ARCH: $ARCH"
case "$OS" in
    Linux*)     OS_TAG=linux;;
    Darwin*)    OS_TAG=osx;;
    CYGWIN*)    OS_TAG=win64;;
    MINGW*)     OS_TAG=win64;;
    MSYS_NT*)   OS_TAG=win64;;
    *)          OS_TAG=$OS
esac
case "$ARCH" in
    aarch64)    ARCH_TAG=aarch_64;;
    arm64)      ARCH_TAG=aarch_64;;
    i686)       ARCH_TAG=x86_32;;
    *)          ARCH_TAG=$ARCH
esac
PROTOC_ZIP=protoc-31.1-$OS_TAG-$ARCH_TAG.zip
curl -OL https://github.com/protocolbuffers/protobuf/releases/download/v31.1/$PROTOC_ZIP
unzip -o $PROTOC_ZIP -d ${HOME}/.local
rm -f $PROTOC_ZIP

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
