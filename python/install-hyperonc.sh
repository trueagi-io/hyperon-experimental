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
            exit 1
            ;;
    esac
done

echo "hyperonc repository URL $HYPERONC_URL"
echo "hyperonc revision $HYPERONC_REV"

# This is to build subunit from Conan on CentOS based manylinux images.
if test "$AUDITWHEEL_POLICY" = "manylinux2014"; then
    yum install -y perl-devel
fi

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
export PATH="${PATH}:${HOME}/.cargo/bin"
cargo install cbindgen

python3 -m pip install conan==1.60.2 pip==23.1.2
PATH="${PATH}:${HOME}/.local/bin"
conan profile new --detect default

mkdir -p ${HOME}/hyperonc
cd ${HOME}/hyperonc
git init
git remote add origin $HYPERONC_URL
git fetch --depth=1 origin $HYPERONC_REV
git reset --hard FETCH_HEAD

mkdir -p ${HOME}/hyperonc/c/build
cd ${HOME}/hyperonc/c/build
# Rust doesn't support building shared libraries under musllinux environment
cmake -DBUILD_SHARED_LIBS=OFF -DCMAKE_BUILD_TYPE=Release ..
make
make check
make install
