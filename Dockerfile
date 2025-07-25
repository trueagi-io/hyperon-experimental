FROM python:3.10-slim-bookworm AS os

FROM os AS build

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    apt-get install -y sudo git curl cmake build-essential \
        pkg-config libssl-dev zlib1g-dev protobuf-compiler && \
    rm -rf /var/lib/apt/lists/*

RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN useradd -m -g users user
RUN usermod -aG sudo user
USER user
ENV HOME=/home/user
WORKDIR ${HOME}

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
RUN sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
ENV PATH="${PATH}:${HOME}/.cargo/bin"
RUN cargo install cbindgen

RUN python3 -m pip install conan==2.16.1 pip==23.1.2
ENV PATH="${PATH}:${HOME}/.local/bin"
RUN conan profile detect --force

ADD --chown=user:users . ${HOME}/hyperon-experimental

WORKDIR ${HOME}/hyperon-experimental
RUN cargo test --release
RUN cargo build --features python,git --release

ENV BUILD=${HOME}/hyperon-experimental/build
RUN mkdir ${BUILD}
WORKDIR ${BUILD}
RUN cmake -DCMAKE_BUILD_TYPE=Release ..
RUN make
RUN make check

ENV HYPERONPY=${BUILD}/hyperonpy-install
RUN mkdir ${HYPERONPY}
RUN python3 -m pip install --prefix ${HYPERONPY} ../python

WORKDIR ${HOME}/hyperon-experimental

CMD bash

FROM os

ENV BUILD=/home/user/hyperon-experimental/build
COPY --from=build /home/user/hyperon-experimental/target/release/metta-repl /usr/bin
COPY --from=build ${BUILD}/hyperonc-install /usr
COPY --from=build ${BUILD}/hyperonpy-install /usr/local
COPY --from=build ${BUILD}/../python/VERSION /root/version

RUN cat >>/root/welcome <<EOF
echo ""
echo "  Welcome to the MeTTa $(cat /root/version) running environment."
echo "  Use the following commands to run MeTTa interpreter:"
echo ""
echo "  metta-repl - start Rust based REPL"
echo "  metta-py - start Python based script executor"
echo ""
EOF
RUN echo "sh /root/welcome" >/root/.bashrc

CMD bash
