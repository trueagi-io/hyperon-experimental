FROM ubuntu:22.04 AS os

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    apt-get install -y python3 python3-pip

FROM os AS build

RUN DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    apt-get install -y sudo git curl gcc cmake \
        pkg-config libssl-dev zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN useradd -m -g users user
RUN usermod -aG sudo user
USER user
ENV HOME=/home/user
WORKDIR ${HOME}

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
RUN sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
ENV PATH="${PATH}:/home/user/.cargo/bin"
RUN cargo install cbindgen

RUN python3 -m pip install conan==1.64 pip==23.1.2
ENV PATH="${PATH}:/home/user/.local/bin"
RUN conan profile new --detect default

ADD --chown=user:users . ${HOME}/hyperon-experimental

ENV PREFIX=${HOME}/prefix
RUN mkdir ${PREFIX}

WORKDIR ${HOME}/hyperon-experimental
RUN cargo test
RUN cargo build --release

ENV BUILD=${HOME}/hyperon-experimental/build
RUN mkdir ${BUILD}
WORKDIR ${BUILD}
RUN cmake -DCMAKE_BUILD_TYPE=Release ..
RUN make
RUN make check

ENV HYPERONPY=${BUILD}/hyperonpy-install
RUN mkdir ${HYPERONPY}
RUN python3 -m pip install --prefix ${HYPERONPY} ../python

FROM os

RUN rm -rf /var/lib/apt/lists/*

ENV BUILD=/home/user/hyperon-experimental/build
COPY --from=build /home/user/hyperon-experimental/target/release/metta /usr/bin/metta-rust
COPY --from=build ${BUILD}/hyperonc-install /usr
COPY --from=build ${BUILD}/hyperonpy-install /usr

