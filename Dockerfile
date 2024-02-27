FROM ubuntu:22.04

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
    apt-get install -y sudo git python3 python3-pip curl gcc cmake && \
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

RUN python3 -m pip install conan==1.62 pip==23.1.2
ENV PATH="${PATH}:/home/user/.local/bin"
RUN conan profile new --detect default

ADD --chown=user:users . ${HOME}/hyperon-experimental
WORKDIR ${HOME}/hyperon-experimental
RUN cargo test

RUN mkdir build
WORKDIR ${HOME}/hyperon-experimental/build
RUN cmake -DCMAKE_BUILD_TYPE=Release ..
RUN make
RUN make check

WORKDIR ${HOME}/hyperon-experimental
RUN python3 -m pip install -e ./python[dev]
