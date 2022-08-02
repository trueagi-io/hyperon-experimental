# This docker image simulates the GitHub build environment to check CI builds
# locally.
# Build image:
#   docker build -t hyperon-ci -f Dockerfile .
# Run image:
#   docker run --rm -ti hyperon-ci
FROM ubuntu:22.04

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    TZ=UTC \
	apt-get install -y git python3 python3-pip curl gcc cmake && \
	rm -rf /var/lib/apt/lists/*

RUN useradd -m -g users user
USER user
WORKDIR /home/user

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /tmp/rustup.sh
RUN sh /tmp/rustup.sh -y && rm /tmp/rustup.sh
ENV PATH="${PATH}:/home/user/.cargo/bin"
RUN cargo install cbindgen

RUN python3 -m pip install conan==1.47
ENV PATH="${PATH}:/home/user/.local/bin"
RUN conan profile new --detect default

RUN git clone https://github.com/trueagi-io/hyperon-experimental.git
WORKDIR ./hyperon-experimental
RUN python3 -m pip install -e ./python[dev]
