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

RUN useradd -g users relex
RUN mkdir /home/relex
RUN chown relex:users /home/relex
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
run apt update
RUN echo "N" | DEBIAN_FRONTEND=noninteractive apt install sudo
run usermod -aG sudo relex
run chown relex:users -R /usr/local/


USER relex
WORKDIR /home/relex
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > rustup.sh
RUN sh rustup.sh -y
ENV PATH="${PATH}:/home/relex/.local/bin"
ENV PATH="${PATH}:/home/relex/.cargo/bin"
RUN echo $PATH
RUN cargo install cbindgen

RUN echo "export PATH=\${PATH}:/home/relex/.local/bin" >> ~/.bashrc
RUN cat /home/relex/.bashrc
RUN python3 -m pip install conan==1.47
RUN conan profile new --detect default

RUN git clone https://github.com/trueagi-io/hyperon-experimental.git
WORKDIR ./hyperon-experimental
RUN python3 -m pip install -e ./python[dev]

RUN cd ./lib && cargo build
RUN mkdir -p build && cd build && cmake .. && make 
RUN cd build && make install

RUN echo "export LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/usr/local/lib/" >> ~/.bashrc
