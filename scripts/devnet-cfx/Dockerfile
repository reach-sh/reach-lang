ARG DEBIAN_NODE_IMAGE
ARG DEBIAN_IMAGE

FROM $DEBIAN_IMAGE as build

RUN apt-get update; apt-get install clang libsqlite3-dev pkg-config libssl-dev cmake curl git openssl -y
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /rustup.sh
RUN chmod +x /rustup.sh
RUN /rustup.sh -y
RUN /root/.cargo/bin/rustup install 1.47.0

RUN apt-get install wget
ARG CFX_VERSION="1.1.7"
RUN wget --quiet https://github.com/Conflux-Chain/conflux-rust/archive/refs/tags/v${CFX_VERSION}.tar.gz && tar -xvf v${CFX_VERSION}.tar.gz
RUN mv conflux-rust-${CFX_VERSION} conflux-rust
WORKDIR conflux-rust
COPY patch ./patch
RUN patch -d ./core/src/spec/ < patch/genesis.patch
RUN /root/.cargo/bin/cargo build --release

FROM $DEBIAN_NODE_IMAGE

# Some of these are weirdly needed to npm install on m1
RUN apt-get update; apt-get install curl make python3 libc-dev gcc g++ -y; apt-get clean

RUN mkdir /faucet
COPY faucet/package.json /faucet
RUN cd /faucet && npm install
COPY faucet/index.mjs /faucet

COPY --from=build /conflux-rust/target/release/conflux /
COPY log.yaml default.toml genesis_secret.txt run.sh /

ENTRYPOINT ["/run.sh"]

HEALTHCHECK \
  CMD curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "cfx_clientVersion", "params":[], "id":67}' http://localhost:12537

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
