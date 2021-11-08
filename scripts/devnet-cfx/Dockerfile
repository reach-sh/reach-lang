FROM debian:buster-slim as build
ARG CFX_VERSION="v1.1.5-testnet"

RUN apt-get update; apt-get install clang libsqlite3-dev pkg-config libssl-dev cmake curl git openssl -y
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > /rustup.sh
RUN chmod +x /rustup.sh
RUN /rustup.sh -y
RUN /root/.cargo/bin/rustup install 1.47.0
RUN git clone https://github.com/Conflux-Chain/conflux-rust
WORKDIR conflux-rust
RUN git checkout $CFX_VERSION
COPY patch ./patch
RUN patch -d /conflux-rust/core/src/spec/ < patch/genesis-$CFX_VERSION.patch
RUN /root/.cargo/bin/cargo build --release


FROM debian:buster-slim

RUN apt-get update; apt-get install curl -y; apt-get clean
COPY --from=build /conflux-rust/target/release/conflux /
COPY log.yaml default.toml genesis_secret.txt run.sh /

ENTRYPOINT ["/run.sh"]

HEALTHCHECK \
  CMD curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "cfx_clientVersion", "params":[], "id":67}' http://localhost:12537

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
