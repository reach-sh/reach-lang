FROM confluxchain/conflux-rust:1.1.5-testnet as builder

FROM debian:buster-slim

COPY --from=builder /root/run/conflux /
COPY log.yaml default.toml genesis_secret.txt run.sh /

ENTRYPOINT ["/run.sh"]

HEALTHCHECK \
  CMD curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "cfx_clientVersion", "params":[], "id":67}' http://localhost:12537

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
