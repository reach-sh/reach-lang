FROM confluxchain/conflux-rust:1.1.4 as builder

WORKDIR /root/run/scripts
RUN node initConfig.js

FROM debian:buster-slim

COPY --from=builder /usr/local/bin/conflux /
COPY --from=builder /root/run/log.yaml /
COPY --from=builder /root/run/default.toml /
COPY --from=builder /root/run/genesis_secret.txt /

COPY run.sh /

ENTRYPOINT ["/run.sh"]

HEALTHCHECK \
  CMD curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "cfx_clientVersion", "params":[], "id":67}' http://localhost:12537

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
