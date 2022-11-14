FROM ethereum/client-go:v1.10.26

# hadolint ignore=DL3018
RUN apk add curl --no-cache

COPY run.sh /run.sh
COPY daily-ping.sh /

RUN /run.sh init

ENTRYPOINT ["/run.sh"]

HEALTHCHECK \
  CMD curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' http://localhost:8545

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
