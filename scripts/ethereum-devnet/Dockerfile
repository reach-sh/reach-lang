FROM ethereum/client-go:v1.9.21

RUN apk add curl=7.69.1-r1 --no-cache

COPY run.sh /run.sh

RUN /run.sh init

ENTRYPOINT ["/run.sh"]

HEALTHCHECK \
  CMD curl -sSf -X POST \
        -H "Content-Type: application/json" \
        --data '{"jsonrpc":"2.0", "method": "web3_clientVersion", "params":[], "id":67}' http://localhost:8545
