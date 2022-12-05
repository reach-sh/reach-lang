ARG GOLANG_IMAGE
ARG ALPINE_IMAGE
FROM $GOLANG_IMAGE as build

RUN apk add --no-cache \
  ca-certificates \
  make \
  git \
  bash \
  patch \
  libtool \
  autoconf \
  automake \
  gcc \
  boost-dev \
  g++ \
  sqlite

ENV SRC_PARENT ${GOPATH}/src/github.com/algorand

### ALGOD
WORKDIR ${SRC_PARENT}
# algod release
ARG ALGO_VERSION=3.12.2-stable
RUN wget --quiet https://github.com/algorand/go-algorand/archive/refs/tags/v${ALGO_VERSION}.tar.gz && tar -xvf v${ALGO_VERSION}.tar.gz
WORKDIR ${SRC_PARENT}/go-algorand-${ALGO_VERSION}
RUN ./scripts/configure_dev-deps.sh
RUN make install
COPY reach.patch .
RUN patch -p1 < reach.patch
RUN make install
COPY reach2.patch .
RUN patch -p1 < reach2.patch
RUN make install
COPY reach3.patch .
RUN patch -p1 < reach3.patch
RUN make install

### INDEXER
WORKDIR ${SRC_PARENT}
# indexer release
ARG INDEXER_VERSION=2.15.0

#RUN wget --quiet https://github.com/algorand/indexer/archive/refs/tags/${INDEXER_VERSION}.tar.gz && tar -xvf ${INDEXER_VERSION}.tar.gz
#WORKDIR ${SRC_PARENT}/indexer-${INDEXER_VERSION}

RUN git clone https://github.com/algorand/indexer && cd indexer && git checkout ${INDEXER_VERSION}
WORKDIR ${SRC_PARENT}/indexer

RUN make
RUN cp cmd/algorand-indexer/algorand-indexer /go/bin

FROM $ALPINE_IMAGE as prepare

RUN apk add --no-cache \
  bash

WORKDIR /

### ALGOD & INDEXER
COPY --from=build /go/bin/algod /go/bin/algorand-indexer /go/bin/goal /go/bin/kmd /bin/

### Setup
ENV ALGORAND_NETWORK=/var/lib/algorand_network
ENV ALGORAND_DATA=${ALGORAND_NETWORK}/Primary
COPY algorand_network/ ${ALGORAND_NETWORK}/
COPY algorand_data/* ${ALGORAND_DATA}/

RUN chmod -R 700 ${ALGORAND_DATA}/kmd*

# pre-run some stuff so that it starts up faster
COPY prime-algod.sh /
RUN /prime-algod.sh

FROM $ALPINE_IMAGE as generate
COPY --from=build /go/bin/goal /go/bin/algod /go/bin/kmd /bin/
COPY network_template.json .
COPY generate.sh .

#ENV ALGORAND_NETWORK=algorand_network
#ENV ALGORAND_DATA=${ALGORAND_NETWORK}/Primary
#COPY ${ALGORAND_NETWORK}/FAUCET.0.3000000.partkey ${ALGORAND_NETWORK}/
#COPY ${ALGORAND_NETWORK}/FAUCET.rootkey ${ALGORAND_NETWORK}/
#COPY ${ALGORAND_NETWORK}/FAUCET.0.3000000.partkey ${ALGORAND_DATA}/devnet-v1/
#COPY ${ALGORAND_NETWORK}/FAUCET.rootkey ${ALGORAND_DATA}/devnet-v1/
#COPY ${ALGORAND_DATA}/kmd-v0.5/sqlite_wallets/ {ALGORAND_DATA}/sqlite_wallets/

RUN ./generate.sh

FROM scratch as ignore-generate-out
COPY --from=generate /algorand_network /

FROM $ALPINE_IMAGE

RUN apk add --no-cache bash libstdc++ curl

# Install Postgres
ENV POSTGRES_PORT=5432
ENV POSTGRES_DATA=/postgres
ENV POSTGRES_USER=algogrand
ENV POSTGRES_PASSWORD=indexer
ENV POSTGRES_DB=pgdb
COPY install-postgres.sh /
COPY wait-for-postgres.sh /
RUN bash install-postgres.sh

ENV ALGORAND_NETWORK=/var/lib/algorand_network
ENV ALGORAND_DATA=${ALGORAND_NETWORK}/Primary

COPY --from=build /go/bin/goal /go/bin/algod /go/bin/algorand-indexer /bin/
COPY --from=prepare /var/lib/algorand_network/ /var/lib/algorand_network/

### Our stuff
COPY rotate_dbg.sh /
COPY start.sh /
COPY daily-ping.sh /

EXPOSE 4180
EXPOSE 8980

ENTRYPOINT ["/bin/bash", "/start.sh"]
ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"

