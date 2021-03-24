FROM golang:1.14-alpine as builder

RUN apk add --no-cache \
  ca-certificates \
  make \
  git \
  bash \
  patch \
  libtool

ENV SRC_PARENT $GOPATH/src/github.com/algorand

### ALGOD
WORKDIR $SRC_PARENT
# algod release (doesn't work)
ARG ALGO_VERSION=2.5.2-beta
RUN wget --quiet https://github.com/algorand/go-algorand/archive/refs/tags/v${ALGO_VERSION}.tar.gz && tar -xvf v${ALGO_VERSION}.tar.gz
WORKDIR $SRC_PARENT/go-algorand-${ALGO_VERSION}
RUN ./scripts/configure_dev-deps.sh
COPY reach.patch .
RUN patch -p1 < reach.patch
RUN apk add --no-cache \
  autoconf \
  automake \
  gcc \
  boost-dev \
  g++ \
  shellcheck \
  sqlite
RUN make install

### INDEXER
WORKDIR $SRC_PARENT
# indexer release (doesn't work)
# ARG INDEXER_VERSION=2.2.1
# RUN wget --quiet https://github.com/algorand/indexer/archive/refs/tags/${INDEXER_VERSION}.tar.gz && tar -xvf ${INDEXER_VERSION}.tar.gz
# WORKDIR $SRC_PARENT/indexer-${INDEXER_VERSION}
# indexer develop branch as of 2021-03-19
RUN git clone https://github.com/algorand/indexer && cd indexer && git checkout e54c4d97df60e2a6768abb3d97919f3412e58597
WORKDIR $SRC_PARENT/indexer
RUN make
RUN cp cmd/algorand-indexer/algorand-indexer /go/bin

FROM alpine:latest

RUN echo "@testing https://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories

RUN apk add --no-cache \
  ca-certificates \
  racket@testing

WORKDIR /

### ALGOD & INDEXER
COPY --from=builder /go/bin/algod /go/bin/algorand-indexer /go/bin/goal /bin

### DEBUGGER
COPY server.rkt /

### Setup
ENV ALGORAND_NETWORK=/var/lib/algorand_network
ENV ALGORAND_DATA=$ALGORAND_NETWORK/Primary
COPY algorand_network/ $ALGORAND_NETWORK/
COPY algorand_data/* $ALGORAND_DATA/
COPY start.sh /

# pre-run some stuff so that it starts up faster
COPY prime-algod.sh /
RUN /prime-algod.sh

EXPOSE 9392
EXPOSE 4180
EXPOSE 8980

RUN apk add --no-cache \
  bash

ENTRYPOINT ["/bin/bash", "/start.sh"]
ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"

