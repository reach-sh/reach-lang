FROM ubuntu:18.04
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
#  autoconf=2.69-11 \
#  automake=1:1.15.1-3ubuntu2 \
#  bsdmainutils=11.1.2ubuntu1 \
#  build-essential=12.4ubuntu1 \
#  ca-certificates=20190110~18.04.1 \
#  git=1:2.17.1-1ubuntu0.7 \
#  libboost-all-dev=1.65.1.0ubuntu1 \
#  libtool=2.4.6-2 \
#  sqlite3=3.22.0-1ubuntu0.4 \
#  sudo=1.8.21p2-3ubuntu1.2 \
#  systemd=237-3ubuntu10.42 \
#  tzdata=2020a-0ubuntu0.18.04 \
#  wget=1.19.4-1ubuntu2.2 \
  autoconf \
  automake \
  bsdmainutils \
  build-essential \
  ca-certificates \
  git \
  libboost-all-dev \
  libtool \
  sqlite3 \
  sudo \
  systemd \
  tzdata \
  wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /root
ARG GOLANG_VERSION=1.14.7
RUN wget --quiet https://dl.google.com/go/go${GOLANG_VERSION}.linux-amd64.tar.gz && tar -xvf go${GOLANG_VERSION}.linux-amd64.tar.gz && mv go /usr/local
ENV GOROOT /usr/local/go
ENV GOPATH /go
ENV GOBIN /go/bin
ENV PATH   $GOPATH/bin:$GOROOT/bin:$PATH
ENV SRC_PARENT $GOPATH/src/github.com/algorand
RUN mkdir -p $SRC_PARENT

### ALGOD
WORKDIR $SRC_PARENT

# algod release (doesn't work)
ARG ALGO_VERSION=v2.5.1-beta
RUN wget --quiet https://github.com/algorand/go-algorand/archive/refs/tags/${ALGO_VERSION}.tar.gz && tar -xvf ${ALGO_VERSION}.tar.gz
ARG ALGO_DIRVERSION=2.5.1-beta
WORKDIR $SRC_PARENT/go-algorand-${ALGO_DIRVERSION}
RUN ./scripts/configure_dev.sh
RUN make install
COPY reach.patch .
RUN patch -p1 < reach.patch
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
RUN cp cmd/algorand-indexer/algorand-indexer $GOPATH/bin

### DEBUGGER
WORKDIR /
RUN apt-get update && apt-get install -y --no-install-recommends \
  software-properties-common \
#  software-properties-common=0.96.24.32.14 \
  && add-apt-repository -y ppa:plt/racket \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
  racket \
#  racket=8.0+ppa1-5~bionic1 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
COPY server.rkt /

WORKDIR /
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

ENTRYPOINT ["/bin/bash", "/start.sh"]
ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
