FROM ubuntu:18.04
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
  autoconf=2.69-11 \
  automake=1:1.15.1-3ubuntu2 \
  bsdmainutils=11.1.2ubuntu1 \
  build-essential=12.4ubuntu1 \
  ca-certificates=20190110~18.04.1 \
  git=1:2.17.1-1ubuntu0.7 \
  libboost-all-dev=1.65.1.0ubuntu1 \
  libtool=2.4.6-2 \
  sqlite3=3.22.0-1ubuntu0.4 \
  sudo=1.8.21p2-3ubuntu1.2 \
  systemd=237-3ubuntu10.42 \
  tzdata=2020a-0ubuntu0.18.04 \
  wget=1.19.4-1ubuntu2.2 \
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

# algod
WORKDIR $SRC_PARENT
RUN git clone https://github.com/jeapostrophe/go-algorand
WORKDIR $SRC_PARENT/go-algorand
# RUN git checkout 595aa4722824be2bbad92ec367267584a87f29fe && ./scripts/configure_dev.sh && make install
RUN ./scripts/configure_dev.sh
RUN git fetch && git reset --hard 3bfd1918a5085b882d8af91b5bd8ca348b774d87
RUN make install
RUN git fetch && git reset --hard b61cf649
RUN make install

# indexer
WORKDIR $SRC_PARENT
RUN git clone https://github.com/algorand/indexer
WORKDIR $SRC_PARENT/indexer
RUN make
RUN cp $SRC_PARENT/indexer/cmd/algorand-indexer/algorand-indexer $GOPATH/bin

# debugger
WORKDIR /
RUN apt-get install -y --no-install-recommends \
  software-properties-common=0.96.24.32.14 \
  && add-apt-repository -y ppa:plt/racket \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
  racket=7.8+ppa1-1~bionic1 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
COPY server.rkt /

WORKDIR /
ENV ALGORAND_NETWORK=/var/lib/algorand_network
ENV ALGORAND_DATA=$ALGORAND_NETWORK/Primary
COPY algorand_network/ $ALGORAND_NETWORK/
COPY algorand_data/* $ALGORAND_DATA/
COPY start.sh /

EXPOSE 9392
EXPOSE 4180
EXPOSE 8980

ENTRYPOINT ["/bin/bash", "/start.sh"]
ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
