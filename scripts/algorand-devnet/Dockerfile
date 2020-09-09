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

WORKDIR $SRC_PARENT
ARG GIT_CLONE_REPO=https://github.com/algorand/go-algorand
ARG GIT_COMMIT=b648e0d75ac89440afb435b7acbad1d6711d24ad
# ^ master as of Sept 7, 2020
ENV PROJ_NAME go-algorand
ENV SRC $SRC_PARENT/$PROJ_NAME
RUN git clone $GIT_CLONE_REPO $PROJ_NAME

WORKDIR $SRC
RUN git checkout $GIT_COMMIT && ./scripts/configure_dev.sh && make install

ENV ALGORAND_NETWORK=/var/lib/algorand_network
ENV ALGORAND_DATA=$ALGORAND_NETWORK/Primary

COPY algorand_network/ $ALGORAND_NETWORK/

EXPOSE 4180

# You can't use env vars in ENTRYPOINT/CMD
# otherwise we'd use $ALGORAND_DATA
ENTRYPOINT ["algod", "-d", "/var/lib/algorand_network/Primary"]
