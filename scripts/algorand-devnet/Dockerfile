FROM ubuntu:18.04
ENV DEBIAN_FRONTEND noninteractive

RUN apt update && apt-get install -y git libboost-all-dev wget sqlite3 autoconf sudo tzdata bsdmainutils

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
ENTRYPOINT ["/bin/bash"]
