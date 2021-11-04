ARG NODE_VERSION

FROM ubuntu:latest AS racket

RUN apt-get update
RUN apt-get install -y --no-install-recommends \
    software-properties-common

RUN add-apt-repository -y ppa:plt/racket && apt update
RUN apt install -y --no-install-recommends \
    libcairo2 \
    libfontconfig1 \
    libjpeg62 \
    libpangocairo-1.0-0 \
    racket \
    python3-setuptools

RUN apt-get install -y --no-install-recommends \
    build-essential

FROM racket as render
WORKDIR /proj

ADD pygments ./pygments
RUN (cd pygments && make install)

COPY VERSION reach .
ADD examples ./examples
ADD rpc-client/py ./rpc-client/py
ADD docs/scrbl ./docs/scrbl
RUN (cd docs/scrbl && make render)

FROM racket as convert
WORKDIR /proj

ADD docs/scrbl ./docs/scrbl
ADD docs/convert ./docs/convert
RUN raco pkg install --no-setup lang-file lang-file-lib
RUN (cd docs/convert && make)

FROM node:17-alpine3.13 as dev
WORKDIR /proj

RUN apk add make
ADD docs/dev ./docs/dev
RUN (cd docs/dev && make installed)
RUN (cd docs/dev && make)
WORKDIR /proj/docs/dev

FROM scratch

COPY --from=render /proj/docs/scrbl/build/ ./build/
COPY --from=render /proj/docs/scrbl/scribble.log ./
COPY --from=convert /proj/docs/convert/src ./md/
COPY --from=convert /proj/docs/convert/log ./convert.log
COPY --from=dev /proj/docs/dev/build ./build/
