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
RUN (cd ./docs/scrbl/build && ls *html) | sed 's/\.html//' > ./docs/scrbl/manifest.txt

FROM racket as convert
WORKDIR /proj

RUN raco pkg install --no-setup lang-file lang-file-lib

ADD docs/scrbl ./docs/scrbl
ADD docs/convert ./docs/convert
RUN (cd docs/convert && make)

FROM node:17-alpine3.13 as dev
WORKDIR /proj

RUN apk add make bash
COPY docs/dev/Makefile docs/dev/package.json ./docs/dev/
RUN (cd docs/dev && make installed)

COPY VERSION .
ADD examples ./examples
ADD rpc-client/py ./rpc-client/py
ADD docs/scrbl/images ./docs/dev/src/images
ADD docs/dev ./docs/dev
COPY --from=convert /proj/docs/convert/src ./docs/md/
RUN (cd docs/dev && ./rewire.sh)
COPY --from=convert /proj/docs/convert/src ./docs/md/
COPY --from=render /proj/docs/scrbl/manifest.txt ./docs/dev/
RUN (cd docs/dev && make)
RUN rm ./docs/dev/build/index.html

FROM scratch

COPY --from=render /proj/docs/scrbl/build/ ./build/
COPY --from=render /proj/docs/scrbl/scribble.log ./
# This can be deleted, but I'm leaving it in for inspecting the output for
# debugging
COPY --from=convert /proj/docs/convert/src ./md/
COPY --from=convert /proj/docs/convert/log ./convert.log
COPY --from=dev /proj/docs/dev/build ./build/
COPY --from=dev /proj/docs/dev/log ./dev.log
