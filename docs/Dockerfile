ARG NODE_VERSION

FROM node:17-alpine3.13 as dev
WORKDIR /proj

RUN apk add make bash patch
COPY docs/dev/Makefile docs/dev/package.json docs/dev/mdast-util-toc.patch ./docs/dev/
RUN (cd docs/dev && make installed)

COPY VERSION .
ADD examples ./examples
ADD rpc-client/py ./rpc-client/py
ADD docs/dev ./docs/dev
RUN (cd docs/dev && make)

FROM scratch

COPY --from=dev /proj/docs/dev/build ./build/
COPY --from=dev /proj/docs/dev/log ./dev.log
COPY --from=dev /proj/docs/dev/searchData.json ./search/index.json
COPY reach ./build/
