ARG DOCS_IMAGE
FROM $DOCS_IMAGE as dev

WORKDIR /proj

RUN apk add make bash patch
ADD docs/dev ./docs/dev
RUN (cd docs/dev && make installed)

COPY VERSION .
ADD examples ./examples
ADD rpc-client/py ./rpc-client/py
ADD docs/src ./docs/src
RUN (cd docs/dev && make)

FROM scratch

COPY --from=dev /proj/docs/build ./build/
COPY --from=dev /proj/docs/dev/log ./dev.log
COPY --from=dev /proj/docs/dev/searchData.json ./search/index.json
COPY reach ./build/
