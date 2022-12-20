ARG DOCS_IMAGE
FROM $DOCS_IMAGE as gen

WORKDIR /proj

RUN apk add make bash patch
ADD docs/gen ./docs/gen
RUN (cd docs/gen && make installed)

COPY VERSION .
ADD rpc-client/py ./rpc-client/py
ADD hs/t ./hs/t
ADD examples ./examples
ADD docs/src ./docs/src
RUN (cd docs/gen && make)

FROM scratch

COPY --from=gen /proj/docs/build ./build/
COPY --from=gen /proj/docs/gen/log ./gen.log
COPY --from=gen /proj/docs/gen/xrefs.json ./xrefs.json
COPY --from=gen /proj/docs/gen/searchData.json ./search/index.json
COPY reach ./build/
