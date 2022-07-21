ARG NODE_IMAGE
FROM $NODE_IMAGE as dev

WORKDIR /proj

# Update the extension's hover documentation on every build.
RUN npm install -g ts-node
ADD docs/src/rsh ./docs/src/rsh
ADD docs/xrefs.json ./docs/xrefs.json
ADD examples/ ./examples/
ADD hs/t ./hs/t
ADD vsce/util/updateKeywordDocumentation.ts ./vsce/u/updateDocs.ts
ADD vsce/data/print-keyword-info.json ./vsce/data/print-keyword-info.json
ADD vsce/server/src ./vsce/server/src
RUN (cd vsce/ && ts-node u/updateDocs.ts)

FROM scratch
COPY --from=dev /proj/vsce/server/src/keywordToDocumentation.ts ./kToD.ts
