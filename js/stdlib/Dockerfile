ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION
ARG REACH_VERSION
ARG SOLC_VERSION
ARG JS_DEPS_IMAGE=reachsh/js-deps:$REACH_VERSION

FROM reachsh/haskell-build-artifacts as haskell

FROM ethereum/solc:${SOLC_VERSION} as solc

# hadolint ignore=DL3006
FROM $JS_DEPS_IMAGE as js-deps

FROM node:14.17.0 as sol
ARG REACH_VERSION
ENV REACH_VERSION=${REACH_VERSION}
RUN curl -sSL https://git.io/get-mo -o mo
RUN chmod +x mo
RUN mv mo /usr/local/bin
WORKDIR /stdlib
COPY --from=solc /usr/local/bin/solc /stdlib/solc
RUN chmod +x /stdlib/solc
COPY ts /stdlib/ts
COPY --from=haskell /build/sol /stdlib/sol/
WORKDIR /stdlib/sol
RUN echo $(echo "export default" ; /stdlib/solc --optimize --combined-json abi,bin stdlib.sol) > /stdlib/ts/stdlib_sol.ts

# hadolint ignore=DL3006
FROM $NODE_IMAGE AS prepare
ARG REACH_VERSION
ENV REACH_VERSION=${REACH_VERSION}
RUN apk update; apk add bash curl
WORKDIR /stdlib
COPY --from=js-deps /js-deps/node_modules /stdlib/node_modules
COPY ./package.json .
#COPY --from=sol /usr/local/bin/mo /usr/local/bin/mo
#RUN mo package.mo.json > package.json

RUN npm link

COPY --from=sol /stdlib/ts /stdlib/ts
# uncomment if we ever need custom types again
# COPY types /stdlib/types

# We need .gitignore for eslint, weirdly
COPY tsconfig*.json \
     webpack.config.js \
     index.ts \
     .eslintrc.yaml .eslintignore .gitignore \
     launchToken.mjs \
     /stdlib/
COPY sbin/* /stdlib/sbin/

RUN npm run build
RUN ./sbin/fix.sh
RUN mv ./dist/mjs/* ./
RUN rm ./sbin/fix.sh

FROM $NODE_IMAGE 
COPY --from=prepare /stdlib /stdlib
RUN apk add git

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"