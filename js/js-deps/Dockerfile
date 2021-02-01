ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

# hadolint ignore=DL3006
FROM $NODE_IMAGE

WORKDIR /js-deps
COPY package.json package-lock.json \
     /js-deps/
RUN npm install

# TODO: remove this eventually
COPY langspec.json /app/node_modules/algosdk/src/logic/

ENTRYPOINT ["bin/sh"]
CMD []

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
