ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

# hadolint ignore=DL3006
FROM $NODE_IMAGE

WORKDIR /js_deps
COPY package.json package-lock.json \
     /js_deps/
RUN npm install

ENTRYPOINT ["bin/sh"]
CMD []

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
