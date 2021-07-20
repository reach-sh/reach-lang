ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

# hadolint ignore=DL3006
FROM $NODE_IMAGE

WORKDIR /js-deps
COPY package.json /js-deps/
COPY package-lock.json /js-deps/maybe
ARG REFRESH_LOCK
ENV REFRESH_LOCK="${REFRESH_LOCK}"
RUN [ "x${REFRESH_LOCK}" = "xY" ] || mv /js-deps/maybe /js-deps/package-lock.json
RUN npm install

ENTRYPOINT ["bin/sh"]
CMD []

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
