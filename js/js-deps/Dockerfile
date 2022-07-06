# hadolint ignore=DL3006
ARG NODE_IMAGE
FROM $NODE_IMAGE

RUN apk update && apk add git make python3 libc-dev gcc g++
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
