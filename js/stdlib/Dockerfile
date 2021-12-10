ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

ARG REACH_VERSION
ARG JS_DEPS_IMAGE=reachsh/js-deps:$REACH_VERSION

# hadolint ignore=DL3006
FROM $JS_DEPS_IMAGE as js-deps

FROM reachsh/reach AS reach

# hadolint ignore=DL3006
FROM $NODE_IMAGE AS prepare

WORKDIR /stdlib
COPY --from=js-deps /js-deps/node_modules ./node_modules
RUN apk update && apk add git make python3 libc-dev gcc g++ curl bash
COPY Makefile.docker ./Makefile
COPY package.mo.json .
COPY .docker-root ./.docker-root
RUN make ROOT=.docker-root install-mo package.json

# The above should cover all `npm install` needs
# RUN npm install

RUN npm link

COPY ts /stdlib/ts
COPY --from=reach /stdlib_sol.json .
RUN make ROOT=.docker-root ts/version.ts ts/stdlib_sol.ts

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
RUN apk update && apk add git make python3 libc-dev gcc g++
# XXX does this need to be uncommented?
# RUN npm link

# XXX redundant
# COPY *.mjs /stdlib/

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
