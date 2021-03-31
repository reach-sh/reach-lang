ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

ARG REACH_VERSION
ARG JS_DEPS_IMAGE=reachsh/js-deps:$REACH_VERSION

# hadolint ignore=DL3006
FROM $JS_DEPS_IMAGE as js-deps

# hadolint ignore=DL3006
FROM $NODE_IMAGE

WORKDIR /stdlib
COPY --from=js-deps /js-deps/node_modules /stdlib/node_modules
COPY package.json /stdlib/
# The above should cover all `npm install` needs
# RUN npm install

RUN npm link

COPY ts/* /stdlib/ts/
COPY types/* /stdlib/types/
# We need .gitignore for eslint, weirdly
COPY tsconfig.json \
     .eslintrc.yaml .eslintignore .gitignore \
     /stdlib/
COPY sbin/* /stdlib/sbin/

RUN npm run tsc
RUN ./sbin/fix.sh
RUN rm ./sbin/fix.sh

RUN npm link

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
