ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION
ARG REACH_VERSION
ARG STDLIB_IMAGE=reachsh/stdlib:$REACH_VERSION
ARG JS_DEPS_IMAGE=reachsh/js-deps:$REACH_VERSION

# hadolint ignore=DL3006
FROM $JS_DEPS_IMAGE AS js-deps

# hadolint ignore=DL3006
FROM $STDLIB_IMAGE AS stdlib

WORKDIR /app
COPY --from=js-deps /js-deps/node_modules/ /app/node_modules/
COPY package.json \
     /app/

# unfortunately we need to npm install here
# because react-scripts requires webpack 4
# but we've got webpack 5 installed
RUN npm install

COPY public/ /app/public/

FROM $NODE_IMAGE

COPY --from=stdlib /stdlib /stdlib
WORKDIR /stdlib
RUN npm link

COPY --from=stdlib /app /app
WORKDIR /app
RUN npm link @reach-sh/stdlib

ENTRYPOINT ["npm", "run", "--"]
CMD ["start"]

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
