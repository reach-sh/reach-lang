ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

ARG REACH_VERSION
ARG JS_DEPS_IMAGE=reachsh/js-deps:$REACH_VERSION
ARG STDLIB_IMAGE=reachsh/stdlib:$REACH_VERSION

# hadolint ignore=DL3006
FROM $JS_DEPS_IMAGE as js-deps

# hadolint ignore=DL3006
FROM $NODE_IMAGE as app0

WORKDIR /app
COPY --from=js-deps /js-deps/node_modules/ /app/node_modules/
COPY package.json package-lock.json \
     /app/
RUN npm install

# hadolint ignore=DL3006
FROM $STDLIB_IMAGE as stdlib

FROM app0

# TODO: make this less flimsy
COPY --from=stdlib /stdlib/*.mjs /stdlib/package.json \
     /app/node_modules/@reach-sh/stdlib/
RUN sed -i.bak '/ganache-core/d' '/app/node_modules/@reach-sh/stdlib/package.json' \
  && rm '/app/node_modules/@reach-sh/stdlib/package.json.bak'
RUN sed -i.bak "s/import('ganache-core')/null/" '/app/node_modules/@reach-sh/stdlib/ETH.mjs' \
  && rm '/app/node_modules/@reach-sh/stdlib/ETH.mjs.bak'

COPY public/ /app/public/
COPY craco.config.js /app/

ENTRYPOINT ["npm", "run", "--"]
CMD ["start"]

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
