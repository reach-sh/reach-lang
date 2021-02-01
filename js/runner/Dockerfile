ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

ARG REACH_VERSION
ARG JS_DEPS_IMAGE=reachsh/js_deps:$REACH_VERSION
ARG STDLIB_IMAGE=reachsh/stdlib2:$REACH_VERSION
# XXX ^ stdlib2 -> stdlib

# hadolint ignore=DL3006
FROM $JS_DEPS_IMAGE as js_deps

# hadolint ignore=DL3006
FROM $NODE_IMAGE as app0

WORKDIR /app
COPY --from=js_deps /js_deps/node_modules/ /app/node_modules/
COPY package.json package-lock.json \
     /app/

# hadolint ignore=DL3006
FROM $STDLIB_IMAGE as stdlib

FROM app0

# TODO: make this less flimsy
COPY --from=stdlib /stdlib/*.mjs /stdlib/package.json \
     /app/node_modules/@reach-sh/stdlib/

ENTRYPOINT ["npm", "run", "--"]
CMD ["start"]

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
