ARG REACH_VERSION
ARG STDLIB_IMAGE=reachsh/stdlib:$REACH_VERSION
ARG JS_DEPS_IMAGE=reachsh/js-deps:$REACH_VERSION

# hadolint ignore=DL3006
FROM $JS_DEPS_IMAGE as js-deps

# hadolint ignore=DL3006
FROM $STDLIB_IMAGE

WORKDIR /app
COPY --from=js-deps /js-deps/node_modules/ /app/node_modules/
COPY package.json package-lock.json \
     /app/
RUN npm install
RUN npm link @reach-sh/stdlib

COPY public/ /app/public/
COPY craco.config.js /app/

ENTRYPOINT ["npm", "run", "--"]
CMD ["start"]

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
