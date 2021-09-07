ARG REACH_VERSION
ARG STDLIB_IMAGE=reachsh/stdlib:$REACH_VERSION

# hadolint ignore=DL3006
FROM $STDLIB_IMAGE AS stdlib

FROM node:14.17.0-alpine3.13 as stage
COPY --from=stdlib /stdlib /stdlib

WORKDIR /stdlib
RUN npm link

WORKDIR /app
COPY package.json /app/
RUN npm link @reach-sh/stdlib

ENTRYPOINT ["npm", "run", "--"]
CMD ["start"]

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
