ARG REACH_VERSION
ARG NODE_VERSION
ARG STDLIB_IMAGE=reachsh/stdlib:$REACH_VERSION

# hadolint ignore=DL3006
FROM $STDLIB_IMAGE AS stdlib

FROM node:$NODE_VERSION as stage
RUN apk update && apk add git make python3 libc-dev gcc g++

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
