ARG REACH_VERSION
ARG STDLIB_IMAGE=reachsh/stdlib:$REACH_VERSION

# hadolint ignore=DL3006
FROM $STDLIB_IMAGE

WORKDIR /app
COPY package.json /app/
RUN npm link @reach-sh/stdlib

ENTRYPOINT ["npm", "run", "--"]
CMD ["start"]

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
