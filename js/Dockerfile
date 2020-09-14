ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION

# hadolint ignore=DL3006
FROM $NODE_IMAGE

WORKDIR /stdlib
ARG ESLINT_VERSION
COPY package.json package-lock.json /stdlib/
RUN npm install
RUN npm install -g eslint@$ESLINT_VERSION
RUN npm link

COPY . /stdlib

ENV REACH_RUNNER_MOUNT_DIR /app
ENTRYPOINT ["npm", "run", "--"]
CMD ["runner", "FAKE", "index.mjs"]
