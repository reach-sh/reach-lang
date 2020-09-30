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

RUN rm -fr /stdlib/node_modules/algosdk

COPY . /stdlib
RUN mv js-algorand-sdk /stdlib/node_modules/algosdk

RUN npm run tsc
RUN ./sbin/fix.sh
RUN rm ./sbin/fix.sh

ENV REACH_RUNNER_MOUNT_DIR /app
ENTRYPOINT ["npm", "run", "--"]
CMD ["runner", "FAKE", "index.mjs"]

ARG REACH_GIT_HASH
ENV REACH_GIT_HASH="${REACH_GIT_HASH}"
