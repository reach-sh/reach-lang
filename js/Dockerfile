FROM node:12.18.3

WORKDIR /stdlib
COPY package.json /stdlib
RUN npm install
RUN npm link

COPY . /stdlib

ENV REACH_RUNNER_MOUNT_DIR /app
ENTRYPOINT ["npm", "run", "--"]
CMD ["runner", "FAKE", "index.mjs"]
