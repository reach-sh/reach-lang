FROM node:12.9.1

WORKDIR /stdlib
COPY package.json /stdlib
RUN npm install
RUN npm link

COPY . /stdlib

ENV REACH_RUNNER_MOUNT_DIR /app
ENTRYPOINT ["npm", "run", "runner"]
CMD ["FAKE", "index.mjs"]
