ARG NODE_VERSION
ARG NODE_IMAGE=node:$NODE_VERSION
FROM $NODE_IMAGE

WORKDIR /app

COPY package.json /app/package.json
COPY ./lib /lib
RUN npm install --loglevel=verbose /lib
RUN npm install --loglevel=verbose

COPY index.mjs /app

ENV FORCE_COLOR 1
ENTRYPOINT ["npm", "run", "--"]
CMD ["index"]
