FROM node:17-alpine3.13 as dev
WORKDIR /proj

COPY package.json .
RUN npm install
COPY index.mjs index.json ./

ENTRYPOINT node index.mjs
