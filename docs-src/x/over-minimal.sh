#!/bin/sh -e

APP=over-minimal

DOCKERFILE=Dockerfile.${APP}
PACKAGE_JSON=package.json.${APP}

../../reachc -o build ${APP}.rsh

cat >${PACKAGE_JSON} <<EOF
{
  "name": "@reach-sh/${APP}",
  "type": "module",
  "dependencies": {
    "@reach-sh/stdlib": "0.1.1"
  },
  "author": "reach.sh",
  "license": "Apache-2.0",
  "scripts": {
    "app": "node --experimental-modules --unhandled-rejections=strict ${APP}.mjs"
  }
}
EOF

cat >${DOCKERFILE} <<EOF
FROM reachsh/stdlib:v0.1.1

WORKDIR /app

COPY package.json.${APP} /app/package.json
RUN npm link '@reach-sh/stdlib'
RUN npm install

COPY ${APP}.mjs /app
RUN mkdir /app/build
COPY build/${APP}.main.mjs /app/build

CMD npm run app
EOF

docker build -f ${DOCKERFILE} -t reachsh/reach-app-${APP}:latest .

rm -f package.json.${APP} ${DOCKERFILE}

cat<<EOF | docker-compose -f - run reach-app
version: '3'
services:
  reach-app:
    image: reachsh/reach-app-${APP}:latest
    depends_on:
      - devnet
    environment:
      - ETH_NODE_URI=http://devnet:8545
  devnet:
    image: reachsh/ethereum-devnet:v0.1.0
EOF
