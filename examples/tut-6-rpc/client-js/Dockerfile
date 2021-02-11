FROM node:12.8

WORKDIR /app

COPY package.json /app/package.json

# TODO ditch following COPY and RUN once `@reach-sh/rpc-client`
#      package is published to NPM registry
# hadolint ignore=DL3010
COPY ./reach-sh-rpc-client-0.0.1.tgz /app
RUN npm install --loglevel=verbose ./reach-sh-rpc-client-0.0.1.tgz

RUN npm install --loglevel=verbose

COPY . /app

ENTRYPOINT ["npm", "run", "--"]
CMD ["index"]
