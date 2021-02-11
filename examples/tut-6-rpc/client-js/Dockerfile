FROM node:12.8

WORKDIR /app

COPY package.json /app/package.json

# Note: the following COPY and RUN are unnecessary for end-users' programs but
# included here for Reach core development purposes
COPY ./rpc-client /rpc-client
RUN npm install --loglevel=verbose /rpc-client

RUN npm install --loglevel=verbose

COPY . /app

ENTRYPOINT ["npm", "run", "--"]
CMD ["index"]
