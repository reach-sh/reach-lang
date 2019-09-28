FROM node:12.9.1

WORKDIR /stdlib
COPY package.json /stdlib
RUN npm install
RUN npm link

COPY . /stdlib

CMD npm run test
