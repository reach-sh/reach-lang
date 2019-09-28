FROM reachsh/stdlib:v0.1.0

WORKDIR /app

COPY package.json /app
RUN npm link '@reach-sh/stdlib'
RUN npm install

COPY . /app

CMD npm run test && npm run demo
