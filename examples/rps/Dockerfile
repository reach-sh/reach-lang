FROM reachsh/stdlib:v0.1.1

WORKDIR /app

COPY package.json /app
RUN npm link '@reach-sh/stdlib'
RUN npm install

COPY . /app

CMD npm run test --- FAKE && ETH_NODE_TYPE=in_memory_ganache npm run test --- ETH && npm run test --- ETH # && npm run test --- ALGO
