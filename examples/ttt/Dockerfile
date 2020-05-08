FROM reachsh/stdlib:v0.1.1

WORKDIR /app

COPY package.json /app
RUN npm link '@reach-sh/stdlib'
RUN npm install

COPY . /app

CMD ./wait-for-geth.sh && npm run test --- ETH && npm run test --- ALGO
