FROM reachsh/stdlib:0.1

WORKDIR /app

# If your project needs more node dependencies:
RUN npm link '@reach-sh/stdlib'
RUN cp /stdlib/package-lock.json /app/package-lock.json
RUN cp -r /stdlib/node_modules /app/node_modules
COPY package.json /app/package.json
RUN npm install --loglevel verbose

COPY . /app

CMD ["index"]
