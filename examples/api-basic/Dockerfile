FROM reachsh/runner:latest

COPY package.json /app/package.json
RUN npm install
RUN npm link @reach-sh/stdlib

COPY . /app
CMD ["index"]
