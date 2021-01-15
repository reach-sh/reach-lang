FROM node:12.8

WORKDIR /app

COPY package.json /app/package.json
RUN npm install --loglevel=verbose
RUN npm install -g eslint@7.7.0

COPY . /app

ENTRYPOINT ["npm", "run", "--"]
CMD ["index"]
