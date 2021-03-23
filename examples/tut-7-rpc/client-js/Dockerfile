FROM node:12.18.3

WORKDIR /app

COPY package.json /app/package.json

# The following COPY and RUN are unnecessary for most frontend authors but are
# included here for Reach core development purposes
COPY ./lib /lib
RUN npm install --loglevel=verbose /lib

RUN npm install --loglevel=verbose

COPY . /app

ENTRYPOINT ["npm", "run", "--"]
CMD ["index"]
