# TODO: pull version from DEPS
FROM cypress/base:14.17.0

WORKDIR /test
COPY package.json yarn.lock ./
RUN yarn install --frozen-lockfile

COPY cypress.json ./
# COPY cypress/ ./cypress/

ENV CYPRESS_BASE_URL="http://host.docker.internal:3000"
ENTRYPOINT ["yarn", "run", "cypress", "run"]
CMD []
