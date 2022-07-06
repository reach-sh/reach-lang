# TODO: pull version from DEPS
ARG ALPINE_IMAGE
ARG CYPRESS_IMAGE
FROM $ALPINE_IMAGE
RUN apk add mariadb-connector-c-dev pcre-dev

FROM $CYPRESS_IMAGE

WORKDIR /test
COPY package.json yarn.lock ./
RUN yarn install --frozen-lockfile

COPY cypress.json ./
# COPY cypress/ ./cypress/

ARG CYPRESS_BASE_URL
ENV CYPRESS_BASE_URL="${CYPRESS_BASE_URL}"
ENTRYPOINT ["yarn", "run", "cypress", "run"]
CMD []
