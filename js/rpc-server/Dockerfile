ARG        RUNNER_VERSION

FROM alpine as mo
ARG REACH_VERSION
ENV REACH_VERSION=${REACH_VERSION}}

RUN apk update; apk add curl bash
COPY package.mo.json .

RUN	curl -sSL https://git.io/get-mo -o mo
RUN	chmod +x mo
RUN	mv mo /usr/local/bin

RUN mo /package.mo.json > package.json


# hadolint ignore=DL3006
FROM       $RUNNER_VERSION

COPY       --from=mo /package.json /app/
COPY       index.mjs /app/
ENTRYPOINT ["npm", "run", "--"]
CMD        ["app"]
EXPOSE     3000
