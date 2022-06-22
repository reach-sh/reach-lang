ARG        RUNNER_VERSION

# hadolint ignore=DL3006
FROM reachsh/runner:latest

RUN apk add make curl bash
COPY Makefile.docker ./Makefile
COPY .docker-root ./.docker-root
COPY package.mo.json .
RUN make ROOT=.docker-root package.json

COPY index.mjs /app/
ENTRYPOINT ["npm", "run", "--"]
CMD        ["app"]
EXPOSE     3000
