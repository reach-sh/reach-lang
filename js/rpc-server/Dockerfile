ARG        RUNNER_VERSION

# hadolint ignore=DL3006
FROM       $RUNNER_VERSION

COPY       ./package.json /app/
COPY       index.mjs /app/
ENTRYPOINT ["npm", "run", "--"]
CMD        ["app"]
EXPOSE     3000
