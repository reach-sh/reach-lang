ARG        RUNNER_VERSION

# hadolint ignore=DL3006
FROM       $RUNNER_VERSION

COPY       package.json index.mjs /app/
ENTRYPOINT ["npm", "run", "--"]
CMD        ["app"]
