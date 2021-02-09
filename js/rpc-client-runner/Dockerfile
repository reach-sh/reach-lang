ARG        RPC_CLIENT_RUNNER_VERSION

# hadolint ignore=DL3006
FROM       $RPC_CLIENT_RUNNER_VERSION

COPY       package.json /app

ENTRYPOINT ["npm", "run", "--"]
CMD        ["app"]
