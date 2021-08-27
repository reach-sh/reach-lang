ARG        RUNNER_VERSION

# hadolint ignore=DL3006
FROM       reachsh/runner:vmazzi
#$RUNNER_VERSION

COPY       package.json index.mjs /app/
ENTRYPOINT ["npm", "run", "--"]
CMD        ["app"]
EXPOSE     3000
