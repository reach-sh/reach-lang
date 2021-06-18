FROM reachsh/runner:${REACH_VERSION}

# If your project needs more node dependencies:
# COPY ${PACKAGE_JSON} /app/package.json
# RUN npm install
# RUN npm link @reach-sh/stdlib

COPY . /app
${CPLINE}
CMD ["${APP}"]
