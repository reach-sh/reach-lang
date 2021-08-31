FROM reachsh/runner:${REACH_VERSION}

# If your project needs more node dependencies:
# COPY package.json /app/package.json
# RUN npm install
# RUN npm link @reach-sh/stdlib

COPY . /app
CMD ["${APP}"]
