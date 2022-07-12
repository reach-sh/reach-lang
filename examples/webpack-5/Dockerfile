FROM reachsh/runner:latest

RUN apk --no-cache add make

COPY Makefile package.json package-lock.json /app/

RUN make js-install

COPY \
  index.html \
  main.js \
  webpack.config.js \
  /app/

RUN make js-build

ENTRYPOINT ["make"]
CMD []
