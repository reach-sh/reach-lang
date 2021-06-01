FROM reachsh/runner:0.1

COPY Makefile package.json /app/

RUN make js-install

COPY \
  index.html \
  main.js \
  webpack.config.js \
  /app/

RUN make js-build

ENTRYPOINT ["make"]
CMD []
