FROM reachsh/react-runner:0.1

WORKDIR /app/
RUN rm package.json public/index.html

COPY Makefile package.json ./

RUN make js-install

RUN mkdir -p ./public
COPY \
  public/* \
  ./public/

RUN mkdir -p ./src
COPY \
  src/* \
  ./src/

RUN make js-build

ENTRYPOINT ["make"]
CMD []
