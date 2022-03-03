ARG GOLANG_IMAGE
FROM $GOLANG_IMAGE
WORKDIR /app
COPY    index.go /app/

# This example copies the `reachrpc` package directly into the image's
# filesystem in order to remain in-sync with the repository's client code, but
# frontend authors will normally skip this step and import from GitHub as
# explained in the `index.go` module
COPY    ./lib/ /usr/local/go/src/reachrpc/

CMD     ["go", "run", "index.go"]
