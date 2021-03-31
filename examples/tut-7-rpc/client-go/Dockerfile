FROM    golang:1.15-alpine
WORKDIR /app
COPY    index.go /app/

# This example copies the `reachrpc` package directly into the image's
# filesystem in order to remain in-sync with the repository's client code, but
# frontend authors will normally skip this step and import from GitHub as
# explained in the `index.go` module
COPY    ./lib/ /go/src/reachrpc/

CMD     ["go", "run", "index.go"]
