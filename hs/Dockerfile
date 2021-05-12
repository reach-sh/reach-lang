# --- Build
FROM reachsh/haskell-builder:lts-17.4 as build
WORKDIR /build

COPY stack.yaml package.yaml stack.yaml.lock ./
RUN stack build --dependencies-only

COPY . /build/
RUN stack build
RUN mv "$(stack path --local-install-root)/bin" /build/bin/

# --- Deploy
FROM ethereum/solc:0.8.2 as solc

FROM alpine:3.12 as dl-deps
RUN apk add wget unzip --no-cache
WORKDIR /z3
ENV Z3_VER 4.8.10
ENV Z3_SYS x64-ubuntu-18.04
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VER}/z3-${Z3_VER}-${Z3_SYS}.zip
RUN unzip z3-${Z3_VER}-${Z3_SYS}.zip
RUN rm z3-${Z3_VER}-${Z3_SYS}.zip
RUN ln -s /z3/z3-${Z3_VER}-${Z3_SYS}/bin/z3 /z3/z3

FROM ubuntu:18.04 as deps
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp10 \
    ca-certificates \
    git \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=dl-deps /z3 /z3
RUN ln -s /z3/z3 /usr/bin/z3
COPY --from=solc /usr/bin/solc /usr/bin/solc

FROM deps as app
COPY --from=build /build/bin/reachc /usr/bin/reachc
WORKDIR /app
ENTRYPOINT ["/usr/bin/reachc"]
ARG REACHC_HASH
ENV REACHC_HASH="${REACHC_HASH}"
ENV REACH_GIT_HASH="${REACHC_HASH}"
