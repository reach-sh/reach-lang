# --- Deps
FROM fpco/stack-build:lts-16.12 as build-deps
WORKDIR /build

# First build long-term stable dependencies
# COPY long-term/*.yaml{,.lock} /opt/build/  
# RUN stack build --system-ghc --dependencies-only

COPY stack.yaml package.yaml stack.yaml.lock /build/
RUN stack build --system-ghc --dependencies-only

# --- Build
FROM fpco/stack-build:lts-16.12 as build
WORKDIR /build
COPY --from=build-deps /root/.stack /root/.stack
COPY . /build/
RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /build/bin/

# --- Deploy
FROM ethereum/solc:0.7.0 as solc

FROM alpine:3 as dl-deps
RUN apk add wget=1.20.3-r1 unzip=6.0-r7 --no-cache
WORKDIR /z3
ENV Z3_VER 4.8.9
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VER}/z3-${Z3_VER}-x64-ubuntu-16.04.zip
RUN unzip z3-${Z3_VER}-x64-ubuntu-16.04.zip
RUN rm z3-${Z3_VER}-x64-ubuntu-16.04.zip
RUN ln -s /z3/z3-${Z3_VER}-x64-ubuntu-16.04/bin/z3 /z3/z3

FROM ubuntu:18.04 as deps
RUN apt-get update && apt-get install -y --no-install-recommends \
  libgmp10=2:6.1.2+dfsg-2 \
  ca-certificates=20190110~18.04.1 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=dl-deps /z3 /z3
RUN ln -s /z3/z3 /usr/bin/z3
COPY --from=solc /usr/bin/solc /usr/bin/solc

FROM deps as app
COPY --from=build /build/bin/reachc /usr/bin/reachc
WORKDIR /app
ENTRYPOINT ["/usr/bin/reachc"]
