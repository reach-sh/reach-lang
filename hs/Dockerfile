# --- Build
FROM alpine-ghc-stack:2.1.rc as build
WORKDIR /build

COPY stack.yaml package.yaml stack.yaml.lock ./
RUN stack build --dependencies-only

COPY . /build/
RUN stack build
RUN mv "$(stack path --local-install-root)/bin" /build/bin/

# --- Deploy
FROM ethereum/solc:0.8.5 as solc

FROM alpine 

RUN apk update && apk add libgmpxx ca-certificates git z3 z3-dev

#COPY --from=dl-deps /z3 /z3
#RUN ln -s /z3/z3 /usr/bin/z3
COPY --from=solc /usr/bin/solc /usr/bin/solc
COPY --from=build /build/bin/reachc /usr/bin/reachc

WORKDIR /app
ENTRYPOINT ["/usr/bin/reachc"]
ARG REACHC_HASH
ENV REACHC_HASH="${REACHC_HASH}"
ENV REACH_GIT_HASH="${REACHC_HASH}"
