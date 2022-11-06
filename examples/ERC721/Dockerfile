FROM reachsh/reach:latest AS build
COPY oz_erc721.sol build.sh /app/
RUN /app/build.sh

FROM reachsh/runner:latest
COPY --from=build /app/build/oz_erc721.json /app/build/
COPY . /app
CMD ["index.mjs"]
