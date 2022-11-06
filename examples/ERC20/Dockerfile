FROM reachsh/reach:latest AS build
COPY build.sh oz_erc20.sol /app/
RUN /app/build.sh

FROM reachsh/runner:latest
COPY --from=build /app/build /app/build
COPY . /app
CMD ["index.mjs"]
