FROM reachsh/reach:latest AS build
COPY . /app
RUN /app/build.sh

FROM reachsh/runner:latest
COPY --from=build /app /app
CMD ["index.mjs"]
