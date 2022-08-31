FROM reachsh/reach:latest AS build
COPY . /app
RUN /app/build.sh

FROM reachsh/runner:latest
RUN npm install -g npm@8.18.0
COPY --from=build /app /app
CMD ["index.mjs"]
