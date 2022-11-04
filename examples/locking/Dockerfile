FROM reachsh/reach:latest AS build

COPY index.sol .
RUN solc --optimize --combined-json abi,bin index.sol > /out

FROM reachsh/runner:latest

# If your project needs more node dependencies:
#COPY package.json /app/package.json
#RUN npm install
#RUN npm link @reach-sh/stdlib

COPY . /app
COPY --from=build /out /app/build/index.sol.json

CMD ["index"]
