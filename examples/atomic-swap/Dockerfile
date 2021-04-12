FROM reachsh/reach:latest as builder

ARG OPENZEP=4.0.0
WORKDIR /
RUN apt-get update && apt-get install -y wget
RUN wget https://github.com/OpenZeppelin/openzeppelin-contracts/archive/refs/tags/v${OPENZEP}.tar.gz
RUN tar xzvf v${OPENZEP}.tar.gz
WORKDIR openzeppelin-contracts-${OPENZEP}
RUN solc --optimize --combined-json abi,bin --allow-paths . contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol > /token.sol.json

FROM reachsh/runner:latest

# If your project needs more node dependencies:
COPY package.json /app/package.json
RUN npm install
RUN npm link @reach-sh/stdlib
COPY --from=builder /token.sol.json build/

COPY . /app

CMD ["index"]
