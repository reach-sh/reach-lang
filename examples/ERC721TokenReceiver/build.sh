#!/bin/sh -e
mkdir -p build
[ -f "v4.7.3.tar.gz" ] || wget https://github.com/OpenZeppelin/openzeppelin-contracts/archive/refs/tags/v4.7.3.tar.gz
[ -d "openzeppelin-contracts-4.7.3" ] || tar xf v4.7.3.tar.gz
solc --pretty-json --optimize --combined-json abi,bin oz_erc721.sol > build/oz_erc721.json
solc --pretty-json --optimize --combined-json abi,bin oz_erc721_tokenreceiver.sol > build/oz_erc721_tokenreceiver.json
