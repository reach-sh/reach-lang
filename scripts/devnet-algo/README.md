# How to update to a new version of Algorand

## Update Dockerfile

## Update the config.json

```
wget https://raw.githubusercontent.com/algorand/go-algorand/master/installer/config.json.example
mv config.json.example algorand_data/config.json
git diff algorand_data
# review changes and preserve our modification
```

## Regenerate network

```
make generate-network
```

Add and remove sqlite wallet

## Update rawFaucetDefaultMnemonic in /js/stdlib/ts/ALGO.ts

