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

Then look at the modified genesis.json [you want the new proto field] and config.json files and port the changes (to preserve the faucet address) [restore all the other files that got changed]

