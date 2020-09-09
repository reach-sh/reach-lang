# WARNING

This docker container is busy, even when you do not interact with it.
Be sure to kill it if you're not using it.

TODO: figure out how to run a devnet
without 100% cpu usage when you're not using it.

# algorand_data 

This folder contains a few file overrides for the Primary node.

algorand_data/config.json came from `~/.algorand/config.json.example`
(present on the image)

The only config option changed was:

```
    "EndpointAddress": "0.0.0.0:4180",
```

This was so that port forwarding would work with docker.
See: https://stackoverflow.com/questions/39525820/docker-port-forwarding-not-working

4180 was chosen per the defaults for AlgodClient:
https://github.com/algorand/js-algorand-sdk/blob/dcec38cc7926de7f54328ce28e76290ffea9fe41/src/client/v2/algod/algod.js#L20

The other thing in algorand_data/ is fixed API tokens
which were generated at some point and can be reused
even if you regenerate the network.

# algorand_network

This folder is generated automatically by `make clean-network generate-network`

Try not to re-generate this too often,
because the FAUCET mnemonic needs to be updated in js/ALGO.mjs
each time you do.

Also note that `make generate-network` requires the image,
but `make build` (which builds the image) requires `make generate-network`.
You can close the loop by either:

* using a prebuilt image (this is what I, Dan, did.)
* `mkdir algorand_network` ;
  then `make build clean-network generate-network` ;
  then `make build` again.
* or, do the equivalent of `generate-network` with a local installation of goal

(The generated network stuff is committed to the repo,
so you shouldn't actually have to go through this bootstrapping process.)
