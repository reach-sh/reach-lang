WARNING: this docker container is busy, even when you do not interact with it.
Be sure to kill it if you're not using it.

TODO: figure out how to run a devnet
without 100% cpu usage when you're not using it.

config.json came from `~/.algorand/config.json.example`
(present on the image)

The only config option changed was:

```
    "EndpointAddress": "0.0.0.0:4180",
```

This was so that port forwarding would work with docker.
See: https://stackoverflow.com/questions/39525820/docker-port-forwarding-not-working

4180 was chosen per the defaults for AlgodClient:
https://github.com/algorand/js-algorand-sdk/blob/dcec38cc7926de7f54328ce28e76290ffea9fe41/src/client/v2/algod/algod.js#L20
