Run `make react` to try it out.

This demo shows how to use the various wallets that Reach supports.
The code is intended to be read directly from start to finish.

FIXME (Dan) `make react` is not working quite right... but this works:

```
$ (cd ../../js && make build)
$ docker run -it --entrypoint=/bin/sh -v "${PWD}:/app/src" -p 3000:3000 reachsh/react-runner
/app # REACT_APP_REACH_CONNECTOR_MODE=ALGO npm run start
```
