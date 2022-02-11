# `reachsh/reach-cli`

## Subcommand notes

### `version-compare` + `update-ide` for [the IDE extension](../../vsce)

```bash
$ ./reach version-compare --json; echo $?
{ "noDiff":  false
, "script":  true
, "dockerH": "/home/matt/.config/reach/_docker/vc-2022-02-02T22:50:17.103735041Z.json"
, "dockerC": "/app/config/_docker/vc-2022-02-02T22:50:17.103735041Z.json"
}
60

$ ./reach update-ide \
  --script \
  --rm-json \
  --json=/app/config/_docker/vc-2022-02-02T22:50:17.103735041Z.json

Backed up ./reach to /home/matt/.config/reach/_backup/reach-2022-02-02T22:51:06.225758021Z.
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  2366  100  2366    0     0  12652      0 --:--:-- --:--:-- --:--:-- 12652

Replaced ./reach with latest version.
```

Exit code `60` from `reach version-compare --json` means either the script or the Docker bits are out of date (or both).
If the IDE receives exit code `0` it can just skip `reach update-ide` since it means everything is already synchronized.

#### `reach version-compare --json` output:
- `noDiff` means the user doesn't have the `diff` program available on their `$PATH` so we couldn’t compare their script to the latest.
- `script` will say `false` if it’s already up to date; `true` if not.
- `dockerH` is a path to the JSON on the user’s `H`ost machine.
  It contains the following fields:
  - `newConnector` records are optional (e.g. `reachsh/devnet-cfx` + `reachsh/devnet-eth` if they only care about `reachsh/devnet-algo`).
  - `newDigest` and `newTag` records represent things that are out of date; these require `reach update-ide`.
  - `synced` records mean the image is present and all its associated tags are in order as well.
- `dockerC` is the path to the JSON that a Docker `C`ontainer will understand in the next step.

#### `reach update-ide ...` args:
- `--script` should only be used when `"script": true` above.
- `--rm-json` (optionally) tidies up after itself since the JSON file isn’t used for anything else.
  (Ideally we wouldn't use an intermediate file here anyway, but this is necessary due to CORE-1000.)
- `--json=…` should be given the `dockerC` path from above.
