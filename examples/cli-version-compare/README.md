# `cli-version-compare`

This test suite pairs flat JSON stub inputs with the hidden [`version-compare2` CLI subcommand](../../hs/app/reach/Main.hs).

## `run.sh` target conventions

For a given `$TARGET` directory in this test suite (e.g. `unknown-tag:0.1.7`), the following conventions apply:

##### **`$TARGET/l.json`**
This file is effectively the same as the temporary `$XDG_CONFIG_HOME/reach/_docker/ils-$ZULU.json` output produced by `version-compare` in the "first step" of checking for updates.
The stubs you see here were just copied from `$XDG_CONFIG_HOME/reach/_docker` and lightly edited by hand to match the desired test parameters.
These files are called `l.json` because they're generated from `l`ocally-available Docker artifacts.

##### **`$TARGET/r.json`**
`r.json` represents the `r`emote data obtained from a cloud registry (for now, just [DockerHub](https://hub.docker.com/u/reachsh)).
In the normal case the CLI doesn't serialize this stage of the data to disk when a user runs the updater, but it's easy to produce new stubs by temporarily inserting
```haskell
assocR <- remoteDockerAssocFor imagesAll
liftIO $ encodeFile "/tmp/assocR" assocR -- 👈 a line such as this
```
in `Main.hs` then copying `/tmp/assocR` to `$NEWTARGET/r.json`.
The `r-template.json` file was obtained this way.

Copying `r-template.json` to `$NEWTARGET/r.json` and removing entries to trigger the desired test case behavior is another convenient way of generating an `r.json`.

##### **`$TARGET/o.txt`**
This file is the `o`utput one expects to see when evaluating `reach version-compare2` against `$TARGET`'s stubs.
`run.sh` transparently supplies stub arguments and will report an error if `o.txt` `diff`ers from the real output.

##### **`$TARGET/REACH_VERSION`** (optional)
`$TARGET/REACH_VERSION` can be used to override the CLI's `REACH_VERSION` environment variable-defaulting behavior.

##### **`$TARGET/reach`** (optional)
`$TARGET/reach` can be used to exercise the CLI's script-checking behavior.
