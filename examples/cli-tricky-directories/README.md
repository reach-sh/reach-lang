# `cli-tricky-directories`

This test suite exists to stress the CLI's path-handling and project-name-defaulting behavior.

It'll look something like this:
```sh
$ make run
Running "A directory name with spaces "...         ✔ Succeeded
Running "Doesn't break"...                         ✔ Succeeded
Running "foo"...                                   ✔ Succeeded
Running "  foo _ bar @ baaz "...                   ✔ Succeeded
Running " that directory with "double quotes""...  ✔ Succeeded
Running "What' about 'this'?"...                   ✔ Succeeded
Running "«Інтернет»"...                            ✔ Succeeded
Running "世界您好"...                              ✔ Succeeded

```

## `run.sh` target conventions

For a given `$TARGET` directory in this test suite the following conventions apply:

##### **`$TARGET/.../CNAME`**
This file contains the `$TARGET`'s expected package.json `name` field (if auto-generated) and Docker container base-name (e.g. `reachsh/reach-app-the-target-name-1234`).
The test suite understands deeply-nested directories (hence `...`).

##### **`$TARGET/.../index.{mjs|rsh}`**
These will be generated automatically by the test suite and shouldn't be committed to source control.

##### **`.tmp/$TARGET.log`**
Captures output from running `go.sh $TARGET`.
In the case of a successful run this file's contents won't be shown, but if any stage of `go.sh $TARGET` fails it'll be dumped to `stdout`.

##### **`.tmp/build/$TARGET/`**
Tests the CLI's ability to channel compilation output to another directory.

##### **`.tmp/EXIT`**
If any `$TARGET` should fail its tests this file will be updated to contain `1`; otherwise it'll contain a default of `0`.
The test suite terminates with `exit "$(cat ".tmp/EXIT")"` to ensure propagating a true go/no-go status.
