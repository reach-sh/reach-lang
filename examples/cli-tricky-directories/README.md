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

##### **`DS`**
This file lists \<expected normalized name\>:\<space-sensitive subdirectory\>.

The normalized name in the first column should match how we automatically label the project's `package.json` file and containers spawned by `reach run`.

The `$TARGET` (and any subdirectories) in the second column will be `mkdir -p`-ed on the fly.
Any character after the first `:` is used verbatim (including leading-spaces).

##### **`$TARGET/.../index.{mjs|rsh}`**
These will be generated automatically by the test suite and shouldn't be committed to source control.
`run.sh` deletes the test directory after `go.sh` has been applied to `$TARGET`.

##### **`.tmp/$TARGET.log`**
Captures output from running `go.sh $TARGET`.
In the case of a successful run this file's contents won't be shown, but if any stage of `go.sh $TARGET` fails it'll be dumped to `stdout`.

##### **`.tmp/build/$TARGET/`**
Tests the CLI's ability to channel compilation output to another directory.

##### **`.tmp/EXIT`**
If any `$TARGET` should fail its tests this file will be updated to contain `1`; otherwise it'll contain a default of `0`.
The test suite terminates with `exit "$(cat ".tmp/EXIT")"` to ensure propagating a true go/no-go status.

## Note
Executing `make run` twice will fail due to a quirk with Docker volumes and the fact that `run.sh` deletes test directories after invoking `go.sh`.
Use `reach down` between invocations of `make run` if you need to make this work.
