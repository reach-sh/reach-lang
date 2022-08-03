# Documentation

```
$ make build
$ make serve-up
```

We use [semantic newlines](https://rhodesmill.org/brandon/2012/one-sentence-per-line/).

Did CI complain at you about the `docs-render`, saying something like this?

```
cmp kToD.ts "../vsce/server/src/keywordToDocumentation.ts"
kToD.ts ../vsce/server/src/keywordToDocumentation.ts differ: byte 157250, line 138
```

Do this:

```
make check-accept
```
