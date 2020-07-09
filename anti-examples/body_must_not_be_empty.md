```
reachc: body_must_not_be_empty.rsh: body must not be empty (given: [])
```

This one has no src loc.
It apparently means that `commit` needs a tail. Appending `return 0;` to main fixes it.

```
$ reachc body_must_not_be_empty.rsh

...

+ stack exec -- reachc -o .reachc-out body_must_not_be_empty.rsh
Getting project config file from STACK_YAML environment
reachc: body_must_not_be_empty.rsh: body must not be empty (given: [])
CallStack (from HasCallStack):
  error, called at src/Reach/ParserInternal.hs:255:24 in reach-0.1.0-GXfBqqypx6e4heaYMMTc2J:Reach.ParserInternal
```
