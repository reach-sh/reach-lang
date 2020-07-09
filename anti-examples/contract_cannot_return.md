```
reachc: scratch.rsh:7:28: contract cannot: "return"
```

This gives a srcloc of the last line of `main`, which helps, but the meaning is unclear.
It seems to mean that you need to close the consensus step with commit().


```
$ reachc contract_cannot_return.md

...

+ stack exec -- reachc -o .reachc-out scratch.rsh
Getting project config file from STACK_YAML environment
Verifying with honest = True; role = RoleContract
...checking VC_Top
Verifying with honest = True; role = RolePart (0,("A",LT_BT BT_Address))
...checking VC_Top
Verifying with honest = False; role = RoleContract
...checking VC_Top
Verifying with honest = False; role = RolePart (0,("A",LT_BT BT_Address))
...checking VC_Top
Checked 10 theorems; No failures!
reachc: scratch.rsh:7:28: contract cannot: "return"
CallStack (from HasCallStack):
  error, called at src/Reach/Compiler.hs:66:23 in reach-0.1.0-GXfBqqypx6e4heaYMMTc2J:Reach.Compiler

```
