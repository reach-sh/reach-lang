```
$ export REACHC_ENABLE_EXPERIMENTAL_CONNECTORS=true
$ reachc rps_double.rsh

...

+ stack exec -- reachc -o .reachc-out rps_double.rsh
Getting project config file from STACK_YAML environment
Verifying with honest = True; role = RoleContract
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Verifying with honest = True; role = RolePart (0,("A",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Verifying with honest = True; role = RolePart (3,("B",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Verifying with honest = True; role = RolePart (4,("O",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Verifying with honest = False; role = RoleContract
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Verifying with honest = False; role = RolePart (0,("A",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Verifying with honest = False; role = RolePart (3,("B",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Verifying with honest = False; role = RolePart (4,("O",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(15
...checking VC_WhileTail_AssumeUntil (IL_Let
...checking VC_WhileBody_AssumeNotUntil [(28
...checking VC_WhileTail_AssumeUntil (IL_Let
Checked 278 theorems; No failures!
reachc: Handler not defined --- 4 --- in fromList [(3,2)]
CallStack (from HasCallStack):
  error, called at src/Reach/EmitEVM.hs:163:7 in reach-0.1.0-GXfBqqypx6e4heaYMMTc2J:Reach.EmitEVM
```
