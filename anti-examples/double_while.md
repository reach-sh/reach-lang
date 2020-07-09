```
$ reachc anti-examples/double_while.rsh

...

+ stack exec -- reachc -o .reachc-out anti-examples/double_while.rsh
Getting project config file from STACK_YAML environment
Verifying with honest = True; role = RoleContract
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(3,
...checking VC_WhileTail_AssumeUntil (IL_Ret
...checking VC_WhileBody_AssumeNotUntil [(9,
...checking VC_WhileTail_AssumeUntil (IL_Ret
Verifying with honest = True; role = RolePart (0,("A",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(3,
...checking VC_WhileTail_AssumeUntil (IL_Ret
...checking VC_WhileBody_AssumeNotUntil [(9,
...checking VC_WhileTail_AssumeUntil (IL_Ret
Verifying with honest = False; role = RoleContract
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(3,
...checking VC_WhileTail_AssumeUntil (IL_Ret
...checking VC_WhileBody_AssumeNotUntil [(9,
...checking VC_WhileTail_AssumeUntil (IL_Ret
Verifying with honest = False; role = RolePart (0,("A",LT_BT BT_Address))
...checking VC_Top
...checking VC_WhileBody_AssumeNotUntil [(3,
...checking VC_WhileTail_AssumeUntil (IL_Ret
...checking VC_WhileBody_AssumeNotUntil [(9,
...checking VC_WhileTail_AssumeUntil (IL_Ret
Checked 30 theorems; No failures!
goal clerk compile failed:
STDOUT:

STDERR:
-: :322 label l2 is before reference but only forward jumps are allowed
```
