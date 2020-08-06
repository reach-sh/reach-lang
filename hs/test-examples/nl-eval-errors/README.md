XXX Known missing srclocs on err

* Err_Block_Assign
* Err_App_InvalidPartSpec

XXX Known weird srclocs on err

* Err_Block_NotNull

XXX Figure out how to trigger

* Err_Parser_Arrow_NoFormals -- (=> e) didn't work
* Err_Parse_IllegalLiteral -- undefined didn't work
* Err_Parse_NotModule
* Err_CannotReturn -- most attempts were not valid js
* Err_Eval_IllegalLift
* Err_Eval_NoReturn -- not syntactically possible?
* Err_Obj_IllegalFieldValues -- not possible with Grammar7?
  | Err_Obj_SpreadNotObj SLVal
  | Err_Prim_InvalidArgs SLPrimitive [SLVal]
  | Err_Shadowed SLVar
  | Err_TailEmpty
  | Err_TailNotEmpty [JSStatement]
  | Err_ToConsensus_Double ToConsensusMode
  | Err_TopFun_NoName
  | Err_Top_IllegalJS JSStatement
  | Err_Top_NotDApp SLVal
  | Err_While_IllegalInvariant [JSExpression]
