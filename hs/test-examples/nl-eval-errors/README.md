XXX Known missing srclocs on err

* Err_Block_Assign
* Err_Block_NotNull  -- regression? 2020-08-13
* Err_Eval_IfCondNotBool  -- regression? 2020-08-13
* Err_Eval_IllegalJS  -- regression? 2020-08-13

XXX Known weird srclocs on err

* Err_App_InvalidPartSpec -- srcloc is of the App(), not bad but could be closer

XXX Figure out how to trigger

* Err_Parser_Arrow_NoFormals -- (=> e) didn't work
* Err_Parse_IllegalLiteral -- undefined didn't work
* Err_Parse_NotModule
* Err_CannotReturn -- most attempts were not valid js
* Err_Decl_IllegalJS
* Err_Eval_IllegalLift
* Err_Eval_NoReturn -- not syntactically possible?
* Err_Obj_IllegalFieldValues -- not possible with Grammar7?
* Err_ToConsensus_Double -- prevented by earlier parsing?
* Err_TopFun_NoName -- hiding behind Err_Type_None
* Err_Eval_NotApplicableVals -- previous test example subsumed by Err_ToConsensus_TimeoutArgs
