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
  | Err_Eval_NotObject SLVal
  | Err_Eval_RefEmptyArray
  | Err_Eval_RefNotArray SLVal
  | Err_Eval_RefNotInt SLVal
  | Err_Eval_RefOutOfBounds Int Integer
  | Err_Eval_ReturnsDifferentTypes [SLType]
  | Err_Eval_UnboundId SLVar [SLVar]
  | Err_ExpectedPrivate SLVal
  | Err_ExpectedPublic SLVal
  | Err_Export_IllegalJS JSExportDeclaration
  | Err_Form_InvalidArgs SLForm [JSExpression]
  | Err_Fun_NamesIllegal
  | Err_Import_IllegalJS JSImportDeclaration
  | Err_Import_ShadowedImport SLVar
  | Err_Module_Return (SLRes SLStmtRes)
  | Err_NoHeader [JSModuleItem]
  | Err_Obj_IllegalComputedField SLVal
  | Err_Obj_IllegalField JSPropertyName
  | Err_Obj_IllegalFieldValues [JSExpression]
  | Err_Obj_IllegalJS JSObjectProperty
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
