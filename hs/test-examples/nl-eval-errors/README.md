* XXX Figure out how to trigger `Err_Parser_Arrow_NoFormals`
* XXX Figure out how to trigger `Err_Parse_IllegalLiteral` -- undefined didn't work
* XXX Figure out how to trigger `Err_Parse_NotModule`

* XXX All this stuff
  = Err_Apply_ArgCount Int Int
  | Err_Block_Assign
  | Err_Block_Continue
  | Err_Block_IllegalJS JSStatement
  | Err_Block_NotNull SLVal
  | Err_Block_Variable
  | Err_Block_While
  | Err_CannotReturn
  | Err_DApp_InvalidInteract SLSVal
  | Err_DApp_InvalidPartSpec SLVal
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_IllegalJS JSExpression
  | Err_Decl_NotArray SLVal
  | Err_Decl_WrongArrayLength Int Int
  | Err_Dot_InvalidField SLVal String
  | Err_EvalRefIndirectNotHomogeneous [SLType]
  | Err_Eval_ContinueNotInWhile
  | Err_Eval_ContinueNotLoopVariable SLVar
  | Err_Eval_IfCondNotBool SLVal
  | Err_Eval_IfNotNull SLVal SLVal
  | Err_Eval_IllegalContext SLCtxtMode String
  | Err_Eval_IllegalJS JSExpression
  | Err_Eval_IllegalLift SLCtxtMode
  | Err_Eval_NoReturn
  | Err_Eval_NotApplicable SLVal
  | Err_Eval_NotApplicableVals SLVal
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
