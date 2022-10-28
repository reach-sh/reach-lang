module Reach.EditorInfo (printBaseKeywordInfo) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Generics.Deriving (Generic, conNameOf)
import Reach.AST.SL

-- https://hackage.haskell.org/package/aeson-pretty-0.8.9/docs/
customConfig :: A.Config
customConfig = A.Config {
  confIndent = A.Spaces 2,
  confCompare = compare,
  confNumFormat = A.Generic,
  confTrailingNewline = True
}

printBaseKeywordInfo :: (M.Map String SLVal) -> IO ()
printBaseKeywordInfo env = do
  let baseKinds = M.mapMaybe completionKind env
  let filtered = M.filterWithKey (\k _ -> not $ L.isInfixOf "_" k) baseKinds
  B.putStr $
    A.encodePretty' customConfig $ A.toJSON $
       M.map
         (\v -> M.singleton ("CompletionItemKind" :: String) $ show v)
         filtered

data CompletionItemKind
  = CK_Text
  | CK_Method
  | CK_Function
  | CK_Constructor
  | CK_Field
  | CK_Variable
  | CK_Class
  | CK_Interface
  | CK_Module
  | CK_Property
  | CK_Unit
  | CK_Value
  | CK_Enum
  | CK_Keyword
  | CK_Snippet
  | CK_Color
  | CK_File
  | CK_Reference
  | CK_Folder
  | CK_EnumMember
  | CK_Constant
  | CK_Struct
  | CK_Event
  | CK_Operator
  | CK_TypeParameter
  deriving (Generic)

instance Show CompletionItemKind where
  show k = drop 3 $ conNameOf k

completionKind :: SLVal -> Maybe CompletionItemKind
completionKind v =
  case v of
    SLV_Null _ _ -> Just CK_Constant
    SLV_Bool _ _ -> Just CK_Constant
    SLV_Int _ _ _ -> Just CK_Constant
    SLV_Bytes _ _ -> Just CK_Constant
    SLV_BytesDyn _ _ -> Just CK_Constant
    SLV_String _ _ -> Just CK_Constant
    SLV_Array _ _ _ -> Just CK_Constant
    SLV_Tuple _ _ -> Just CK_Constant
    SLV_Object _ _ _ -> Just CK_Constant
    SLV_Struct _ _ -> Just CK_Constant
    SLV_Clo _ _ _ -> Just CK_Function
    SLV_Data _ _ _ _ -> Just CK_Constant
    SLV_DLC _ -> Just CK_Constant
    SLV_Connector _ -> Just CK_Constant
    SLV_RaceParticipant _ _ -> Just CK_Constant
    SLV_Participant _ _ _ _ -> Just CK_Constant
    SLV_Map _ -> Just CK_Variable
    SLV_Deprecated _ _ -> Nothing
    SLV_ContractCode {} -> Just CK_Constant
    SLV_Type _ -> Just CK_TypeParameter
    SLV_Kwd _ -> Just CK_Keyword
    SLV_DLVar _ -> Just CK_Variable
    SLV_Anybody -> Just CK_Keyword
    SLV_Prim slp ->
      case slp of
        SLPrim_makeEnum -> Just CK_Function
        SLPrim_declassify -> Just CK_Function
        SLPrim_digest -> Just CK_Function
        SLPrim_commit -> Just CK_Function
        SLPrim_committed -> Nothing
        SLPrim_claim _ -> Just CK_Function
        SLPrim_localf _ _ _ _ -> Nothing
        SLPrim_is_type -> Just CK_Function
        SLPrim_type_eq -> Just CK_Function
        SLPrim_typeOf -> Just CK_Function
        SLPrim_Fun -> Just CK_TypeParameter
        SLPrim_Refine -> Just CK_TypeParameter
        SLPrim_Bytes -> Just CK_TypeParameter
        SLPrim_BytesDynCast -> Just CK_Function
        SLPrim_Data -> Just CK_TypeParameter
        SLPrim_Data_variant _ _ _ _ -> Just CK_Function
        SLPrim_data_match -> Just CK_Method
        SLPrim_Array -> Just CK_TypeParameter
        SLPrim_Array_iota -> Just CK_Method
        SLPrim_array -> Just CK_Function
        SLPrim_array_elemType -> Just CK_Method
        SLPrim_array_length -> Just CK_Method
        SLPrim_array_set -> Just CK_Method
        SLPrim_array_concat -> Just CK_Method
        SLPrim_array_map _ -> Just CK_Method
        SLPrim_array_reduce _ -> Just CK_Method
        SLPrim_array_zip -> Just CK_Method
        SLPrim_Struct -> Just CK_TypeParameter
        SLPrim_Struct_fromTuple _ -> Just CK_Method
        SLPrim_Struct_fromObject _ -> Just CK_Method
        SLPrim_Struct_toTuple -> Just CK_Method
        SLPrim_Struct_toObject -> Just CK_Method
        SLPrim_Struct_fields -> Just CK_Method
        SLPrim_Tuple -> Just CK_TypeParameter
        SLPrim_tuple_includes -> Just CK_Method
        SLPrim_tuple_length -> Just CK_Method
        SLPrim_tuple_set -> Just CK_Method
        SLPrim_Object -> Just CK_TypeParameter
        SLPrim_Object_has -> Just CK_Method
        SLPrim_Object_fields -> Just CK_Method
        SLPrim_Object_set -> Just CK_Method
        SLPrim_App_Delay {} -> Nothing
        SLPrim_op op -> case op of
          S_MUL_DIV _ -> Just CK_Function
          _ -> Just CK_Operator
        SLPrim_transfer -> Just CK_Function
        SLPrim_transfer_amt_to _ -> Nothing
        SLPrim_exit -> Just CK_Function
        SLPrim_exitted -> Nothing
        SLPrim_forall -> Just CK_Function
        SLPrim_PrimDelay _ _ _ _ -> Nothing
        SLPrim_part_set -> Just CK_Method
        SLPrim_part_setted _ _ _ -> Nothing
        SLPrim_fluid_read _ -> Just CK_Function
        SLPrim_fluid_read_didPublish _ -> Just CK_Function
        SLPrim_fluid_read_canWait _ -> Just CK_Function
        SLPrim_race -> Just CK_Function
        SLPrim_Map -> Just CK_TypeParameter
        SLPrim_Map_new -> Just CK_Method
        SLPrim_Map_reduce -> Just CK_Function
        SLPrim_Participant -> Just CK_Constructor
        SLPrim_ParticipantClass -> Just CK_Constructor
        SLPrim_View -> Just CK_Constructor
        SLPrim_API -> Just CK_Constructor
        SLPrim_Foldable -> Just CK_TypeParameter
        SLPrim_is -> Just CK_Function
        SLPrim_remote -> Just CK_Function
        SLPrim_remotef {} -> Nothing
        SLPrim_balance -> Just CK_Function
        SLPrim_Token_accepted -> Just CK_Function
        SLPrim_Token_supply -> Just CK_Method
        SLPrim_viewis _ _ _ _ -> Nothing
        SLPrim_init -> Just CK_Function
        SLPrim_inited -> Nothing
        SLPrim_setOptions -> Just CK_Function
        SLPrim_adaptReachAppTupleArgs -> Nothing
        SLPrim_padTo _ -> Just CK_Method
        SLPrim_Token_new -> Just CK_Method
        SLPrim_Token_burn -> Just CK_Method
        SLPrim_Token_destroy -> Just CK_Method
        SLPrim_Token_destroyed -> Just CK_Method
        SLPrim_Token_track -> Just CK_Method
        SLPrim_didPublish -> Just CK_Function
        SLPrim_unstrict -> Just CK_Function
        SLPrim_polyNeq -> Just CK_Function
        SLPrim_getCompanion -> Just CK_Function
        SLPrim_getContract -> Just CK_Function
        SLPrim_getAddress -> Just CK_Function
        SLPrim_EmitLog -> Nothing
        SLPrim_Event -> Just CK_Constructor
        SLPrim_event_is _ _ _ -> Just CK_Method
        SLPrim_verifyMuldiv -> Just CK_Function
        SLPrim_getUntrackedFunds -> Just CK_Function
        SLPrim_isDataVariant -> Just CK_Function
        SLPrim_fromSome -> Just CK_Function
        SLPrim_currentMode -> Just CK_Function
        SLPrim_distinct -> Just CK_Function
        SLPrim_xor -> Just CK_Function
        SLPrim_mod -> Just CK_Function
        SLPrim_castOrTrunc _ -> Just CK_Function
        SLPrim_ContractCode -> Just CK_Constructor
        SLPrim_Contract_new -> Just CK_Function
        SLPrim_Contract_new_ctor {} -> Just CK_Function
        SLPrim_toStringDyn -> Just CK_Function
        SLPrim_Bytes_fromHex -> Just CK_Method
        SLPrim_Contract_fromAddress -> Just CK_Method
    SLV_Form slf ->
      case slf of
        SLForm_App -> Just CK_Constructor
        SLForm_each -> Just CK_Method
        SLForm_EachAns _ _ _ _ -> Nothing
        SLForm_Part_Only _ _ -> Just CK_Method
        SLForm_liftInteract _ _ _ -> Nothing
        SLForm_Part_ToConsensus _ -> Nothing
        SLForm_unknowable -> Just CK_Function
        SLForm_fork -> Just CK_Function
        SLForm_fork_partial _ -> Nothing
        SLForm_parallel_reduce -> Just CK_Function
        SLForm_parallel_reduce_partial _ -> Nothing
        SLForm_apiCall -> Just CK_Function
        SLForm_apiCall_partial _ -> Nothing
        SLForm_wait -> Just CK_Function
        SLForm_setApiDetails -> Nothing
