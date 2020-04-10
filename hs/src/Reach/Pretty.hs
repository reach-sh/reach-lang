{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reach.Pretty where

import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc
import Data.List (intersperse)

import Reach.AST

--- Emiting Code ---

instance Pretty BaseType where
  pretty BT_UInt256 = pretty "uint256"
  pretty BT_Bool = pretty "bool"
  pretty BT_Bytes = pretty "bytes"
  pretty BT_Address = pretty "address"

instance Pretty LType where
  pretty (LT_BT bt) = pretty bt
  pretty (LT_FixedArray bt hm) = pretty bt <> pretty "[" <> viaShow hm <> pretty "]"

instance Pretty ExprType where
  pretty (TY_Con bt) = pretty bt
  pretty (TY_Var s) = viaShow s

instance Pretty Constant where
  pretty (Con_I i) = viaShow i
  pretty (Con_B b) = viaShow b
  pretty (Con_BS bs) = viaShow bs

instance Pretty C_Prim where
  pretty = viaShow

instance Pretty EP_Prim where
  pretty p = case p of
    CP cp -> pretty cp
    _ -> viaShow p

instance Pretty a => Pretty (Role a) where
  pretty (RolePart p) = pretty p
  pretty RoleContract = pretty "CTC"

instance Show a => Pretty (XLProgram a) where
  pretty = viaShow

instance Show a => Pretty (XILProgram a) where
  pretty = viaShow

instance Pretty (ILArg a) where
  pretty (IL_Var _ v) = prettyILVar v
  pretty (IL_Con _ c) = pretty c

prettyApp :: (Pretty p, Pretty a) => p -> [a] -> Doc ann
prettyApp p al = group $ parens $ pretty p <> alp
  where alp = case al of [] -> emptyDoc
                         _ -> space <> (hsep $ map pretty al)

prettyInteract :: Pretty a => String -> LType -> [a] -> Doc ann
prettyInteract m bt al = group $ parens $ pretty ("interact." ++ m) <+> pretty bt <+> (hsep $ map pretty al)

prettyClaim :: Pretty a => ClaimType -> a -> Doc ann
prettyClaim ct a = group $ parens $ pretty cts <+> pretty a
  where cts = case ct of
          CT_Assert -> "assert!"
          CT_Assume -> "assume!"
          CT_Require -> "require!"
          CT_Possible -> "possible?"

prettyTransfer :: Pretty a => Doc ann -> a -> Doc ann
prettyTransfer to a = group $ parens $ pretty "transfer!" <+> to <+> pretty a

prettyWhile :: Pretty b => Pretty c => (a -> Doc ann) -> [a] -> [b] -> c -> c -> c -> c -> Doc ann
prettyWhile prettyVar loopvs initas untilt invt bodyt kt = vsep [ group $ parens $ pretty "do" <+> brackets (vsep $ zipWith (\v i -> prettyVar v <+> pretty i) loopvs initas) <> nest 2 (hardline <> pretty "until" <+> prettyBegin untilt <> hardline <> pretty "invariant" <+> prettyBegin invt <> hardline <> prettyBegin bodyt), pretty kt ]

prettyBegin :: Pretty a => a -> Doc ann
prettyBegin x = group $ parens $ pretty "begin" <+> (nest 2 $ hardline <> pretty x)

instance Pretty (ILExpr a) where
  pretty (IL_PrimApp _ p al) = prettyApp p al
  pretty (IL_Interact _ m bt al) = prettyInteract m bt al
  pretty (IL_Declassify _ a) = group $ parens $ pretty "declassify" <+> pretty a
  pretty (IL_Digest _ al) = prettyApp "digest" al
  pretty (IL_ArrayRef _ ae ee) = prettyApp "vector-ref" [ae, ee]

instance Pretty (ILStmt a) where
  pretty (IL_Transfer _ to a) = prettyTransfer (prettyILVar to) a
  pretty (IL_Claim _ ct a) = prettyClaim ct a

prettyValues :: Pretty a => [a] -> Doc ann
prettyValues [ a ] = pretty a
prettyValues [] = group $ parens $ pretty "values"
prettyValues al = group $ parens $ (pretty "values") <+> (hsep $ map pretty al)

prettyIf :: (Pretty a, Pretty b) => a -> b -> b -> Doc ann
prettyIf ca tt ft = group $ parens $ pretty "cond" <+> (nest 2 $ hardline <> vsep [(group $ brackets $ (pretty ca) <+> pretty tt), (group $ brackets $ pretty "else" <+> pretty ft)])

prettyLet :: (Pretty xe, Pretty bt) => (x -> Doc ann) -> (Doc ann -> Doc ann) -> x -> xe -> bt -> Doc ann
prettyLet prettyVar at v e bt =
  vsep [(group $ at (ivp $ pretty e)), pretty bt]
  where ivp ep = parens $ pretty "define" <+> prettyVar v <> space <> ep

prettyDo :: (Pretty xe, Pretty bt) => (Doc ann -> Doc ann) -> xe -> bt -> Doc ann
prettyDo at e bt =
  vsep [(group $ at $ pretty e), pretty bt]

instance Pretty (ILTail a) where
  pretty (IL_Ret _ al) = prettyValues al
  pretty (IL_If _ ca tt ft) = prettyIf ca tt ft
  pretty (IL_Let _ r iv e bt) = prettyLet prettyILVar at iv e bt
    where at d = (group $ parens $ pretty "@" <+> pretty r <+> d)
  pretty (IL_Do _ r s bt) = prettyDo at s bt
    where at d = (group $ parens $ pretty "@" <+> pretty r <+> d)
  pretty (IL_ToConsensus _ (ok_ij, p, svs, pa) (mtwho, da, tt) ct) =
    vsep [(group $ parens $ pretty "@" <+> pretty ok_ij <+> prettyILVar p <+> (nest 2 $ hardline <> vsep [svsp, pap, tp])),
          pretty ct]
    where svsp = parens $ pretty "publish!" <+> prettyILVars svs
          pap = parens $ pretty "pay!" <+> pretty pa
          tp = parens $ pretty "timeout" <+> pretty mtwho <+> pretty da <+> (nest 2 $ hardline <> pretty tt)
  pretty (IL_FromConsensus _ lt) =
    vsep [(group $ parens $ pretty "commit!"),
          pretty lt]
  pretty (IL_While _ loopvs inita untilt invt bodyt kt) = prettyWhile prettyILVar loopvs inita untilt invt bodyt kt
  pretty (IL_Continue _ as) = parens $ pretty "continue!" <+> (hsep $ map pretty as)

prettyILVar :: ILVar -> Doc ann
prettyILVar (n, (s, et)) = pretty n <> pretty "/" <> pretty s <> pretty ":" <> pretty et

prettyILVars :: [ILVar] -> Doc ann
prettyILVars vs = parens $ hsep $ map prettyILVar vs

prettyILPartArg :: ILVar -> Doc ann
prettyILPartArg v = group $ brackets $ prettyILVar v

prettyILPart :: (ILPart, [ILVar]) -> Doc ann
prettyILPart (p, vs) =
  group $ parens $ pretty "define-participant" <+> pretty p <> body
  where pvs = map prettyILPartArg vs
        body = case vs of [] -> emptyDoc
                          _ -> (nest 2 $ hardline <> vsep pvs)

prettyILPartInfo :: ILPartInfo b -> Doc ann
prettyILPartInfo ps =
  vsep $ pretty "#:participants" : (map prettyILPart (M.toList ps))

instance Pretty (ILProgram a) where
  pretty (IL_Prog _ ps t) = vsep [pretty "#lang reach/il", emptyDoc, prettyILPartInfo ps, emptyDoc, pretty "#:main", pretty t]

instance Pretty (BLArg a) where
  pretty (BL_Con _ c) = pretty c
  pretty (BL_Var _ v) = prettyBLVar v

instance Pretty (EPExpr a) where
  pretty (EP_Arg _ a) = pretty a
  pretty (EP_Interact _ m bt al) = prettyInteract m bt al
  pretty (EP_PrimApp _ p al) = prettyApp p al
  pretty (EP_Digest _ al) = prettyApp "digest" al
  pretty (EP_ArrayRef _ ae ee) = prettyApp "vector-ref" [ae, ee]

instance Pretty (EPStmt a) where
  pretty (EP_Claim _ ct a) = prettyClaim ct a
  pretty (EP_Transfer _ to a) = prettyTransfer (prettyBLVar to) a

instance Pretty (CExpr a) where
  pretty (C_PrimApp _ p al) = prettyApp p al
  pretty (C_Digest _ al) = prettyApp "digest" al
  pretty (C_ArrayRef _ ae ee) = prettyApp "vector-ref" [ae, ee]

instance Pretty (CStmt a) where
  pretty (C_Claim _ ct a) = prettyClaim ct a
  pretty (C_Transfer _ to a) = prettyTransfer (prettyBLVar to) a

prettyTimeout :: (FromSpec, Int, BLArg a, EPTail a) -> Doc b
prettyTimeout (who_to, hi_to, delay, tt) =
  (nest 2 (hardline <> pretty "#:timeout" <+> pretty who_to <+> pretty hi_to <+> pretty delay <+> (nest 2 (hardline <> prettyBegin tt))))

instance Pretty (EPTail a) where
  pretty (EP_Ret _ al) = prettyValues al
  pretty (EP_If _ ca tt ft) = prettyIf ca tt ft
  pretty (EP_Let _ v e bt) = prettyLet prettyBLVar (\x->x) v e bt
  pretty (EP_Do _ s bt) = prettyDo (\x->x) s bt
  pretty (EP_SendRecv _ svs (fs_ok, hi_ok, vs, pa, bt) info_to) =
    vsep [group $ parens $ pretty "send!" <+> pretty fs_ok <+> pretty hi_ok <+> prettyBLVars svs <+> prettyBLVars vs <+> pretty pa <+> prettyTimeout info_to,
          pretty bt]
  pretty (EP_Recv _ svs (fs_ok, hi_ok, vs, bt) info_to) =
    vsep [group $ parens $ pretty "define-values" <+> prettyBLVars svs <+> prettyBLVars vs <+>
          (parens $ pretty "recv!" <+> pretty fs_ok <+> pretty hi_ok <+> prettyTimeout info_to),
          pretty bt]
  pretty (EP_Loop _ which loopvs initas bt) =
    group $ parens $ pretty "loop" <+> pretty which <+> (prettyVarArgs prettyBLVar loopvs initas) <> nest 2 (hardline <> prettyBegin bt)
  pretty (EP_Continue _ which vs args) = group $ parens $ pretty "continue" <+> pretty which <+> (prettyVarArgs prettyBLVar vs args)

prettyVarArgs :: Pretty c => (b -> Doc a) -> [b] -> [c] -> Doc a
prettyVarArgs prettyVar vs as = parens $ vsep $ zipWith (\v a -> brackets $ prettyVar v <+> pretty a) vs as

instance Pretty (CTail a) where
  pretty (C_Halt _) = group $ parens $ pretty "halt!"
  pretty (C_Wait _ last_i svs) = group $ parens $ pretty "wait!" <+> pretty last_i <+> prettyBLVars svs
  pretty (C_If _ ca tt ft) = prettyIf ca tt ft
  pretty (C_Let _ mv e bt) = prettyLet prettyBLVar (\x -> x) mv e bt
  pretty (C_Do _ s bt) = prettyDo (\x -> x) s bt
  pretty (C_Jump _ which svs vs args) = group $ parens $ pretty "jump" <+> pretty which <+> prettyBLVars svs <+> (prettyVarArgs prettyBLVar vs args)

instance Pretty FromSpec where
  pretty (FS_From bv) = parens $ pretty "#:from" <+> prettyBLVar bv
  pretty (FS_Join bv) = parens $ pretty "#:join" <+> prettyBLVar bv
  pretty (FS_Any) = parens $ pretty "#:any"

prettyCHandler :: CHandler a -> Doc ann
prettyCHandler (C_Handler _ who timeout (last_i, svs) args delay ct i) =
  group $ brackets $ pretty i <+> pretty who <+> pretty timeout <+> pretty delay <+> pretty last_i <+> prettyBLVars svs <+> prettyBLVars args <+> (nest 2 $ hardline <> pretty ct)
prettyCHandler (C_Loop _ svs args it ct i) =
  group $ brackets $ pretty i <+> pretty "!loop!" <+> prettyBLVars svs <+> prettyBLVars args <+> pretty "invariant" <+> prettyBegin it <> (nest 2 $ hardline <> pretty ct)

instance Pretty (CProgram a) where
  pretty (C_Prog _ hs) = group $ parens $ pretty "define-contract" <+> (nest 2 $ hardline <> vsep hsp)
    where hsp = map prettyCHandler hs

prettyBLVar :: BLVar -> Doc ann
prettyBLVar v = prettyILVar v

prettyBLVars :: [BLVar] -> Doc ann
prettyBLVars bs = parens $ hsep $ map prettyBLVar bs

prettyBLPart :: (BLPart, EProgram b) -> Doc ann
prettyBLPart (p, (EP_Prog _ args t)) =
  group $ parens $ pretty "define-participant" <+> pretty p <+> (nest 2 $ hardline <> vsep [argp, emptyDoc, pretty t])
  where argp = group $ parens $ vsep $ map prettyBLVar args

prettyBLParts :: BLParts b -> Doc ann
prettyBLParts ps =
  vsep $ intersperse emptyDoc $ map prettyBLPart (M.toList ps)

instance Pretty (BLProgram a) where
  pretty (BL_Prog _ ps ctc) = vsep [pretty "#lang reach/bl", emptyDoc, pretty ctc, emptyDoc, prettyBLParts ps]
