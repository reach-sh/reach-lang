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

instance Pretty Role where
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

prettyInteract :: Pretty a => String -> BaseType -> [a] -> Doc ann
prettyInteract m bt al = group $ parens $ pretty ("interact." ++ m) <+> pretty bt <+> (hsep $ map pretty al)

prettyClaim :: Pretty a => ClaimType -> a -> Doc ann
prettyClaim ct a = group $ parens $ pretty cts <+> pretty a
  where cts = case ct of
          CT_Assert -> "assert!"
          CT_Assume -> "assume!"
          CT_Require -> "require!"
          CT_Possible -> "possible?"

prettyTransfer :: Pretty a => Participant -> a -> Doc ann
prettyTransfer to a = group $ parens $ pretty "transfer!" <+> pretty to <+> pretty a

prettyWhile :: Pretty b => Pretty c => (a -> Doc ann) -> a -> b -> c -> c -> c -> c -> Doc ann
prettyWhile prettyVar loopv inita untilt invt bodyt kt = vsep [ group $ parens $ pretty "do" <+> brackets (prettyVar loopv <+> pretty inita) <> nest 2 (hardline <> pretty "until" <+> prettyBegin untilt <> hardline <> pretty "invariant" <+> prettyBegin invt <> hardline <> prettyBegin bodyt), pretty kt ]

prettyBegin :: Pretty a => a -> Doc ann
prettyBegin x = group $ parens $ pretty "begin" <+> (nest 2 $ hardline <> pretty x)

instance Pretty (ILExpr a) where
  pretty (IL_PrimApp _ p al) = prettyApp p al
  pretty (IL_Interact _ m bt al) = prettyInteract m bt al
  pretty (IL_Declassify _ a) = group $ parens $ pretty "declassify" <+> pretty a

instance Pretty (ILStmt a) where
  pretty (IL_Transfer _ to a) = prettyTransfer to a
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
  pretty (IL_ToConsensus _ (p, svs, pa) (twho, da, tt) ct) =
    vsep [(group $ parens $ pretty "@" <+> pretty p <+> (nest 2 $ hardline <> vsep [svsp, pap, tp])),
          pretty ct]
    where svsp = parens $ pretty "publish!" <+> prettyILVars svs
          pap = parens $ pretty "pay!" <+> pretty pa
          tp = parens $ pretty "timeout" <+> pretty twho <+> pretty da <+> (nest 2 $ hardline <> pretty tt)
  pretty (IL_FromConsensus _ lt) =
    vsep [(group $ parens $ pretty "commit!"),
          pretty lt]
  pretty (IL_While _ loopv inita untilt invt bodyt kt) = prettyWhile prettyILVar loopv inita untilt invt bodyt kt
  pretty (IL_Continue _ a) = parens $ pretty "continue!" <+> pretty a

prettyILVar :: ILVar -> Doc ann
prettyILVar (n, (s, et)) = pretty n <> pretty "/" <> pretty s <> pretty ":" <> pretty et

prettyILVars :: [ILVar] -> Doc ann
prettyILVars vs = parens $ hsep $ map prettyILVar vs

prettyILPartArg :: ILVar -> Doc ann
prettyILPartArg v = group $ brackets $ prettyILVar v

prettyILPart :: (Participant, [ILVar]) -> Doc ann
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

instance Pretty (EPStmt a) where
  pretty (EP_Claim _ ct a) = prettyClaim ct a
  pretty (EP_Send _ hi svs vs pa) =
    group $ parens $ pretty "send!" <+> pretty hi <+> prettyBLVars svs <+> prettyBLVars vs <+> pretty pa

instance Pretty (CExpr a) where
  pretty (C_PrimApp _ p al) = prettyApp p al

instance Pretty (CStmt a) where
  pretty (C_Claim _ ct a) = prettyClaim ct a
  pretty (C_Transfer _ to a) = prettyTransfer to a

instance Pretty (EPTail a) where
  pretty (EP_Ret _ al) = prettyValues al
  pretty (EP_If _ ca tt ft) = prettyIf ca tt ft
  pretty (EP_Let _ v e bt) = prettyLet prettyBLVar (\x -> x) v e bt
  pretty (EP_Do _ s bt) = prettyDo (\x -> x) s bt
  pretty (EP_Recv _ fromme hi svs vs bt) =
    vsep [group $ parens $ pretty "define-values" <+> pretty fromme <+> prettyBLVars svs <+> prettyBLVars vs <+> (parens $ pretty "recv!" <+> pretty hi),
          pretty bt]
  pretty (EP_Loop _ which loopv inita bt) =
    group $ parens $ pretty "loop" <+> pretty which <+> prettyBLVar loopv <+> pretty inita <> nest 2 (hardline <> prettyBegin bt)
  pretty (EP_Continue _ which arg) = group $ parens $ pretty "continue" <+> pretty which <+> pretty arg

instance Pretty (CTail a) where
  pretty (C_Halt _) = group $ parens $ pretty "halt!"
  pretty (C_Wait _ i svs) = group $ parens $ pretty "wait!" <+> pretty i <+> prettyBLVars svs
  pretty (C_If _ ca tt ft) = prettyIf ca tt ft
  pretty (C_Let _ mv e bt) = prettyLet prettyBLVar (\x -> x) mv e bt
  pretty (C_Do _ s bt) = prettyDo (\x -> x) s bt
  pretty (C_Jump _ which svs a) = group $ parens $ pretty "jump" <+> pretty which <+> prettyBLVars svs <+> pretty a

prettyCHandler :: Int -> CHandler a -> Doc ann
prettyCHandler i (C_Handler _ who svs args ct) =
  group $ brackets $ pretty i <+> pretty who <+> prettyBLVars svs <+> prettyBLVars args <+> (nest 2 $ hardline <> pretty ct)
prettyCHandler i (C_Loop _ svs arg it ct) =
  group $ brackets $ pretty i <+> pretty "!loop!" <+> prettyBLVars svs <+> prettyBLVar arg <+> pretty "invariant" <+> prettyBegin it <> (nest 2 $ hardline <> pretty ct)

instance Pretty (CProgram a) where
  pretty (C_Prog _ ps hs) = group $ parens $ pretty "define-contract" <+> (nest 2 $ hardline <> vsep (psp : hsp))
    where psp = group $ pretty "#:participants" <+> (parens $ hsep $ map pretty ps)
          hsp = zipWith prettyCHandler [0..] hs

prettyBLVar :: BLVar -> Doc ann
prettyBLVar v = prettyILVar v

prettyBLVars :: [BLVar] -> Doc ann
prettyBLVars bs = parens $ hsep $ map prettyBLVar bs

prettyBLPart :: (Participant, EProgram b) -> Doc ann
prettyBLPart (p, (EP_Prog _ args t)) =
  group $ parens $ pretty "define-participant" <+> pretty p <+> (nest 2 $ hardline <> vsep [argp, emptyDoc, pretty t])
  where argp = group $ parens $ vsep $ map prettyBLVar args

prettyBLParts :: BLParts b -> Doc ann
prettyBLParts ps =
  vsep $ intersperse emptyDoc $ map prettyBLPart (M.toList ps)

instance Pretty (BLProgram a) where
  pretty (BL_Prog _ ps ctc) = vsep [pretty "#lang reach/bl", emptyDoc, pretty ctc, emptyDoc, prettyBLParts ps]
