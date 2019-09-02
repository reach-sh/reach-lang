module Reach.AST where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc
import Data.List (intersperse)
import Control.Monad.Except

-- Shared types

data BaseType
  = AT_UInt256
  | AT_Bool
  | AT_Bytes
  deriving (Show,Eq,Ord)
data ExprType
  = TY_Var String
  | TY_Con BaseType
  deriving (Show,Eq)
data FunctionType
  = TY_Arrow [ExprType] ExprType
  | TY_Forall [String] FunctionType
  deriving (Show,Eq)

tBool :: ExprType
tUInt256 :: ExprType
tBytes :: ExprType
(-->) :: [ExprType] -> ExprType -> FunctionType
tBool = TY_Con AT_Bool
tUInt256 = TY_Con AT_UInt256
tBytes = TY_Con AT_Bytes
ins --> out = TY_Arrow ins out

data Constant
  = Con_I Integer
  | Con_B Bool
  | Con_BS B.ByteString
  deriving (Show,Eq)

conType :: Constant -> BaseType
conType (Con_I _) = AT_UInt256
conType (Con_B _) = AT_Bool
conType (Con_BS _) = AT_Bytes

-- -- Primitives are divided into ones that the contract can do and
-- -- ones that endpoints can do.

data C_Prim
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | PLT
  | PLE
  | PEQ
  | PGE
  | PGT
  | IF_THEN_ELSE
  | UINT256_TO_BYTES
  | DIGEST
  | BYTES_EQ
  | BYTES_LEN
  | BCAT
  | BCAT_LEFT
  | BCAT_RIGHT
  | BALANCE
  | TXN_VALUE
  deriving (Show,Eq)

data EP_Prim
  = CP C_Prim
  | RANDOM
  | INTERACT
  deriving (Show,Eq)

primType :: EP_Prim -> FunctionType
primType (CP ADD) = [tUInt256, tUInt256] --> tUInt256
primType (CP SUB) = [tUInt256, tUInt256] --> tUInt256
primType (CP MUL) = [tUInt256, tUInt256] --> tUInt256
primType (CP DIV) = [tUInt256, tUInt256] --> tUInt256
primType (CP MOD) = [tUInt256, tUInt256] --> tUInt256
primType (CP PLT) = [tUInt256, tUInt256] --> tBool
primType (CP PLE) = [tUInt256, tUInt256] --> tBool
primType (CP PEQ) = [tUInt256, tUInt256] --> tBool
primType (CP PGE) = [tUInt256, tUInt256] --> tBool
primType (CP PGT) = [tUInt256, tUInt256] --> tBool
primType (CP IF_THEN_ELSE) = TY_Forall ["a"] ([tBool, TY_Var "a", TY_Var "a"] --> TY_Var "a")
primType (CP UINT256_TO_BYTES) = [tUInt256] --> tBytes
primType (CP DIGEST) = ([tBytes] --> tUInt256)
primType (CP BYTES_EQ) = [tBytes, tBytes] --> tBool
primType (CP BYTES_LEN) = [tBytes] --> tUInt256
primType (CP BCAT)       = ([tBytes, tBytes] --> tBytes)
primType (CP BCAT_LEFT)  = ([tBytes] --> tBytes)
primType (CP BCAT_RIGHT) = ([tBytes] --> tBytes)
primType (CP BALANCE) = ([] --> tUInt256)
primType (CP TXN_VALUE) = ([] --> tUInt256)
primType RANDOM = ([] --> tUInt256)
primType INTERACT = ([tBytes] --> tBytes)

type TypeVarEnv = M.Map String BaseType

checkFun :: FunctionType -> [BaseType] -> BaseType
checkFun top topdom = toprng
  where
    toprng = case runExcept mrng of
      Left err -> error err
      Right v -> v
    mrng = hFun [] M.empty top topdom
    hTy :: TypeVarEnv -> ExprType -> Except String BaseType
    hTy γ et = case et of
      TY_Con bt -> return bt
      TY_Var v -> case M.lookup v γ of
        Nothing -> throwError $ "checkFun: Unconstrained/bound type variable: " ++ show v
        Just et' -> return et'
    hExpr :: [String] -> TypeVarEnv -> ExprType -> BaseType -> Except String TypeVarEnv
    hExpr vs γ et at = case et of
      TY_Con bt ->
        if at == bt then return γ
        else throwError $ "checkFun: Expected " ++ show bt ++ ", got: " ++ show at
      TY_Var v ->
        if not $ elem v vs then
          throwError $ "checkFun: Unbound type variable: " ++ show v
        else
          case M.lookup v γ of
            Just bt -> hExpr vs γ (TY_Con bt) at
            Nothing -> return $ M.insert v at γ
    hFun :: [String] -> TypeVarEnv -> FunctionType -> [BaseType] -> Except String BaseType
    hFun vs γ ft adom = case ft of
      TY_Forall nvs ft' -> hFun (vs ++ nvs) γ ft' adom
      TY_Arrow edom rng -> do
        γ' <- hExprs vs γ edom adom
        hTy γ' rng
    hExprs :: [String] -> TypeVarEnv -> [ExprType] -> [BaseType] -> Except String TypeVarEnv
    hExprs vs γ esl asl = case esl of
      [] ->
        case asl of
          [] -> return γ
          _ -> throwError $ "checkFun: Received more than expected"
      e1 : esl' ->
        case asl of
          [] -> throwError $ "checkFun: Received fewer than expected"
          a1 : asl' -> do
            γ' <- (hExprs vs γ esl' asl')
            hExpr vs γ' e1 a1

type Participant = String

data Role
  = RolePart Participant
  | RoleContract
  deriving (Show,Eq,Ord)

role_me :: Role -> Role -> Bool
role_me _ RoleContract = True
role_me RoleContract _ = False
role_me (RolePart x) (RolePart y) = x == y

data ClaimType
  = CT_Assert   --- Verified on all paths
  | CT_Assume   --- Always assumed true
  | CT_Require  --- Verified in honest, assumed in dishonest. (This may
                --- sound backwards, but by verifying it in honest
                --- mode, then we are checking that the other
                --- participants fulfill the promise when acting
                --- honestly.)
  | CT_Possible --- Check if an assignment of variables exists to make
                --- this true.
  deriving (Show,Eq,Ord)

{- Surface Language

 -}

{- Expanded Language (the language after expansion)

   There are some extensions we need to add in the future:

 -}

type XLVar = String

data XLExpr
  = XL_Con Constant
  | XL_Var XLVar
  | XL_PrimApp EP_Prim [XLExpr]
  | XL_If Bool XLExpr XLExpr XLExpr
  | XL_Claim ClaimType XLExpr
  --- A ToConsensus transfers control to the contract. The arguments
  --- are (initiator, message, pay expression, contract body). The
  --- message is a sequence of variables, because it binds these in
  --- the contract body. The contract body is expected to end in a
  --- FromConsensus that will switch back.
  | XL_ToConsensus Participant [XLVar] XLExpr XLExpr
  --- A FromConsensus expression is a terminator inside of a contract
  --- block that switches the context back away from the consensus,
  --- while still retaining all of the bindings established during the
  --- consensus execution.
  | XL_FromConsensus XLExpr
  | XL_Values [XLExpr]
  --- Transfer expressions are always from the contract to another
  --- role. In the future, we could make something like mutable state
  --- on a local side of a transaction that collects all the transfers
  --- and puts them in the pay position.
  | XL_Transfer Participant XLExpr
  | XL_Declassify XLExpr
  --- Where x Vars x Expression x Body
  | XL_Let (Maybe Participant) (Maybe [XLVar]) XLExpr XLExpr
  | XL_While XLVar XLExpr XLExpr XLExpr XLExpr XLExpr
  | XL_Continue XLExpr
  --- Impossible in inlined
  | XL_FunApp XLVar [XLExpr]
  deriving (Show,Eq)

data XLDef
  = XL_DefineValues [XLVar] XLExpr
  | XL_DefineFun XLVar [XLVar] XLExpr
  deriving (Show,Eq)

type XLPartInfo = (M.Map Participant [(XLVar, BaseType)])

data XLProgram =
  XL_Prog [XLDef] XLPartInfo XLExpr
  deriving (Show,Eq)

data XLInlinedProgram =
  XL_InlinedProg XLPartInfo XLExpr
  deriving (Show,Eq)

{- Intermediate Language

   This language is the result of ANF. It is stratified so that all
   expressions receive simple arguments (constants or variables). The
   most complex thing is that consensus blocks have been transformed
   into binding by observing their continuation and embedding
   it.

   Another subtlety is that IF blocks embed their continuation, which
   means if an IF is not in tail position, then it duplicates the
   continuation. The ANF transform should track whether expressions
   are pure and turn them into ITE expressions to limit how much this
   occurs.

   This language is NOT guaranteed to be type-correct.

   It is essential that all participants agree on the number of times
   consensus is reached. This means that ANF has to do another complex
   job: it must ensure that IFs are consensual.
 -}

--- The string is just for debugging, it tracks where the variable was
--- created.
type ILVar = (Int, String)

data ILArg
  = IL_Con Constant
  | IL_Var ILVar
  deriving (Show,Eq)

data ILExpr
  = IL_PrimApp EP_Prim [ILArg]
  | IL_Declassify ILArg
  deriving (Show,Eq)

data ILStmt
  = IL_Transfer Participant ILArg
  | IL_Claim ClaimType ILArg
  deriving (Show,Eq)

data ILTail
  = IL_Ret [ILArg]
  | IL_If ILArg ILTail ILTail
  --- This role represents where the action happens. If it is
  --- RoleContract, then this means that everyone does it.
  | IL_Let Role ILVar ILExpr ILTail
  | IL_Do Role ILStmt ILTail
  --- As in XL, a ToConsensus is a transfer to the contract with
  --- (initiator, message, pay amount). The tail is inside of the
  --- contract.
  | IL_ToConsensus Participant [ILVar] ILArg ILTail
  --- A FromConsensus moves back from the consensus; the tail is
  --- "local" again.
  | IL_FromConsensus ILTail
  | IL_While ILVar ILArg ILTail ILTail ILTail ILTail
  | IL_Continue ILArg
  deriving (Show,Eq)

type ILPartArgs = [(ILVar, BaseType)]
type ILPartInfo = (M.Map Participant ILPartArgs)

data ILProgram =
  IL_Prog ILPartInfo ILTail
  deriving (Show,Eq)

{- Backend Language

   These languages are the result of EPP. Like ANF, they are stratified,
   but there are two categories: one for the end-points (the
   participants) and one for the contract.

   Consensus blocks in IL are transformed into
   1. A receive from the contract in all participants.
   2. A send to the contract in the originator.
   3. A handler block in the contract.

   During EPP, the total number of consensus blocks are tracked and
   tagged with integers, so that all participants can agree on which
   block is going to run.

   -}

type BLVar = (Int, String, BaseType)

data BLArg
  = BL_Con Constant
  | BL_Var BLVar
  deriving (Show,Eq)

-- -- End-Points
data EPExpr
  = EP_Arg BLArg
  | EP_PrimApp EP_Prim [BLArg]
  deriving (Show,Eq)

data EPStmt
  = EP_Claim ClaimType BLArg
  | EP_Send Int [BLVar] [BLVar] BLArg
  deriving (Show,Eq)

data EPTail
  = EP_Ret [BLArg]
  | EP_If BLArg EPTail EPTail
  | EP_Let BLVar EPExpr EPTail
  | EP_Do EPStmt EPTail
  {- This recv is what the sender sent; we will be doing the same
     computation as the contract. -}
  | EP_Recv Bool Int [BLVar] [BLVar] EPTail
  | EP_Loop Int BLVar BLArg EPTail
  | EP_Continue Int BLArg
  deriving (Show,Eq)

data EProgram
  = EP_Prog [BLVar] EPTail
  deriving (Show,Eq)

-- -- Contracts
data CExpr
  = C_PrimApp C_Prim [BLArg]
  deriving (Show,Eq)

data CStmt
  = C_Claim ClaimType BLArg
  | C_Transfer Participant BLArg
  deriving (Show,Eq)

data CTail
  = C_Halt
  | C_Wait Int [BLVar]
  | C_If BLArg CTail CTail
  | C_Let BLVar CExpr CTail
  | C_Do CStmt CTail
  | C_Jump Int [BLVar] BLArg
  deriving (Show,Eq)

data CHandler
  --- Each handler has a message that it expects to receive
  = C_Handler Participant [BLVar] [BLVar] CTail
  | C_Loop [BLVar] BLVar CTail CTail
  deriving (Show,Eq)

--- A contract program is just a sequence of handlers.
data CProgram
  = C_Prog [Participant] [CHandler]
  deriving (Show,Eq)

-- -- Backend
type BLParts = M.Map Participant EProgram

data BLProgram
  = BL_Prog BLParts CProgram
  deriving (Show,Eq)

--- Emiting Code ---

instance Pretty BaseType where
  pretty AT_UInt256 = pretty "uint256"
  pretty AT_Bool = pretty "bool"
  pretty AT_Bytes = pretty "bytes"

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

instance Pretty XLProgram where
  pretty = viaShow

instance Pretty XLInlinedProgram where
  pretty = viaShow

instance Pretty ILArg where
  pretty (IL_Var v) = prettyILVar v
  pretty (IL_Con c) = pretty c

prettyApp :: (Pretty p, Pretty a) => p -> [a] -> Doc ann
prettyApp p al = group $ parens $ pretty p <> alp
  where alp = case al of [] -> emptyDoc
                         _ -> space <> (hsep $ map pretty al)

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

instance Pretty ILExpr where
  pretty (IL_PrimApp p al) = prettyApp p al
  pretty (IL_Declassify a) = group $ parens $ pretty "declassify" <+> pretty a

instance Pretty ILStmt where
  pretty (IL_Transfer to a) = prettyTransfer to a
  pretty (IL_Claim ct a) = prettyClaim ct a

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

instance Pretty ILTail where
  pretty (IL_Ret al) = prettyValues al
  pretty (IL_If ca tt ft) = prettyIf ca tt ft
  pretty (IL_Let r iv e bt) = prettyLet prettyILVar at iv e bt
    where at d = (group $ parens $ pretty "@" <+> pretty r <+> d)
  pretty (IL_Do r s bt) = prettyDo at s bt
    where at d = (group $ parens $ pretty "@" <+> pretty r <+> d)
  pretty (IL_ToConsensus p svs pa ct) =
    vsep [(group $ parens $ pretty "@" <+> pretty p <+> (nest 2 $ hardline <> vsep [svsp, pap])),
          pretty ct]
    where svsp = parens $ pretty "publish!" <+> prettyILVars svs
          pap = parens $ pretty "pay!" <+> pretty pa
  pretty (IL_FromConsensus lt) =
    vsep [(group $ parens $ pretty "commit!"),
          pretty lt]
  pretty (IL_While loopv inita untilt invt bodyt kt) = prettyWhile prettyILVar loopv inita untilt invt bodyt kt
  pretty (IL_Continue a) = parens $ pretty "continue!" <+> pretty a

prettyILVar :: ILVar -> Doc ann
prettyILVar (n, s) = pretty n <> pretty "/" <> pretty s

prettyILVars :: [ILVar] -> Doc ann
prettyILVars vs = parens $ hsep $ map prettyILVar vs

prettyILPartArg :: (ILVar, BaseType) -> Doc ann
prettyILPartArg (v, et) = group $ brackets $ prettyILVar v <+> pretty ":" <+> pretty et

prettyILPart :: (Participant, [(ILVar, BaseType)]) -> Doc ann
prettyILPart (p, vs) =
  group $ parens $ pretty "define-participant" <+> pretty p <> body
  where pvs = map prettyILPartArg vs
        body = case vs of [] -> emptyDoc
                          _ -> (nest 2 $ hardline <> vsep pvs)

prettyILPartInfo :: ILPartInfo -> Doc ann
prettyILPartInfo ps =
  vsep $ pretty "#:participants" : (map prettyILPart (M.toList ps))

instance Pretty ILProgram where
  pretty (IL_Prog ps t) = vsep [pretty "#lang reach/il", emptyDoc, prettyILPartInfo ps, emptyDoc, pretty "#:main", pretty t]

instance Pretty BLArg where
  pretty (BL_Con c) = pretty c
  pretty (BL_Var v) = prettyBLVar v

instance Pretty EPExpr where
  pretty (EP_Arg a) = pretty a
  pretty (EP_PrimApp p al) = prettyApp p al

instance Pretty EPStmt where
  pretty (EP_Claim ct a) = prettyClaim ct a
  pretty (EP_Send hi svs vs pa) =
    group $ parens $ pretty "send!" <+> pretty hi <+> prettyBLVars svs <+> prettyBLVars vs <+> pretty pa

instance Pretty CExpr where
  pretty (C_PrimApp p al) = prettyApp p al

instance Pretty CStmt where
  pretty (C_Claim ct a) = prettyClaim ct a
  pretty (C_Transfer to a) = prettyTransfer to a

instance Pretty EPTail where
  pretty (EP_Ret al) = prettyValues al
  pretty (EP_If ca tt ft) = prettyIf ca tt ft
  pretty (EP_Let v e bt) = prettyLet prettyBLVar (\x -> x) v e bt
  pretty (EP_Do s bt) = prettyDo (\x -> x) s bt
  pretty (EP_Recv fromme hi svs vs bt) =
    vsep [group $ parens $ pretty "define-values" <+> pretty fromme <+> prettyBLVars svs <+> prettyBLVars vs <+> (parens $ pretty "recv!" <+> pretty hi),
          pretty bt]
  pretty (EP_Loop which loopv inita bt) =
    group $ parens $ pretty "loop" <+> pretty which <+> prettyBLVar loopv <+> pretty inita <> nest 2 (hardline <> prettyBegin bt)
  pretty (EP_Continue which arg) = group $ parens $ pretty "continue" <+> pretty which <+> pretty arg

instance Pretty CTail where
  pretty (C_Halt) = group $ parens $ pretty "halt!"
  pretty (C_Wait i svs) = group $ parens $ pretty "wait!" <+> pretty i <+> prettyBLVars svs
  pretty (C_If ca tt ft) = prettyIf ca tt ft
  pretty (C_Let mv e bt) = prettyLet prettyBLVar (\x -> x) mv e bt
  pretty (C_Do s bt) = prettyDo (\x -> x) s bt
  pretty (C_Jump which svs a) = group $ parens $ pretty "jump" <+> pretty which <+> prettyBLVars svs <+> pretty a

prettyCHandler :: Int -> CHandler -> Doc ann
prettyCHandler i (C_Handler who svs args ct) =
  group $ brackets $ pretty i <+> pretty who <+> prettyBLVars svs <+> prettyBLVars args <+> (nest 2 $ hardline <> pretty ct)
prettyCHandler i (C_Loop svs arg it ct) =
  group $ brackets $ pretty i <+> pretty "!loop!" <+> prettyBLVars svs <+> prettyBLVar arg <+> pretty "invariant" <+> prettyBegin it <> (nest 2 $ hardline <> pretty ct)

instance Pretty CProgram where
  pretty (C_Prog ps hs) = group $ parens $ pretty "define-contract" <+> (nest 2 $ hardline <> vsep (psp : hsp))
    where psp = group $ pretty "#:participants" <+> (parens $ hsep $ map pretty ps)
          hsp = zipWith prettyCHandler [0..] hs

prettyBLVar :: BLVar -> Doc ann
prettyBLVar (n, s, et) = group $ brackets $ prettyILVar (n,s) <+> pretty ":" <+> pretty et

prettyBLVars :: [BLVar] -> Doc ann
prettyBLVars bs = parens $ hsep $ map prettyBLVar bs

prettyBLPart :: (Participant, EProgram) -> Doc ann
prettyBLPart (p, (EP_Prog args t)) =
  group $ parens $ pretty "define-participant" <+> pretty p <+> (nest 2 $ hardline <> vsep [argp, emptyDoc, pretty t])
  where argp = group $ parens $ vsep $ map prettyBLVar args

prettyBLParts :: BLParts -> Doc ann
prettyBLParts ps =
  vsep $ intersperse emptyDoc $ map prettyBLPart (M.toList ps)

instance Pretty BLProgram where
  pretty (BL_Prog ps ctc) = vsep [pretty "#lang reach/bl", emptyDoc, pretty ctc, emptyDoc, prettyBLParts ps]
