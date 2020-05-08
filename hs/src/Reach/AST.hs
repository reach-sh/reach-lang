{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}
module Reach.AST where

import Data.Data
import GHC.Generics
import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Shared types

data BaseType
  = BT_UInt256
  | BT_Bool
  | BT_Bytes
  | BT_Address
  deriving (Show,Eq,Ord,Generic,NFData,Data)

data LType
  = LT_BT BaseType
  | LT_FixedArray BaseType Integer
  deriving (Show,Eq,Ord,Generic,NFData,Data)

data ExprType
  = TY_Var String
  | TY_Con LType
  deriving (Show,Eq)
data FunctionType
  = TY_Arrow [ExprType] ExprType
  | TY_Forall [String] FunctionType
  deriving (Show,Eq)

tBool :: ExprType
tBool = TY_Con $ LT_BT BT_Bool

tUInt256 :: ExprType
tUInt256 = TY_Con $ LT_BT BT_UInt256

tBytes :: ExprType
tBytes = TY_Con $ LT_BT BT_Bytes

(-->) :: [ExprType] -> ExprType -> FunctionType
ins --> out = TY_Arrow ins out

data Constant
  = Con_I Integer
  | Con_B Bool
  | Con_BS B.ByteString
  deriving (Show,Eq,Ord,Generic,NFData,Data)

conType :: Constant -> BaseType
conType (Con_I _) = BT_UInt256
conType (Con_B _) = BT_Bool
conType (Con_BS _) = BT_Bytes

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
  | BYTES_EQ
  | BALANCE
  | TXN_VALUE
  | LSH
  | RSH
  | BAND
  | BIOR
  | BXOR
  deriving (Show,Eq,Ord,Generic,NFData,Data)

data EP_Prim
  = CP C_Prim
  | RANDOM
  deriving (Show,Eq,Ord,Generic,NFData,Data)

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
primType (CP BYTES_EQ) = ([tBytes, tBytes] --> tBool)
primType (CP BALANCE) = ([] --> tUInt256)
primType (CP TXN_VALUE) = ([] --> tUInt256)
primType (CP LSH) = [tUInt256, tUInt256] --> tUInt256
primType (CP RSH) = [tUInt256, tUInt256] --> tUInt256
primType (CP BAND) = [tUInt256, tUInt256] --> tUInt256
primType (CP BIOR) = [tUInt256, tUInt256] --> tUInt256
primType (CP BXOR) = [tUInt256, tUInt256] --> tUInt256
primType RANDOM = ([] --> tUInt256)

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
  deriving (Show,Eq,Ord,Generic,NFData,Data)

data Role a
  = RolePart a
  | RoleContract
  deriving (Show,Eq,Ord,Generic,NFData,Data)

role_me :: Eq a => Role a -> Role a -> Bool
role_me _ RoleContract = True
role_me RoleContract _ = False
role_me (RolePart x) (RolePart y) = x == y

data Effect
  = Eff_Comm
  | Eff_Claim
  deriving (Show,Eq,Ord,Generic,NFData,Data)

type Effects = S.Set Effect

{- Expanded Language (the language after expansion) -}

type XLVar = String
type XLPart = XLVar

data XLType a
  = XLT_BT a BaseType
  | XLT_Array a (XLType a) (XLExpr a)
  deriving (Show,Eq,Generic,NFData,Data)

data XLExpr a
  = XL_Con a Constant
  | XL_Var a XLVar
  | XL_Prim a EP_Prim
  | XL_If a (XLExpr a) (XLExpr a) (XLExpr a)
  | XL_Claim a ClaimType (XLExpr a)
  | XL_ToConsensus a (XLPart, [XLVar], (XLExpr a)) (Maybe XLPart, (XLExpr a), (XLExpr a)) (XLExpr a)
  | XL_FromConsensus a (XLExpr a)
  | XL_Values a [XLExpr a]
  | XL_Transfer a XLVar (XLExpr a)
  | XL_Declassify a (XLExpr a)
  | XL_Let a (Maybe XLPart) (Maybe [XLVar]) (XLExpr a) (XLExpr a)
  | XL_While a [XLVar] (XLExpr a) (XLExpr a) (XLExpr a) (XLExpr a) (XLExpr a)
  | XL_Continue a (XLExpr a)
  | XL_Interact a String (XLType a) [XLExpr a]
  | XL_FunApp a (XLExpr a) [XLExpr a]
  | XL_Lambda a [XLVar] (XLExpr a)
  | XL_Digest a [XLExpr a]
  | XL_ArrayRef a (XLExpr a) (XLExpr a)
  deriving (Show,Eq,Generic,NFData,Data)

data XLDef a
  = XL_DefineValues a [XLVar] (XLExpr a)
  | XL_DefineFun a XLVar [XLVar] (XLExpr a)
  deriving (Show,Eq,Generic,NFData)

type XLPartInfo a = (M.Map XLPart (a, [(a, XLVar, (XLType a))]))

data XLProgram  a=
  XL_Prog a [XLDef a] (XLPartInfo a) (XLExpr a)
  deriving (Show,Eq,Generic,NFData)

--- Inlined Language (the language after expansion)

type XILVar = (String, LType)
type XILPart = XILVar

data XILExpr a
  = XIL_Con a Constant
  | XIL_Var a XILVar
  | XIL_PrimApp a EP_Prim LType [XILExpr a]
  | XIL_If a Effects (XILExpr a) [LType] (XILExpr a) (XILExpr a)
  | XIL_Claim a ClaimType (XILExpr a)
  | XIL_ToConsensus a (Bool, XILPart, [XILVar], (XILExpr a)) (Maybe XILPart, (XILExpr a), (XILExpr a)) (XILExpr a)
  | XIL_FromConsensus a (XILExpr a)
  | XIL_Values a [XILExpr a]
  | XIL_Transfer a XLVar (XILExpr a)
  | XIL_Declassify a LType (XILExpr a)
  | XIL_Let a (Maybe XILPart) (Maybe [XILVar]) (XILExpr a) (XILExpr a)
  | XIL_While a [XILVar] (XILExpr a) (XILExpr a) (XILExpr a) (XILExpr a) (XILExpr a)
  | XIL_Continue a (XILExpr a)
  | XIL_Interact a String LType [XILExpr a]
  | XIL_Digest a [XILExpr a]
  | XIL_ArrayRef a LType (XILExpr a) (XILExpr a)
  deriving (Show,Eq)

type XILPartInfo a = (M.Map XILPart (a, [(a, XILVar)]))

data XILProgram a =
  XIL_Prog a [LType] (XILPartInfo a) (XILExpr a)
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

   It is essential that all participants agree on the number of times
   consensus is reached. This means that ANF has to do another complex
   job: it must ensure that IFs are consensual.
 -}

type ILVar = (Int, XILVar)
type ILPart = ILVar

data ILArg a
  = IL_Con a Constant
  | IL_Var a ILVar
  deriving (Show,Eq)

ilarg_type :: ILArg a -> LType
ilarg_type (IL_Con _ c) = LT_BT $ conType c
ilarg_type (IL_Var _ (_, (_, lt))) = lt

data ILExpr a
  = IL_PrimApp a EP_Prim [ILArg a]
  | IL_Declassify a (ILArg a)
  | IL_Interact a String LType [ILArg a]
  | IL_Digest a [ILArg a]
  | IL_ArrayRef a (ILArg a) (ILArg a)
  deriving (Show,Eq)

data ILStmt a
  = IL_Transfer a ILVar (ILArg a)
  | IL_Claim a ClaimType (ILArg a)
  deriving (Show,Eq)

data ILTail a
  = IL_Ret a [ILArg a]
  | IL_If a (ILArg a) (ILTail a) (ILTail a)
  | IL_Let a (Role ILPart) ILVar (ILExpr a) (ILTail a)
  | IL_Do a (Role ILPart) (ILStmt a) (ILTail a)
  | IL_ToConsensus a (Bool, ILPart, [ILVar], (ILArg a)) (Maybe ILPart, (ILArg a), (ILTail a)) (ILTail a)
  | IL_FromConsensus a (ILTail a)
  | IL_While a [ILVar] [ILArg a] (ILTail a) (ILTail a) (ILTail a) (ILTail a)
  | IL_Continue a [ILArg a]
  deriving (Show,Eq)

type ILPartArgs a = [ILVar]
type ILPartInfo a = (M.Map ILPart (ILPartArgs a))

data ILProgram a =
  IL_Prog a [LType] (ILPartInfo a) (ILTail a)
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

type BLVar = ILVar

blvar_type :: BLVar -> LType
blvar_type (_, (_, lt)) = lt

type BLPart = BLVar

blpart_name :: BLPart -> String
blpart_name (_, (pn, _)) = pn

data FromSpec
  = FS_From BLPart
  | FS_Join BLPart
  | FS_Any
  deriving (Show,Eq)

data BLArg a
  = BL_Con a Constant
  | BL_Var a BLVar
  deriving (Show,Eq)

blarg_type :: BLArg a -> LType
blarg_type (BL_Con _ c) = LT_BT $ conType c
blarg_type (BL_Var _ v) = blvar_type v

-- -- End-Points
data EPExpr a
  = EP_Arg a (BLArg a)
  | EP_PrimApp a EP_Prim [BLArg a]
  | EP_Interact a String LType [BLArg a]
  | EP_Digest a [BLArg a]
  | EP_ArrayRef a (BLArg a) (BLArg a)
  deriving (Show,Eq)

data EPStmt a
  = EP_Claim a ClaimType (BLArg a)
  | EP_Transfer a BLVar (BLArg a)
  deriving (Show,Eq)

data EPTail a
  = EP_Ret a [BLArg a]
  | EP_If a (BLArg a) (EPTail a) (EPTail a)
  | EP_Let a BLVar (EPExpr a) (EPTail a)
  | EP_Do a (EPStmt a) (EPTail a)
  | EP_SendRecv a [BLVar] (FromSpec, Int, [BLVar], (BLArg a), (EPTail a)) (FromSpec, Int, BLArg a, EPTail a)
  | EP_Recv a [BLVar] (FromSpec, Int, [BLVar], (EPTail a)) (FromSpec, Int, BLArg a, EPTail a)
  | EP_FromConsensus a (EPTail a)
  | EP_Loop a Int [BLVar] [BLArg a] (EPTail a)
  | EP_Continue a Int [BLVar] [(BLArg a)]
  deriving (Show,Eq)

data EProgram a
  = EP_Prog a [BLVar] (EPTail a)
  deriving (Show,Eq)

--- --- Gather interactions

epe_interacts :: EPExpr a -> M.Map String ([LType], LType)
epe_interacts e =
  case e of
    EP_Arg _ _ -> mempty
    EP_PrimApp _ _ _ -> mempty
    EP_Interact _ n rt args -> M.singleton n (arg_tys, rt)
      where arg_tys = map blarg_type args
    EP_Digest _ _ -> mempty
    EP_ArrayRef _ _ _ -> mempty
  
ept_interacts :: EPTail a -> M.Map String ([LType], LType)
ept_interacts t =
  case t of
    EP_Ret _ _ -> mempty
    EP_If _ _ tt ft -> ept_interacts tt <> ept_interacts ft
    EP_Let _ _ le bt -> epe_interacts le <> ept_interacts bt
    EP_Do _ _ bt -> ept_interacts bt
    EP_SendRecv _ _ (_, _, _, _, ot) (_, _, _, tt) ->
      ept_interacts ot <> ept_interacts tt
    EP_Recv _ _ (_, _, _, ot) (_, _, _, tt) ->
      ept_interacts ot <> ept_interacts tt
    EP_Loop _ _ _ _ bt -> ept_interacts bt
    EP_Continue _ _ _ _ -> mempty
    EP_FromConsensus _ bt -> ept_interacts bt

ep_interacts :: EProgram a -> M.Map String ([LType], LType)
ep_interacts (EP_Prog _ _ et) = ept_interacts et

-- -- Contracts
data CExpr a
  = C_PrimApp a C_Prim [BLArg a]
  | C_Digest a [BLArg a]
  | C_ArrayRef a (BLArg a) (BLArg a)
  deriving (Show,Eq)

data CStmt a
  = C_Claim a ClaimType (BLArg a)
  | C_Transfer a BLVar (BLArg a)
  deriving (Show,Eq)

data CTail a
  = C_Halt a
  | C_Wait a Int [BLVar]
  | C_If a (BLArg a) (CTail a) (CTail a)
  | C_Let a BLVar (CExpr a) (CTail a)
  | C_Do a (CStmt a) (CTail a)
  | C_Jump a Int [BLVar] [BLVar] [BLArg a]
  deriving (Show,Eq)

data CHandler a
  --- Each handler has a message that it expects to receive
  = C_Handler a FromSpec Bool (Int, [BLVar]) [BLVar] (BLArg a) (CTail a) Int
  | C_Loop a [BLVar] [BLVar] (CTail a) (CTail a) Int
  deriving (Show,Eq)

--- A contract program is just a sequence of handlers.
data CProgram a
  = C_Prog a [CHandler a]
  deriving (Show,Eq)

-- -- Backend
type BLParts a = M.Map BLPart (EProgram a)

data BLProgram a
  = BL_Prog a [LType] (BLParts a) (CProgram a)
  deriving (Show,Eq)

