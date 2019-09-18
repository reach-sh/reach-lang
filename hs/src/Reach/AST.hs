module Reach.AST where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

-- Shared types

data BaseType
  = BT_UInt256
  | BT_Bool
  | BT_Bytes
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
tBool = TY_Con BT_Bool

tUInt256 :: ExprType
tUInt256 = TY_Con BT_UInt256

tBytes :: ExprType
tBytes = TY_Con BT_Bytes

(-->) :: [ExprType] -> ExprType -> FunctionType
ins --> out = TY_Arrow ins out

data Constant
  = Con_I Integer
  | Con_B Bool
  | Con_BS B.ByteString
  deriving (Show,Eq)

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
X
 -}

type XLVar = String

data XLExpr a
  = XL_Con a Constant
  | XL_Var a XLVar
  | XL_Prim a EP_Prim
  | XL_If a (XLExpr a) (XLExpr a) (XLExpr a)
  | XL_Claim a ClaimType (XLExpr a)
  --- A ToConsensus transfers control to the contract. The arguments
  --- are (initiator, message, pay expression, contract body). The
  --- message is a sequence of variables, because it binds these in
  --- the contract body. The contract body is expected to end in a
  --- FromConsensus that will switch back.
  | XL_ToConsensus a (Participant, [XLVar], (XLExpr a)) (Participant, (XLExpr a), (XLExpr a)) (XLExpr a)
  --- A FromConsensus expression is a terminator inside of a contract
  --- block that switches the context back away from the consensus,
  --- while still retaining all of the bindings established during the
  --- consensus execution.
  | XL_FromConsensus a (XLExpr a)
  | XL_Values a [XLExpr a]
  --- Transfer expressions are always from the contract to another
  --- role. In the future, we could make something like mutable state
  --- on a local side of a transaction that collects all the transfers
  --- and puts them in the pay position.
  | XL_Transfer a Participant (XLExpr a)
  | XL_Declassify a (XLExpr a)
  --- Where x Vars x Expression x Body
  | XL_Let a (Maybe Participant) (Maybe [XLVar]) (XLExpr a) (XLExpr a)
  | XL_While a XLVar (XLExpr a) (XLExpr a) (XLExpr a) (XLExpr a) (XLExpr a)
  | XL_Continue a (XLExpr a)
  | XL_Interact a String BaseType [XLExpr a]
  | XL_FunApp a (XLExpr a) [XLExpr a]
  | XL_Lambda a [XLVar] (XLExpr a)
  deriving (Show,Eq)

data XLDef a
  = XL_DefineValues a [XLVar] (XLExpr a)
  | XL_DefineFun a XLVar [XLVar] (XLExpr a)
  deriving (Show,Eq)

type XLPartInfo a = (M.Map Participant (a, [(a, XLVar, BaseType)]))

data XLProgram  a=
  XL_Prog a [XLDef a] (XLPartInfo a) (XLExpr a)
  deriving (Show,Eq)

--- Inlined Language (the language after expansion)

type XILVar = (String, BaseType)

data XILExpr a
  = XIL_Con a Constant
  | XIL_Var a XILVar
  | XIL_PrimApp a EP_Prim BaseType [XILExpr a]
  | XIL_If a Bool (XILExpr a) [BaseType] (XILExpr a) (XILExpr a)
  | XIL_Claim a ClaimType (XILExpr a)
  | XIL_ToConsensus a (Participant, [XILVar], (XILExpr a)) (Participant, (XILExpr a), (XILExpr a)) (XILExpr a)
  | XIL_FromConsensus a (XILExpr a)
  | XIL_Values a [XILExpr a]
  | XIL_Transfer a Participant (XILExpr a)
  | XIL_Declassify a BaseType (XILExpr a)
  | XIL_Let a (Maybe Participant) (Maybe [XILVar]) (XILExpr a) (XILExpr a)
  | XIL_While a XILVar (XILExpr a) (XILExpr a) (XILExpr a) (XILExpr a) (XILExpr a)
  | XIL_Continue a (XILExpr a)
  | XIL_Interact a String BaseType [XILExpr a]
  deriving (Show,Eq)

type XILPartInfo a = (M.Map Participant (a, [(a, XILVar)]))

data XILProgram a =
  XIL_Prog a (XILPartInfo a) (XILExpr a)
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

data ILArg a
  = IL_Con a Constant
  | IL_Var a ILVar
  deriving (Show,Eq)

data ILExpr a
  = IL_PrimApp a EP_Prim [ILArg a]
  | IL_Declassify a (ILArg a)
  | IL_Interact a String BaseType [ILArg a]
  deriving (Show,Eq)

data ILStmt a
  = IL_Transfer a Participant (ILArg a)
  | IL_Claim a ClaimType (ILArg a)
  deriving (Show,Eq)

data ILTail a
  = IL_Ret a [ILArg a]
  | IL_If a (ILArg a) (ILTail a) (ILTail a)
  --- This role represents where the action happens. If it is
  --- RoleContract, then this means that everyone does it.
  | IL_Let a Role ILVar (ILExpr a) (ILTail a)
  | IL_Do a Role (ILStmt a) (ILTail a)
  --- As in XL, a ToConsensus is a transfer to the contract with
  --- (initiator, message, pay amount). The tail is inside of the
  --- contract.
  | IL_ToConsensus a Participant [ILVar] (ILArg a) (ILTail a)
  --- A FromConsensus moves back from the consensus; the tail is
  --- "local" again.
  | IL_FromConsensus a (ILTail a)
  | IL_While a ILVar (ILArg a) (ILTail a) (ILTail a) (ILTail a) (ILTail a)
  | IL_Continue a (ILArg a)
  deriving (Show,Eq)

type ILPartArgs a = [ILVar]
type ILPartInfo a = (M.Map Participant (ILPartArgs a))

data ILProgram a =
  IL_Prog a (ILPartInfo a) (ILTail a)
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

data BLArg a
  = BL_Con a Constant
  | BL_Var a BLVar
  deriving (Show,Eq)

-- -- End-Points
data EPExpr a
  = EP_Arg a (BLArg a)
  | EP_PrimApp a EP_Prim [BLArg a]
  | EP_Interact a String BaseType [BLArg a]
  deriving (Show,Eq)

data EPStmt a
  = EP_Claim a ClaimType (BLArg a)
  | EP_Send a Int [BLVar] [BLVar] (BLArg a)
  deriving (Show,Eq)

data EPTail a
  = EP_Ret a [BLArg a]
  | EP_If a (BLArg a) (EPTail a) (EPTail a)
  | EP_Let a BLVar (EPExpr a) (EPTail a)
  | EP_Do a (EPStmt a) (EPTail a)
  {- This recv is what the sender sent; we will be doing the same
     computation as the contract. -}
  | EP_Recv a Bool Int [BLVar] [BLVar] (EPTail a)
  | EP_Loop a Int BLVar (BLArg a) (EPTail a)
  | EP_Continue a Int (BLArg a)
  deriving (Show,Eq)

data EProgram a
  = EP_Prog a [BLVar] (EPTail a)
  deriving (Show,Eq)

-- -- Contracts
data CExpr a
  = C_PrimApp a C_Prim [BLArg a]
  deriving (Show,Eq)

data CStmt a
  = C_Claim a ClaimType (BLArg a)
  | C_Transfer a Participant (BLArg a)
  deriving (Show,Eq)

data CTail a
  = C_Halt a
  | C_Wait a Int [BLVar]
  | C_If a (BLArg a) (CTail a) (CTail a)
  | C_Let a BLVar (CExpr a) (CTail a)
  | C_Do a (CStmt a) (CTail a)
  | C_Jump a Int [BLVar] (BLArg a)
  deriving (Show,Eq)

data CHandler a
  --- Each handler has a message that it expects to receive
  = C_Handler a Participant [BLVar] [BLVar] (CTail a)
  | C_Loop a [BLVar] BLVar (CTail a) (CTail a)
  deriving (Show,Eq)

--- A contract program is just a sequence of handlers.
data CProgram a
  = C_Prog a [Participant] [CHandler a]
  deriving (Show,Eq)

-- -- Backend
type BLParts a = M.Map Participant (EProgram a)

data BLProgram a
  = BL_Prog a (BLParts a) (CProgram a)
  deriving (Show,Eq)

