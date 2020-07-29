{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.NL_AST where

import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Language.JavaScript.Parser

--- Source Information
data ReachSource
  = ReachStdLib
  | ReachSourceFile FilePath
  deriving (Eq, Ord)

instance Show ReachSource where
  show ReachStdLib = "reach standard library"
  show (ReachSourceFile fp) = fp

instance Ord TokenPosn where
  compare (TokenPn x_a x_l x_c) (TokenPn y_a y_l y_c) =
    compare [x_a, x_l, x_c] [y_a, y_l, y_c]

data SrcLoc = SrcLoc (Maybe String) (Maybe TokenPosn) (Maybe ReachSource)
  deriving (Eq, Ord)

instance Show SrcLoc where
  show (SrcLoc mlab mtp mrs) = concat $ intersperse ":" $ concat [sr, loc, lab]
    where
      lab = case mlab of
        Nothing -> []
        Just s -> [s]
      sr = case mrs of
        Nothing -> []
        Just s -> [show s]
      loc = case mtp of
        Nothing -> []
        Just (TokenPn _ l c) -> [show l, show c]

--- XXX Add ctxt frame stack and display
expect_throw :: Show a => HasCallStack => SrcLoc -> a -> b
expect_throw src ce = error $ "error: " ++ (show src) ++ ": " ++ (take 512 $ show ce)

srcloc_top :: SrcLoc
srcloc_top = SrcLoc (Just "<top level>") Nothing Nothing

srcloc_src :: ReachSource -> SrcLoc
srcloc_src rs = SrcLoc Nothing Nothing (Just rs)

srcloc_at :: String -> (Maybe TokenPosn) -> SrcLoc -> SrcLoc
srcloc_at lab mp (SrcLoc _ _ rs) = SrcLoc (Just lab) mp rs

--- Security Levels
data SecurityLevel
  = Secret
  | Public
  deriving (Show, Eq)

public :: a -> (SecurityLevel, a)
public x = (Public, x)

secret :: a -> (SecurityLevel, a)
secret x = (Secret, x)

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

lvlMeet :: SecurityLevel -> (SecurityLevel, a) -> (SecurityLevel, a)
lvlMeet lvl (lvl', x) = (lvl <> lvl', x)

instance Monoid SecurityLevel where
  mempty = Public

--- Static Language
type SLVar = String

data SLType
  = T_Null
  | T_Bool
  | T_UInt256
  | T_Bytes
  | T_Fun [SLType] SLType
  | T_Array [SLType]
  | T_Obj (M.Map SLVar SLType)
  | T_Forall SLVar SLType
  | T_Var SLVar
  deriving (Eq, Show, Ord)

infix 9 -->

(-->) :: [SLType] -> SLType -> SLType
dom --> rng = T_Fun dom rng

type SLPart = B.ByteString

data SLVal
  = SLV_Null SrcLoc String
  | SLV_Bool SrcLoc Bool
  | SLV_Int SrcLoc Int
  | SLV_Bytes SrcLoc B.ByteString
  | SLV_Array SrcLoc [SLVal]
  | SLV_Object SrcLoc SLEnv
  | SLV_Clo SrcLoc (Maybe SLVar) [SLVar] JSBlock SLEnv
  | SLV_DLVar DLVar
  | SLV_Type SLType
  | --- XXX Add something about whether it's bound?
    SLV_Participant SrcLoc SLPart SLVal
  | SLV_Prim SLPrimitive
  | SLV_Form SLForm
  deriving (Eq, Show)

data ToConsensusMode
  = TCM_Publish
  | TCM_Pay
  | TCM_Timeout
  deriving (Eq, Show)

data SLForm
  = SLForm_Part_Only SLVal
  | SLForm_Part_ToConsensus SrcLoc SLPart (Maybe ToConsensusMode) (Maybe [SLVar]) (Maybe JSExpression) (Maybe (JSExpression, JSExpression))
  | SLForm_Part_OnlyAns SrcLoc SLPart SLEnv SLVal
  deriving (Eq, Show)

data ConsensusPrimOp
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
  deriving (Show, Eq, Ord)

data PrimOp
  = CP ConsensusPrimOp
  | RANDOM
  deriving (Show, Eq, Ord)

primOpType :: PrimOp -> SLType
primOpType (CP ADD) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP SUB) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP MUL) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP DIV) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP MOD) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP PLT) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PLE) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PEQ) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PGE) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PGT) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP IF_THEN_ELSE) = T_Forall "a" ([T_Bool, T_Var "a", T_Var "a"] --> T_Var "a")
primOpType (CP BYTES_EQ) = ([T_Bytes, T_Bytes] --> T_Bool)
primOpType (CP BALANCE) = ([] --> T_UInt256)
primOpType (CP TXN_VALUE) = ([] --> T_UInt256)
primOpType (CP LSH) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP RSH) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP BAND) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP BIOR) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP BXOR) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType RANDOM = ([] --> T_UInt256)

data SLPrimitive
  = SLPrim_makeEnum
  | SLPrim_declassify
  | SLPrim_digest
  | SLPrim_commit
  | SLPrim_committed
  | SLPrim_claim ClaimType
  | SLPrim_interact SrcLoc String SLType
  | SLPrim_Fun
  | SLPrim_Array
  | SLPrim_DApp
  | SLPrim_DApp_Delay SrcLoc [SLVal] SLEnv
  | SLPrim_op PrimOp
  | SLPrim_transfer
  | SLPrim_transfer_amt DLArg
  | SLPrim_transfer_amt_to DLArg
  deriving (Eq, Show)

type SLSVal = (SecurityLevel, SLVal)

type SLEnv = M.Map SLVar SLSVal

mt_env :: SLEnv
mt_env = mempty

m_fromList_public :: [(SLVar, SLVal)] -> SLEnv
m_fromList_public kvs =
  M.fromList $ map (\(k, v) -> (k, (Public, v))) kvs

data SLCtxtFrame
  = SLC_CloApp SrcLoc SrcLoc (Maybe SLVar)
  deriving (Eq, Show)

--- Dynamic Language
data DLConstant
  = DLC_Null
  | DLC_Bool Bool
  | DLC_Int Int
  | DLC_Bytes B.ByteString
  deriving (Eq, Show, Ord)

data DLVar = DLVar SrcLoc String SLType Int
  deriving (Eq, Show, Ord)

data DLArg
  = DLA_Var DLVar
  | DLA_Con DLConstant
  | DLA_Array [DLArg]
  | DLA_Obj (M.Map String DLArg)
  deriving (Eq, Show)

data DLExpr
  = DLE_PrimOp SrcLoc PrimOp [DLArg]
  | DLE_ArrayRef SrcLoc DLArg DLArg
  | DLE_Interact SrcLoc String [DLArg]
  | DLE_Digest SrcLoc [DLArg]
  deriving (Eq, Show)

expr_pure :: DLExpr -> Bool
expr_pure e =
  case e of
    DLE_PrimOp {} -> True
    DLE_ArrayRef {} -> True
    DLE_Interact {} -> False
    DLE_Digest {} -> True

data ClaimType
  = CT_Assert --- Verified on all paths
  | CT_Assume --- Always assumed true
  | CT_Require --- Verified in honest, assumed in dishonest. (This may
  --- sound backwards, but by verifying it in honest
  --- mode, then we are checking that the other
  --- participants fulfill the promise when acting
  --- honestly.)
  | CT_Possible --- Check if an assignment of variables exists to make
  --- this true.
  deriving (Show, Eq, Ord)

data DLStmt
  = DLS_Let SrcLoc DLVar DLExpr
  | DLS_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg
  | --- FIXME Record whether it is pure or local in the statement and
    --- track in monad results to avoid quadratic behavior
    DLS_If SrcLoc DLArg DLStmts DLStmts
  | DLS_Transfer SrcLoc SLPart DLArg
  | DLS_Return SrcLoc Int SLVal
  | DLS_Prompt SrcLoc (Either Int DLVar) DLStmts
  | DLS_Only SrcLoc SLPart DLStmts
  | DLS_ToConsensus SrcLoc SLPart [DLArg] [DLVar] (Maybe DLProg) (Maybe (DLArg, DLProg)) DLStmts
  | DLS_FromConsensus SrcLoc DLStmts
  deriving (Eq, Show)

stmt_pure :: DLStmt -> Bool
stmt_pure s =
  case s of
    DLS_Let _ _ e -> expr_pure e
    DLS_Claim {} -> False
    DLS_If _ _ x y -> stmts_pure x && stmts_pure y
    DLS_Transfer {} -> False
    DLS_Return {} -> False
    DLS_Prompt _ _ ss -> stmts_pure ss
    DLS_Only _ _ ss -> stmts_pure ss
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> stmts_pure ss

stmt_local :: DLStmt -> Bool
stmt_local s =
  case s of
    DLS_Let {} -> True
    DLS_Claim {} -> True
    DLS_If _ _ x y -> stmts_local x && stmts_local y
    DLS_Transfer {} -> True
    DLS_Return {} -> True
    DLS_Prompt _ _ ss -> stmts_local ss
    DLS_Only _ _ ss -> stmts_local ss
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> stmts_local ss

type DLStmts = Seq.Seq DLStmt

stmts_pure :: Foldable f => f DLStmt -> Bool
stmts_pure fs = getAll $ foldMap (All . stmt_pure) fs

stmts_local :: Foldable f => f DLStmt -> Bool
stmts_local fs = getAll $ foldMap (All . stmt_local) fs

data DLProg = DLProg DLStmts DLArg
  deriving (Eq, Show)

--- Linear Language
data LLCommon a
  = LL_Return
  | LL_Let SrcLoc DLVar DLExpr a
  | LL_Var SrcLoc DLVar a
  | LL_Set SrcLoc DLVar DLArg a
  | LL_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg a
  | LL_LocalIf SrcLoc DLArg a a a
  deriving (Eq, Show)

data LLLocal
  = LLL_Com (LLCommon LLLocal)
  deriving (Eq, Show)

data LLConsensus
  = LLC_Com (LLCommon LLConsensus)
  | LLC_If SrcLoc DLArg LLConsensus LLConsensus
  | LLC_Transfer SrcLoc SLPart DLArg LLConsensus
  | LLC_FromConsensus SrcLoc LLStep
  deriving (Eq, Show)

data LLStep
  = LLS_Com (LLCommon LLStep)
  | LLS_Stop DLArg
  | LLS_Only SrcLoc SLPart LLLocal LLStep
  | LLS_ToConsensus SrcLoc SLPart [DLArg] [DLVar] (Maybe (LLLocal, DLArg)) (Maybe (DLArg, LLStep)) LLConsensus
  deriving (Eq, Show)
