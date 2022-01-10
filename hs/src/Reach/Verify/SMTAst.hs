module Reach.Verify.SMTAst (
  SMTTrace(..),
  BindingOrigin(..),
  TheoremKind(..),
  SMTLet(..),
  SMTExpr(..),
  SynthExpr(..),
  SMTCat(..),
  SMTVal(..)) where

import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Texty
import Reach.AddCounts
import Reach.CollectCounts
import qualified Data.ByteString as B
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as M
import Data.List (partition)
import Reach.Util (impossible)
import Data.Maybe
import Reach.UnsafeUtil

data BindingOrigin
  = O_Join SLPart Bool
  | O_ClassJoin SLPart
  | O_BuiltIn
  | O_Var
  | O_Assignment
  | O_ReduceVar
  | O_ExportArg
  | O_SwitchCase DLArg
  | O_Publish
  deriving (Eq)

instance PrettySubst BindingOrigin where
  prettySubst = \case
    O_Join who False -> return $ "a dishonest join from " <> pretty who
    O_Join who True -> return $ "an honest join from " <> pretty who
    O_ClassJoin who -> return $ "a join by a class member of " <> pretty who
    O_BuiltIn -> return "builtin"
    O_Var -> return "function return"
    O_Assignment -> return "loop variable"
    O_ReduceVar -> return "map reduction"
    O_ExportArg -> return "function argument"
    O_SwitchCase c -> do
      c' <- prettySubst c
      return $ "switch case binding for " <> c'
    O_Publish -> return $ "a publication"

instance IsPure BindingOrigin where
  isPure = \case
    O_ReduceVar -> True
    O_ExportArg -> True
    O_SwitchCase {} -> True
    -- Rest are `False` to be conservative with the lack of info
    O_Join {} -> False
    O_ClassJoin _ -> False
    O_BuiltIn -> False
    O_Var -> False
    O_Assignment -> False
    O_Publish -> False

data TheoremKind
  = TClaim ClaimType
  | TInvariant Bool
  | TWhen
  deriving (Eq, Show)

instance Pretty TheoremKind where
  pretty = \case
    TClaim c -> pretty c
    TInvariant False -> "while invariant before loop"
    TInvariant True -> "while invariant after loop"
    TWhen -> "when is analyzable"

data SMTCat
  = Witness
  | Context
  deriving (Eq, Show)

instance Pretty SMTCat where
  pretty = viaShow

data SynthExpr
  = SMTMapNew                           -- Context
  | SMTMapFresh DLVar                   -- Witness
  | SMTMapSet DLVar DLArg (Maybe DLArg) -- Context
  | SMTMapRef DLVar DLArg               -- Context
  deriving (Eq, Show)

instance PrettySubst SynthExpr where
  prettySubst = \case
    SMTMapNew -> return "new Map()"
    SMTMapFresh _ -> do
      return "<fresh Map>"
    SMTMapSet m f ma -> do
      m' <- prettySubst $ DLA_Var m
      f' <- prettySubst f
      ma' <- prettySubst ma
      return $ m' <> brackets (f' <+> "<-" <+> ma')
    SMTMapRef m f -> do
      m' <- prettySubst $ DLA_Var m
      f' <- prettySubst f
      return $ m' <> brackets f'

data SMTExpr
  = SMTModel BindingOrigin
  | SMTProgram DLExpr
  | SMTSynth SynthExpr
  deriving (Eq)

instance PrettySubst SMTExpr where
  prettySubst = \case
    SMTModel bo -> do
      b' <- prettySubst bo
      return $ "<" <> b' <> ">"
    SMTProgram de ->  prettySubst de
    SMTSynth se -> prettySubst se

instance Show SMTExpr where
  show = \case
    SMTProgram dl -> show . pretty $ dl
    ow -> show ow

instance CanDupe SynthExpr where
  canDupe = \case
    SMTMapRef {} -> True
    SMTMapNew -> True
    _ -> False

instance IsPure SynthExpr where
  isPure = \case
    SMTMapRef {} -> True
    _ -> False

instance CanDupe SMTExpr where
  canDupe = \case
    SMTProgram de -> canDupe de
    SMTSynth de -> canDupe de
    _ -> True

instance IsPure SMTExpr where
  isPure = \case
    SMTProgram de -> isPure de
    SMTSynth se -> isPure se
    _ -> True

data SMTLet
  = SMTLet SrcLoc DLVar DLLetVar SMTCat SMTExpr
  | SMTCon String (Maybe SMTVal) SMTExpr
  | SMTNop SrcLoc
  deriving (Eq, Show)

instance Ord SMTLet where
  compare (SMTLet _ ldv _ _ _) (SMTLet _ rdv _ _ _) = compare ldv rdv
  compare _ _ = LT

prettySubstWith :: PrettySubst a => PrettySubstEnv -> a -> Doc
prettySubstWith env' =
  runIdentity
    . flip runReaderT env'
    . prettySubst

needsParens :: SMTExpr -> Bool
needsParens = \case
  SMTProgram (DLE_PrimOp _ _ args) -> length args <= 2
  _ -> False

-- Don't inline tail so we can show final value of assertion variable
-- Inline variables that are equal to variables
-- Don't inline variables used many times
instance PrettySubst [SMTLet] where
  prettySubst = \case
    [] -> return ""
    SMTCon _ (Just v) se : tl -> do
      se' <- prettySubst se
      let msg = "  const" <+> se' <+> "=" <+> pretty v <> ";" <> hardline
      rest <- prettySubst tl
      return $ msg <> hardline <> rest
    SMTLet _ dv lv Context se : tl -> do
      se' <- prettySubst se
      env <- ask
      let (isInlinable, env') =
            case (lv, null tl, se) of
              (DLV_Eff, _, _) ->
                impossible "PrettySubst [SMTLet]: AC did not remove Eff bindings"
              (_, _, SMTProgram (DLE_Arg _ (DLA_Var rhs))) ->
                (True, M.insert dv (fromMaybe (viaShow rhs) $ M.lookup rhs env) env)
              (DLV_Let DVC_Once _, False, _) ->
                (True, M.insert dv (if needsParens se then parens se' else se') env)
              (_, _, _) ->
                (False, env)
      case isInlinable of
        True ->
          return $ prettySubstWith env' tl
        False -> do
          let wouldBe x = hardline <> "  //    ^ would be " <> viaShow x
          let info = maybe "" wouldBe (M.lookup dv env)
          let msg = "  const" <+> viaShow dv <+> "=" <+> se' <> ";" <> info
          return $ msg <> hardline <> prettySubstWith (M.delete dv env) tl
    SMTLet at dv _ Witness se : tl -> do
      env <- ask
      se' <- prettySubst se
      let wouldBe x = hardline <> "  //    ^ could = " <> x <> hardline <>
                                  "  //      from:" <+> pretty (unsafeRedactAbsStr $ show at)
      let info = maybe "" wouldBe (M.lookup dv env)
      let msg = "  const" <+> viaShow dv <+> "=" <+> se' <> ";" <> info
      return $ msg <> hardline <> prettySubstWith (M.delete dv env) tl
    _ : tl -> prettySubst tl


data SMTTrace
  = SMTTrace [SMTLet] TheoremKind DLVar
  deriving (Eq, Show)

instance Pretty SMTTrace where
  pretty = runIdentity . flip runReaderT mempty . prettySubst

instance PrettySubst SMTTrace where
  prettySubst (SMTTrace lets tk dv) = do
    let (w_lets, c_lets) = partition isWitness lets
    w_lets' <- prettySubst w_lets
    env <- ask
    let witnessVars = foldr (flip collectVars) [] w_lets
    let env' = foldr M.delete env witnessVars
    let c_lets' = prettySubstWith env' c_lets
    return $
      "  // Violation Witness" <> hardline <> hardline <> w_lets' <> hardline <>
      "  // Theorem Formalization" <> hardline <> hardline <> c_lets' <>
      "  " <> tk' <> parens (viaShow dv) <> ";" <> hardline
    where
      tk' = case tk of
              TClaim c -> pretty c
              TInvariant _ -> "assert"
              TWhen -> "analyze"
      isWitness = \case
        SMTLet _ _ _ Witness _ -> True
        SMTCon {} -> True
        _ -> False
      collectVars acc = \case
        SMTLet _ ldv _ _ _ -> ldv : acc
        _ -> acc

data SMTVal
  = SMV_Bool Bool
  | SMV_Int Integer
  | SMV_Address Integer
  | SMV_Digest String
  | SMV_Null
  | SMV_Bytes B.ByteString
  | SMV_Array DLType [SMTVal]
  | SMV_Tuple [SMTVal]
  | SMV_Object (M.Map String SMTVal)
  | SMV_Struct [(String, SMTVal)]
  | SMV_Data String [SMTVal]
  | SMV_Token String
  | SMV_Contract String
  | SMV_Map
  deriving (Eq, Show)

instance Pretty SMTVal where
  pretty = \case
    SMV_Bool b -> pretty $ DLL_Bool b
    SMV_Int i -> pretty i
    SMV_Address p -> "<abstract address" <+> pretty p <> ">"
    SMV_Contract p -> pretty p
    SMV_Digest p -> pretty p
    SMV_Token p -> pretty p
    SMV_Null -> "null"
    SMV_Bytes b -> pretty b
    SMV_Array t xs -> "array" <> parens (hsep $ punctuate comma [pretty t, brackets $ hsep $ punctuate comma $ map pretty xs])
    SMV_Tuple xs -> brackets $ hsep $ punctuate comma $ map pretty xs
    SMV_Object ts -> braces $ hsep $ punctuate comma $ map (\ (k, v) -> pretty k <> ":" <+> pretty v) (M.toAscList ts)
    SMV_Struct ts -> braces $ hsep $ punctuate comma $ map (\ (k, v) -> pretty k <> ":" <+> pretty v) ts
    SMV_Data c xs -> pretty c <> parens (hsep $ punctuate comma $ map pretty xs)
    SMV_Map -> "<map>"

instance Countable SynthExpr where
  counts = \case
    SMTMapNew -> mempty
    SMTMapFresh m -> counts m
    SMTMapSet m f ma -> counts m <> counts f <> counts ma
    SMTMapRef m f -> counts m <> counts f

instance Countable BindingOrigin where
  counts = \case
    O_SwitchCase d -> counts d
    _ -> mempty

instance Countable SMTExpr where
  counts = \case
    SMTModel s -> counts s
    SMTSynth s -> counts s
    SMTProgram de -> counts de

instance AC SMTLet where
  ac = \case
    SMTLet at dv x c se -> do
      x' <- ac_vdef (canDupe se) x
      case (isPure se, x') of
        (_, DLV_Eff) -> return $ SMTNop at
        _ -> do
          ac_visit se
          return $ SMTLet at dv x' c se
    SMTNop at -> return $ SMTNop at
    SMTCon c mv se -> return $ SMTCon c mv se

instance AC SMTTrace where
  ac (SMTTrace lets tk dv) = do
    ac_visit dv
    lets' <- ac $ reverse lets
    return $ SMTTrace (reverse $ filter dropNop lets') tk dv
    where
      dropNop = \case
        SMTNop _ -> False
        _ -> True
