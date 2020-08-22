module Reach.Eval_Bindings (arrayLiteralBinding, objectLiteralBinding) where

import Control.Monad.Except
import Control.Monad.ST
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Stack (CallStack, HasCallStack, callStack)
import Language.JavaScript.Parser.AST
import Reach.AST
import Reach.Eval_Types
import Reach.Eval_Util
import Reach.JSUtil
import Reach.Parser

--- FIXME Support spreads in array literals
arrayLiteralBinding
  :: SrcLoc
  -> SrcLoc
  -> SrcLoc
  -> SLCtxt s
  -> SLEnv
  -> [JSArrayElement]
  -> ([String], SLSVal -> ST s (DLStmts, SLEnv))
arrayLiteralBinding vat' at at' ctxt lhs_env xs = (ks, makeEnv)
  where
    ks = map (jse_expect_id at') $ jsa_flatten xs
    makeEnv (lvl, v) = do
      (vs_lifts, vs) <-
        case v of
          SLV_Tuple _ x -> return (mempty, x)
          SLV_DLVar dv@(DLVar _ _ (T_Tuple ts) _) -> do
            vs_liftsl_and_dvs <- zipWithM mk_ref ts [0 ..]
            let (vs_liftsl, dvs) = unzip vs_liftsl_and_dvs
            let vs_lifts = mconcat vs_liftsl
            return (vs_lifts, dvs)
            where
              mk_ref t i = do
                let e = (DLE_TupleRef vat' (DLA_Var dv) i)
                (dvi, i_lifts) <- ctxt_lift_expr ctxt at (DLVar vat' (ctxt_local_name ctxt "tuple idx") t) e
                return $ (i_lifts, SLV_DLVar dvi)
          SLV_DLVar dv@(DLVar _ _ (T_Array t sz) _) -> do
            vs_liftsl_and_dvs <- mapM mk_ref [0 .. (sz - 1)]
            let (vs_liftsl, dvs) = unzip vs_liftsl_and_dvs
            let vs_lifts = mconcat vs_liftsl
            return (vs_lifts, dvs)
            where
              mk_ref i = do
                let e = (DLE_ArrayRef vat' (ctxt_stack ctxt) (DLA_Var dv) sz (DLA_Con (DLC_Int i)))
                (dvi, i_lifts) <- ctxt_lift_expr ctxt at (DLVar vat' (ctxt_local_name ctxt "array idx") t) e
                return $ (i_lifts, SLV_DLVar dvi)
          _ ->
            expect_throw at' (Err_Decl_NotRefable v)
      let kvs = zipEq at' Err_Decl_WrongArrayLength ks $ map (\x -> (lvl, x)) vs
      return $ (vs_lifts, foldl' (env_insertp at') lhs_env kvs)

-- | Flattens the property list, but it comes out reversed
jso_flatten_rev :: JSObjectPropertyList -> [JSObjectProperty]
jso_flatten_rev = \case
  JSCTLComma jscl _ -> go jscl
  JSCTLNone jscl -> go jscl
  where
    go = \case
      JSLCons props _ prop -> prop : go props
      JSLOne prop -> [prop]
      JSLNil -> []

type ExceptC e = Except (e, CallStack)

throw_error :: HasCallStack => e -> ExceptC e a
throw_error e = throwError (e, callStack)

newtype ShowMe = ShowMe String

instance Show ShowMe where
  show (ShowMe s) = s

runExceptC :: Show e => ExceptC e a -> Either ShowMe a
runExceptC m = case runExcept m of
  -- TODO: display call stack?
  Left (e, _s) -> Left $ ShowMe $ show e -- <> "\n" <> prettyCallStack s
  Right a -> Right a

-- TODO: clearer error message when spread appears at wrong spot

-- | JSObjectSpread is only permitted at the end of the list
-- (But the list comes in reversed)
-- Spits out the idents in correct order, and maybe the spread ident
revParseIdentsAndSpread
  :: HasCallStack => [JSObjectProperty] -> ExceptC ParserError ([String], Maybe String)
revParseIdentsAndSpread [] = pure ([], Nothing)
revParseIdentsAndSpread (prop0 : props0) = case prop0 of
  (JSObjectSpread _ e0) ->
    (,) <$> go props0 <*> goSpread e0
  _ ->
    (\ss -> (ss, Nothing)) <$> go (prop0 : props0)
  where
    go = goProps . reverse
    goProps = mapM jso_expect_id
    goSpread = \case
      (JSIdentifier _ x) -> pure $ Just x
      e -> throw_error $ Err_Parse_ExpectIdentifier e

jso_expect_id :: HasCallStack => JSObjectProperty -> ExceptC ParserError String
jso_expect_id = \case
  JSPropertyIdentRef _ x -> pure x
  j -> throw_error $ Err_Parse_ExpectIdentifierProp j

objectLiteralBinding
  :: JSExpression
  -> SrcLoc
  -> SrcLoc
  -> SLCtxt s
  -> SLEnv
  -> JSObjectPropertyList
  -> ([String], SLSVal -> ST s (DLStmts, SLEnv))
objectLiteralBinding lhs at at' _ctxt _lhs_env props = runE $ do
  (ks, kSpreadMay) <- parseKs
  let ks' = ks <> maybe [] (\a -> [a]) kSpreadMay
  pure (ks', makeEnv ks kSpreadMay)
  where
    -- TODO: propagate Except further up
    runE :: HasCallStack => ExceptC ParserError a -> a
    runE = either (expect_throw at') id . runExceptC

    -- Returns (obj idents, maybe obj spread ident)
    parseKs :: HasCallStack => ExceptC ParserError ([String], Maybe String)
    parseKs = revParseIdentsAndSpread $ jso_flatten_rev props

    -- type DLStmts = Seq.Seq DLStmt
    -- type SLEnv = M.Map SLVar SLSVal
    makeEnv :: [String] -> Maybe String -> SLSVal -> ST s (DLStmts, SLEnv)
    makeEnv ks kSpreadMay (lvl, val) = case val of
      SLV_Object _ env -> pure (mempty, env')
        where
          env' = case kSpreadMay of
            Just spreadName -> env_insert at spreadName (lvl, spreadObj) envWithKs
            Nothing -> envWithKs
          envWithKs = M.restrictKeys env ksSet
          envWithoutKs = M.withoutKeys env ksSet
          ksSet = S.fromList ks
          spreadObj = SLV_Object at envWithoutKs -- TODO: better srcloc?
          -- TODO: DL
      _ -> expect_throw at (Err_DeclLHS_IllegalJS lhs)
