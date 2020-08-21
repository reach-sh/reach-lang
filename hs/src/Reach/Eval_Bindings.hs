module Reach.Eval_Bindings (arrayLiteralBinding, objectLiteralBinding) where

import Control.Monad (zipWithM)
import Control.Monad.ST
import Data.List (foldl')
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
  -> ST s ([String], SLSVal -> ST s (DLStmts, SLEnv))
arrayLiteralBinding vat' at at' ctxt lhs_env xs = do
  let ks = map (jse_expect_id at') $ jsa_flatten xs
  let _make_env (lvl, v) = do
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
  return (ks, _make_env)

objectLiteralBinding
  :: JSExpression
  -> SrcLoc
  -> SrcLoc
  -> SLCtxt s
  -> SLEnv
  -> JSObjectPropertyList
  -> ST s ([String], SLSVal -> ST s (DLStmts, SLEnv))
objectLiteralBinding lhs at _at' _ctxt _lhs_env _props = do
  let ks = ["XXX"] :: [String]
      makeEnv _slsVal = do
        -- XXX
        return ()
  _ <- return (ks, makeEnv :: a -> IO ())

  expect_throw at (Err_DeclLHS_IllegalJS lhs)
