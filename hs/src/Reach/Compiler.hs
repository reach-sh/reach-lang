{-# LANGUAGE OverloadedStrings #-}

module Reach.Compiler where

import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Sequence as S
import Data.Text.Prettyprint.Doc
import System.Exit
import qualified Filesystem.Path.CurrentOS as FP

import Reach.AST
import Reach.Pretty()
import Reach.Parser
import Reach.EmitJS
import Reach.EmitSol
import Reach.VerifyZ3

{- Basic Type checking
 -}

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

{- Inliner

   We remove XL_FunApp and convert XL_If into IF-THEN-ELSE where
   possible.

 -}

type XLFuns ann = M.Map XLVar ([XLVar], XLExpr ann)
type XLIFuns ann = M.Map XLVar (Bool, ([XLVar], XLExpr ann))
type InlineMonad ann a = State (XLFuns ann, XLIFuns ann) (Bool, a)

inline_fun :: XLVar -> InlineMonad ann ([XLVar], XLExpr ann)
inline_fun f = do
  (σi, σo) <- get
  case M.lookup f σo of
    Just v -> return v
    Nothing -> do
      case M.lookup f σi of
        Nothing -> error $ "Inline: Function unbound, or in cycle: " ++ show f
        Just (formals, fun_body) -> do
          let σi' = M.delete f σi
          put (σi', σo)
          (fp, fun_body') <- inline_expr fun_body
          let v = (fp, (formals, fun_body'))
          (σi'', σo') <- get
          let σo'' = M.insert f v σo'
          put (σi'', σo'')
          return v

inline_exprs :: [XLExpr ann] -> InlineMonad ann [XLExpr ann]
inline_exprs es = foldM (\(tp, es') e -> do
                            (ep, e') <- inline_expr e
                            return (tp && ep, e' : es'))
                  (True, []) (reverse es)

inline_expr :: XLExpr ann -> InlineMonad ann (XLExpr ann)
inline_expr e =
  case e of
    XL_Con _ _ -> return (True, e)
    XL_Var _ _ -> return (True, e)
    XL_PrimApp h p es -> inline_exprs es >>= \(ep, es') -> return (ep, XL_PrimApp h p es')
    XL_If h _ ce te fe -> do
      (cp, ce') <- inline_expr ce
      (tp, te') <- inline_expr te
      (fp, fe') <- inline_expr fe
      return (cp && tp && fp, XL_If h (tp && fp) ce' te' fe')
    XL_Claim h ct ae -> do
      (_, ae') <- inline_expr ae
      --- Assert is impure because it could fail
      return (False, XL_Claim h ct ae')
    XL_ToConsensus h p ins pe ce -> do
      (_, pe') <- inline_expr pe
      (_, ce') <- inline_expr ce
      return (False, XL_ToConsensus h p ins pe' ce')
    XL_FromConsensus h be -> do
      (_, be') <- inline_expr be
      return (False, XL_FromConsensus h be')
    XL_Values h es -> inline_exprs es >>= \(ep, es') -> return (ep, XL_Values h es')
    XL_Transfer h to te -> do
      (_, tp') <- inline_expr te
      return (False, XL_Transfer h to tp')
    XL_Declassify h de -> do
      (dp, de') <- inline_expr de
      return (dp, XL_Declassify h de')
    XL_Let h mp mvs ve be -> do
      (vp, ve') <- inline_expr ve
      (bp, be') <- inline_expr be
      return (vp && bp, XL_Let h mp mvs ve' be')
    XL_FunApp h f args -> do
      (arp, args') <- inline_exprs args
      (fp, (formals, fun_body')) <- inline_fun f
      return (arp && fp, XL_Let h Nothing (Just formals) (XL_Values h args') fun_body')
    XL_While h lv ie ce inve be ke -> do
      (_, ie') <- inline_expr ie
      (_, ce') <- inline_expr ce
      (_, inve') <- inline_expr inve
      (_, be') <- inline_expr be
      (_, ke') <- inline_expr ke
      return (False, XL_While h lv ie' ce' inve' be' ke')
    XL_Continue h ne -> do
      (_, ne') <- inline_expr ne
      return (False, XL_Continue h ne')

inline_defs :: [XLDef ann] -> XLFuns ann -> XLExpr ann -> XLExpr ann
inline_defs [] σ me = me'
  where ((_, me'), _) = runState (inline_expr me) (σ, M.empty)
inline_defs (XL_DefineFun _ f args body : ds) σ me = inline_defs ds σ' me
  where σ' = M.insert f (args,body) σ
inline_defs (XL_DefineValues h vs e : ds) σ me = inline_defs ds σ me'
  where me'= XL_Let h Nothing (Just vs) e me

inline :: XLProgram ann -> XLInlinedProgram ann
inline (XL_Prog h defs ps m) = XL_InlinedProg h ps (inline_defs defs M.empty m)

{- ANF

   See AST for a description of the job of this pass.

   The ANF monad stores the next available variable and the list of
   defined variables.

 -}

data ANFElem a
  = ANFExpr a Role ILVar (ILExpr a) 
  | ANFStmt a Role (ILStmt a)
type ANFMonad ann a = State (Int, S.Seq (ANFElem ann)) a

runANF :: ANFMonad ann a -> a
runANF am = if null vs then a else error "ANF: Left variables in state!"
  where
    (a, (_, vs)) = runState am (0, S.Empty)

--- Run an ANF computation, with local new variables, but a global new
--- variable counter.
collectANF :: (ANFElem ann -> a -> a) -> (ANFMonad ann a) -> ANFMonad ann a
collectANF f ma = do
  (v0, vs0) <- get
  let (a, (v1, vs1)) = runState ma (v0, S.Empty)
  put (v1, vs0)
  return (foldr f a vs1)

consumeANF :: String -> ANFMonad ann ILVar
consumeANF s = do
  (nv, vs) <- get
  put (nv+1, vs)
  return (nv, s)

consumeANF_N :: Int -> ANFMonad ann [ILVar]
consumeANF_N 0 = return []
consumeANF_N n = do
  v <- consumeANF ("ANF_N" ++ show n)
  vs <- consumeANF_N (n - 1)
  return $ v : vs

appendANF :: ann -> Role -> ILStmt ann -> ANFMonad ann ()
appendANF h r s = do
  (nvi, vs) <- get
  put (nvi, vs S.|> (ANFStmt h r s))
  return ()

allocANF :: ann -> Role -> String -> ILExpr ann -> ANFMonad ann ILVar
allocANF h r s e = do
  (nvi, vs) <- get
  let nv = (nvi, s)
  put (nvi + 1, vs S.|> (ANFExpr h r nv e))
  return nv

allocANFs :: ann -> Role -> String -> [ILExpr ann] -> ANFMonad ann [ILVar]
allocANFs h mp s es = mapM (allocANF h mp s) es

type XLRenaming ann = M.Map XLVar (ILArg ann)

makeRename :: ann -> XLRenaming ann -> XLVar -> ANFMonad ann (XLRenaming ann, ILVar)
makeRename h ρ v = do
  nv <- consumeANF v
  return (M.insert v (IL_Var h nv) ρ, nv)

anf_parg :: (XLRenaming ann, [(ILVar, BaseType)]) -> (ann, XLVar, BaseType) -> ANFMonad ann (XLRenaming ann, [(ILVar, BaseType)])
anf_parg (ρ, args) (h, v, t) =
  case M.lookup v ρ of
    Nothing -> do
      (ρ', nv) <- makeRename h ρ v
      return (ρ', args' nv)
    Just (IL_Var _ nv) -> return (ρ, args' nv)
    Just _ -> error $ "ANF: Participant argument not bound to variable: " ++ v
  where args' nv = args ++ [(nv,t)]

anf_part :: (XLRenaming ann, ILPartInfo ann) -> (Participant, (ann, [(ann, XLVar, BaseType)])) -> ANFMonad ann (XLRenaming ann, ILPartInfo ann)
anf_part (ρ, ips) (p, (_h, args)) = do
  (ρ', args') <- foldM anf_parg (ρ, []) args
  let ips' = M.insert p args' ips
  return (ρ', ips')

anf_parts :: XLPartInfo ann -> ANFMonad ann (XLRenaming ann, ILPartInfo ann)
anf_parts ps = foldM anf_part (M.empty, M.empty) (M.toList ps)

anf_exprs :: Show ann => Role -> XLRenaming ann -> [XLExpr ann] -> ([ILArg ann] -> ANFMonad ann (Int, (ILTail ann))) -> ANFMonad ann (Int, (ILTail ann))
anf_exprs me ρ es mk =
  case es of
    [] -> mk []
    e : more ->
      anf_expr me ρ e k1
      where k1 [ e' ] = anf_exprs me ρ more k2
              where k2 es' = mk $ e' : es'
            k1 evs = error $ "anf_exprs, expect 1, got " ++ show evs

vsOnly :: [ILArg ann] -> [ILVar]
vsOnly [] = []
vsOnly (IL_Var _ v : m) = v : vsOnly m
vsOnly (_ : m) = vsOnly m

anf_renamed_to :: XLRenaming ann -> XLVar -> ILArg ann
anf_renamed_to ρ v =
  case M.lookup v ρ of
    Nothing -> error ("ANF: Variable unbound: " ++ (show v))
    Just a -> a

anf_expr :: Show ann => Role -> XLRenaming ann -> XLExpr ann -> ([ILArg ann] -> ANFMonad ann (Int, ILTail ann)) -> ANFMonad ann (Int, ILTail ann)
anf_expr me ρ e mk =
  case e of
    XL_Con h b ->
      mk [ IL_Con h b ]
    XL_Var _h v -> mk [ anf_renamed_to ρ v ]
    XL_PrimApp h p args ->
      anf_exprs me ρ args (\args' -> ret_expr h "PrimApp" (IL_PrimApp h p args'))
    XL_If h is_pure ce te fe ->
      anf_expr me ρ ce k
      where k [ ca ] =
              if is_pure then
                anf_expr me ρ te
                  (\ tvs ->
                      anf_expr me ρ fe
                        (\ fvs ->
                           if (length tvs /= length fvs) then
                             error "ANF: If branches don't have same continuation arity"
                           else do
                             ks <- allocANFs h me "PureIf" $ zipWith (\ t f -> IL_PrimApp h (CP IF_THEN_ELSE) [ ca, t, f ]) tvs fvs
                             mk $ map (IL_Var h) ks))
              else do
                (tn, tt) <- anf_tail me ρ te mk
                (fn, ft) <- anf_tail me ρ fe mk
                unless (tn == fn) $ error "ANF: If branches don't have same continuation arity"
                return (tn, IL_If h ca tt ft)
            k _ = error "anf_expr XL_If ce doesn't return 1"
    XL_Claim h ct ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_stmt h (IL_Claim h ct aa))
    XL_FromConsensus h le -> do
      (ln, lt) <- anf_tail RoleContract ρ le mk
      return (ln, IL_FromConsensus h lt)
    XL_ToConsensus h from ins pe ce ->
      anf_expr (RolePart from) ρ pe
      (\ [ pa ] -> do
         let ins' = vsOnly $ map (anf_renamed_to ρ) ins
         (cn, ct) <- anf_tail RoleContract ρ ce mk
         return (cn, IL_ToConsensus h from ins' pa ct))
    XL_Values _h args ->
      anf_exprs me ρ args (\args' -> mk args')
    XL_Transfer h to ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_stmt h (IL_Transfer h to aa))
    XL_Declassify h ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_expr h "Declassify" (IL_Declassify h aa))
    XL_Let _h mwho mvs ve be ->
      anf_expr who ρ ve k
      where who = case mwho of
                    Nothing -> me
                    Just p -> RolePart p
            k nvs = anf_expr me ρ' be mk
              where ρ' = M.union ρvs ρ
                    ρvs = case mvs of
                      Nothing -> ρ
                      Just ovs ->
                        let olen = length ovs
                            nlen = length nvs in
                        if olen == nlen then
                          (M.fromList $ zip ovs nvs)
                        else
                          error $ "ANF XL_Let, context arity mismatch, " ++ show olen ++ " vs " ++ show nlen
    XL_While h loopv inite untile inve bodye ke ->
      anf_expr me ρ inite k
      where k [ inita ] = do
              (ρ', loopv') <- makeRename h ρ loopv
              (untilc, untilt) <- anf_tail me ρ' untile (anf_ktop h)
              error_unless untilc 1 (return ())
              (invc, invt) <- anf_tail me ρ' inve (anf_ktop h)
              error_unless invc 1 (return ())
              (bodyc, bodyt) <- anf_tail me ρ' bodye anf_knocontinue
              error_unless bodyc 0 (return ())
              (kn, kt) <- anf_tail me ρ' ke mk
              return (kn, (IL_While h loopv' inita untilt invt bodyt kt))
            k _ = error $ "XL_While initial expression must return 1"
            anf_knocontinue _ = error $ "ANF XL_While not terminated by XL_Continue"
    XL_Continue h nve -> 
      anf_expr me ρ nve k
      where k [ nva ] = do
              return (0, (IL_Continue h nva))
            k _ = error "anf_expr XL_Continue nve doesn't return 1"
    XL_FunApp _h _ _ -> error $ "ANF XL_FunApp, impossible after inliner"
  where ret_expr h s ne = do
          nv <- allocANF h me s ne
          mk [ IL_Var h nv ]
        ret_stmt h s = do
          appendANF h me s
          mk [ IL_Con h (Con_B True) ]

error_unless :: Eq a => Show a => a -> a -> b -> b
error_unless x y r =
  if x == y then r
  else error $ show x ++ " not equal to " ++ show y

anf_addVar :: ANFElem ann -> (Int, ILTail ann) -> (Int, ILTail ann)
anf_addVar (ANFExpr h mp v e) (c, t) = (c, IL_Let h mp v e t)
anf_addVar (ANFStmt h mp s) (c, t) = (c, IL_Do h mp s t)

anf_tail :: Show ann => Role -> XLRenaming ann -> XLExpr ann -> ([ILArg ann] -> ANFMonad ann (Int, ILTail ann)) -> ANFMonad ann (Int, ILTail ann)
anf_tail me ρ e mk = do
  collectANF anf_addVar (anf_expr me ρ e mk)

anf_ktop :: ann -> [ILArg ann] -> ANFMonad ann (Int, ILTail ann)
anf_ktop h args = return (length args, IL_Ret h args)

anf :: Show ann => XLInlinedProgram ann -> ILProgram ann
anf xilp = IL_Prog h ips xt
  where
    XL_InlinedProg h ps main = xilp
    (ips, xt) = runANF xm
    --- xm :: ANFMonad ann (ILPartInfo ann, ILTail ann)
    xm = do
      (ρ, nps) <- anf_parts ps
      (_, mt) <- anf_tail RoleContract ρ main (anf_ktop h)
      return (nps, mt)

--- End-Point Projection

{-

This stage needs to generate the sub-programs and verify the following
properties:

1. The program is well-typed. (All types can be derived from the types
   of the participant's initial knowledge, so we only require
   annotations on those.) [This implies that the contract has no
   free-variables.]

2. The contract does not execute illegal primitives. The participants
   do not execute transfer.

3. No secret information is shared. (By default, all participants'
   initial knowledge is secret.)

-}

data SecurityLevel
  = Secret
  | Public
  deriving (Show,Eq)

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

instance Monoid SecurityLevel where
  mempty = Public

type SType = (BaseType, SecurityLevel)

type EPPEnv = M.Map Role (M.Map ILVar SType)
type EPPRes ann = (Set.Set BLVar, CTail ann, M.Map Participant (EPTail ann), Int, [CHandler ann])

must_be_public :: (a, SType) -> (a, BaseType)
must_be_public (v, (et, Public)) = (v, et)
must_be_public (_, (_, Secret)) = error "EPP: Must be public"

boundBLVar :: BLVar -> Set.Set BLVar
boundBLVar bv = Set.singleton bv

boundBLVars :: [BLVar] -> Set.Set BLVar
boundBLVars vs = Set.fromList vs

epp_expect :: SType -> (a, SType) -> a
epp_expect est (a, ast) =
  if est == ast then a
  else error $ "EPP: Expected " ++ show est ++ ", got " ++ show ast

epp_var :: String -> EPPEnv -> Role -> ILVar -> (BLVar, SType)
epp_var dbg γ r iv = ((n, s, et), st)
  where (n,s) = iv
        env = case M.lookup r γ of
          Nothing -> error $ "EPP: Unknown role: " ++ show r
          Just v -> v
        (et, _) = st
        st = case M.lookup iv env of
          Nothing -> error $ "EPP: Role " ++ show r ++ " does not know " ++ show iv ++ " at " ++ dbg ++ " but does know " ++ show env
          Just v -> v

epp_vars :: String -> EPPEnv -> Role -> [ILVar] -> [(BLVar, SType)]
epp_vars dbg γ r ivs = map (epp_var dbg γ r) ivs

epp_arg :: String -> EPPEnv -> Role -> ILArg ann -> ((Set.Set BLVar, BLArg ann), SType)
epp_arg _ _ _ (IL_Con h c) = ((Set.empty, BL_Con h c), (conType c, Public))
epp_arg dbg γ r (IL_Var h iv) = ((Set.singleton bv, BL_Var h bv), st)
  where (bv, st) = epp_var dbg γ r iv

epp_args :: String -> EPPEnv -> Role -> [ILArg ann] -> (Set.Set BLVar, [(BLArg ann, SType)])
epp_args dbg γ r ivs = (svs, args)
  where cmb = map (epp_arg dbg γ r) ivs
        svs = Set.unions $ map (\((a,_),_) -> a) cmb
        args = map (\((_,b),c) -> (b,c)) cmb

epp_e_ctc :: Show ann => EPPEnv -> ILExpr ann -> (SType, Set.Set BLVar, CExpr ann)
epp_e_ctc γ e = case e of
  IL_Declassify _ _ -> error "EPP: Contract cannot declassify"
  IL_PrimApp h p@(CP cp) args -> (sRet, fvs, C_PrimApp h cp args')
    where (fvs, args0) = epp_args ("ctc PrimApp " ++ show cp ++ " " ++ show args) γ RoleContract args
          args'st = map must_be_public $ args0
          args' = map fst args'st
          args't = map snd args'st
          ret = checkFun (primType p) args't
          sRet = (ret, Public)
  IL_PrimApp _h p _ -> error $ "EPP: Contract cannot execute: " ++ show p

epp_e_loc :: EPPEnv -> Participant -> ILExpr ann -> (SType, Set.Set BLVar, EPExpr ann)
epp_e_loc γ p e = case e of
  IL_Declassify h a -> ((et, Public), fvs, EP_Arg h a')
    where ((fvs, a'), (et, _)) = earg "loc Declassify" a
  IL_PrimApp h pr args -> ((ret, slvl), fvs, EP_PrimApp h pr args')
    where (fvs, args'st) = epp_args "loc PrimApp" γ (RolePart p) args
          args't = map (fst . snd) args'st
          args' = map fst args'st
          ret = checkFun (primType pr) args't
          slvl = case pr of
                   INTERACT -> Secret
                   _ -> mconcat $ map (snd . snd) args'st
 where earg dbg = epp_arg dbg γ (RolePart p)

epp_s_ctc :: EPPEnv -> ILStmt ann -> (Set.Set BLVar, CStmt ann)
epp_s_ctc γ e = case e of
  IL_Transfer h r am -> (fvs, C_Transfer h r am')
    where (fvs, am') = eargt "ctc Transfer" am AT_UInt256
  IL_Claim h ct a -> (fvs, C_Claim h ct a')
    where (fvs, a') = eargt "ctc Claim" a AT_Bool
 where earg dbg = epp_arg dbg γ RoleContract
       eargt dbg a expected = epp_expect (expected, Public) $ earg dbg a

epp_s_loc :: EPPEnv -> Participant -> ILStmt ann -> (Set.Set BLVar, EPStmt ann)
epp_s_loc γ p e = case e of
  IL_Transfer _ _ _ -> error "EPP: Local cannot transfer"
  IL_Claim h ct a -> case bt of
                   AT_Bool -> (fvs, EP_Claim h ct a')
                   _ -> error "EPP: Assert argument not bool"
    where ((fvs, a'), (bt, _)) = earg "loc Claim" a
          earg dbg = epp_arg dbg γ (RolePart p)

epp_e_ctc2loc :: CExpr ann -> EPExpr ann
epp_e_ctc2loc (C_PrimApp h cp al) = (EP_PrimApp h (CP cp) al)

epp_s_ctc2loc :: CStmt ann -> Maybe (EPStmt ann)
epp_s_ctc2loc (C_Claim h ct a) = Just (EP_Claim h ct a)
epp_s_ctc2loc (C_Transfer _ _ _) = Nothing

il2bl_var :: ILVar -> SType -> BLVar
il2bl_var (n, s) (et, _)  = (n, s, et)

data EPPCtxt ann
  = EC_Top
  | EC_Invariant
  | EC_WhileUntil (Int -> EPPRes ann) (Int -> EPPRes ann)
  | EC_WhileTrial
  | EC_WhileBody Int BaseType (Set.Set BLVar)

epp_it_ctc_do_if :: ann -> [Participant] -> Int -> (EPPEnv, ILArg ann) -> (Int -> EPPRes ann) -> (Int -> EPPRes ann) -> EPPRes ann
epp_it_ctc_do_if h ps hn0 (γc, ca) tres fres =
  (svs, C_If h cca' ctt' cft', ts3, hn2, hs3)
  where (svs_ca, cca') = epp_expect (AT_Bool, Public) $ epp_arg "ctc If cond" γc RoleContract ca
        (svs_t, ctt', ts1, hn1, hs1) = tres hn0
        (svs_f, cft', ts2, hn2, hs2) = fres hn1
        svs = Set.unions [ svs_ca, svs_t, svs_f ]
        hs3 = hs1 ++ hs2
        ts3 = M.fromList $ map mkt ps
        mkt p = (p, EP_If h ca' tt' ft')
          where (_,ca') = epp_expect (AT_Bool, Public) $ epp_arg "ctc If Cond" γc (RolePart p) ca
                tt' = ts1 M.! p
                ft' = ts2 M.! p

epp_it_ctc :: Show ann => [Participant] -> EPPEnv -> Int -> EPPCtxt ann -> ILTail ann -> EPPRes ann
epp_it_ctc ps γ hn0 ctxt it = case it of
  IL_Ret h args ->
    case ctxt of
      EC_WhileUntil kres bres ->
        epp_it_ctc_do_if h ps hn0 (γ, arg0) kres bres
        where [ arg0 ] = args
      EC_Invariant ->
        (mempty, C_Halt h, mempty, hn0, [])
      _ ->
        error "EPP: CTC cannot return"
  IL_If h ca tt ft ->
    epp_it_ctc_do_if h ps hn0 (γ, ca) (dres tt) (dres ft)
    where dres wt hn = epp_it_ctc ps γ hn ctxt wt
  IL_Let h RoleContract what how next -> (svs, C_Let h what' how_ctc next', ts2, hn1, hs1)
    where (svs1, next', ts1, hn1, hs1) = epp_it_ctc ps γ' hn0 ctxt next
          svs = Set.union (Set.difference svs1 (boundBLVar what')) svs_how
          (st, svs_how, how_ctc) = epp_e_ctc γ how
          what' = il2bl_var what st
          what'env = M.singleton what st
          γ' = M.map (M.union what'env) γ
          how_ep = epp_e_ctc2loc how_ctc
          ts2 = M.map (EP_Let h what' how_ep) ts1
  IL_Let _ (RolePart _) _ _ _ ->
    error "EPP: Cannot perform local binding in consensus"
  IL_Do h RoleContract how next -> (svs, ct2, ts2, hn1, hs1)
    where (svs1, ct1, ts1, hn1, hs1) = epp_it_ctc ps γ hn0 ctxt next
          (svs2, how') = epp_s_ctc γ how
          svs = Set.union svs1 svs2
          ct2 = C_Do h how' ct1
          ts2 = case epp_s_ctc2loc how' of
                  Nothing -> ts1
                  Just how'_ep -> M.map (EP_Do h how'_ep) ts1
  IL_Do _ (RolePart _) _ _ ->
    error "EPP: Cannot perform local action in consensus"
  IL_ToConsensus _ _ _ _ _ ->
    error "EPP: Cannot transition to consensus from consensus"
  IL_FromConsensus _ bt -> epp_it_loc ps γ hn0 ctxt bt
  IL_While h loopv inita untilt invt bodyt kt ->
    --- _invt is ignored because we'll verify it later and don't need to run it.
    (svs, ct, ts, hn2, hs)
    where
      which = hn0
      hn1 = hn0 + 1
      nh = C_Loop h svs2l loopv' ct_inv ct1
      hs = nh : hs1      
      svs2l = Set.toList svs2
      svs2 = Set.difference svs1 (boundBLVar loopv')
      svs = Set.union fvs_a svs2
      (_, ct_inv, _, _, _) = epp_it_ctc ps γ' hn1 EC_Invariant invt
      (svs1_trial, _, _, _, _) = epp_it_ctc ps γ' hn1 (EC_WhileUntil kres bres_trial) untilt
      kres_a = epp_it_ctc ps γ' hn1 ctxt kt
      kres hn = if hn == hn1 then kres_a
                else error $ "While Until cannot escape consensus"
      bres_trial hn = epp_it_ctc ps γ' hn EC_WhileTrial bodyt
      (svs1, ct1, ts1, hn2, hs1) = epp_it_ctc ps γ' hn1 (EC_WhileUntil kres bres_real) untilt
      svs1_trial' = Set.difference svs1_trial (boundBLVar loopv')
      bres_real hn = epp_it_ctc ps γ' hn (EC_WhileBody which loopv_ty svs1_trial') bodyt
      ((fvs_a, inita'), st_a) = epp_arg "ctc While init" γ RoleContract inita
      loopv' = il2bl_var loopv st_a
      loopv'env = M.singleton loopv st_a
      loopv_ty = fst st_a
      γ' = M.map (M.union loopv'env) γ
      ts = M.map (EP_Loop h which loopv' inita') ts1
      ct = C_Jump h which svs2l inita'
  IL_Continue h na ->
    case ctxt of
      EC_WhileTrial ->
        (svs, trial "ct", ts, hn, hs)
        where svs = fvs_a
              ((fvs_a, _), _) = epp_arg "ctc continue" γ RoleContract na
              trial msg = error $ "EPP: WhileTrial: Cannot inspect " ++ msg
              ts = M.fromList $ map mkt ps
              mkt p = (p, EP_Continue h 0 $ trial "continue arg")
      EC_WhileBody which loopv_ty fvs_loop ->
        (svs, ct, ts, hn, hs)
        where (fvs_a, inita') = epp_expect (loopv_ty, Public) $ epp_arg "ctc continue" γ RoleContract na
              svs = Set.union fvs_loop fvs_a
              fvs_loopl = Set.toList fvs_loop
              ct = C_Jump h which fvs_loopl inita'
              ts = M.fromList $ map mkt ps
              mkt p = (p, EP_Continue h which $ snd . fst $ epp_arg "ctc continue loc" γ (RolePart p) na)
      _ ->
        error $ "EPP: Continue not in while body"
    where hn = hn0
          hs = []

epp_it_loc :: Show ann => [Participant] -> EPPEnv -> Int -> EPPCtxt ann -> ILTail ann -> EPPRes ann
epp_it_loc ps γ hn0 ctxt it = case it of
  IL_Ret h al -> ( Set.empty, C_Halt h, ts, hn0, [] )
    where ts = M.fromList $ map mkt ps
          mkt p = (p, EP_Ret h $ map fst $ snd $ epp_args "loc ret" γ (RolePart p) al)
  IL_If _ _ _ _ ->
    error "EPP: Ifs must be consensual"
  IL_Let h who what how next -> (svs1, ct1, ts2, hn1, hs1)
    where (svs1, ct1, ts1, hn1, hs1) = epp_it_loc ps γ' hn0 ctxt next
          iv = what
          γ' = M.mapWithKey addwhat γ
          addwhat r env = if role_me r who then
                            M.insert iv lst env
                          else
                            env
          lst = case fmst of
            Nothing -> error "EPP: Let not local to any participant"
            Just v -> v
          (fmst, ts2) = M.foldrWithKey addhow (Nothing, M.empty) ts1
          addhow p t (mst, ts) =
            if not (role_me (RolePart p) who) then
              (mst, M.insert p t ts)
            else
              (mst', M.insert p t' ts)
              where t' = EP_Let h mbv how' t
                    mst' = Just st
                    (st, _, how') = epp_e_loc γ p how
                    (et, _) = st
                    (n,s) = what
                    mbv = (n, s, et)
  IL_Do h who how next -> (svs1, ct1, ts2, hn1, hs1)
    where (svs1, ct1, ts1, hn1, hs1) = epp_it_loc ps γ hn0 ctxt next
          ts2 = M.mapWithKey addhow ts1
          addhow p t =
            if not (role_me (RolePart p) who) then t
            else EP_Do h s' t
            where (_, s') = epp_s_loc γ p how
  IL_ToConsensus h from what howmuch next -> (svs2, ct2, ts2, hn2, hs2)
    where fromr = RolePart from
          what' = map fst $ map must_be_public $ epp_vars "loc toconsensus" γ fromr what
          (_, howmuch') = epp_expect (AT_UInt256, Public) $ epp_arg "loc howmuch" γ fromr howmuch
          what'env = M.fromList $ map (\(n, s, et) -> ((n,s),(et,Public))) what'
          γ' = M.map (M.union what'env) γ
          hn1 = hn0 + 1
          (svs1, ct1, ts1, hn2, hs1) = epp_it_ctc ps γ' hn1 ctxt next
          svs2 = Set.difference svs1 (boundBLVars what')
          svs2l = Set.toList svs2
          nh = C_Handler h from svs2l what' ct1
          hs2 = nh : hs1
          ts2 = M.mapWithKey addTail ts1
          ct2 = C_Wait h hn0 svs2l
          es = EP_Send h hn0 svs2l what' howmuch'
          addTail p pt1 = pt3
            where pt2 me = EP_Recv h me hn0 svs2l what' pt1
                  pt3 = if p /= from then pt2 False
                        else EP_Do h es $ pt2 True
  IL_FromConsensus _ _ ->
    error "EPP: Cannot transition to local from local"
  IL_While _ _ _ _ _ _ _ -> error $ "EPP: While illegal outside consensus"
  IL_Continue _ _ -> error $ "EPP: Continue illegal outside consensus"

epp :: Show ann => ILProgram ann -> BLProgram ann
epp (IL_Prog h ips it) = BL_Prog h bps cp
  where cp = C_Prog h ps chs
        ps = M.keys ips
        bps = M.mapWithKey mkep ets
        mkep p ept = EP_Prog h args ept
          where args = map (\((n, s), et) -> (n,s,et)) $ ips M.! p
        (_, _, ets, _, chs) = epp_it_loc ps γ 0 EC_Top it
        γi = M.fromList $ map initγ $ M.toList ips
        initγ (p, args) = (RolePart p, M.fromList $ map initarg args)
        initarg ((n, s), et) = ((n, s), (et, Secret))
        γ = M.insert RoleContract M.empty γi

data CompilerOpts = CompilerOpts
  { output_dir :: FilePath
  , source :: FilePath }

compile :: CompilerOpts -> IO ()
compile copts = do
  let srcp = source copts
  let out ext = FP.encodeString $ FP.append (FP.decodeString $ output_dir copts) (FP.basename (FP.decodeString srcp) `FP.addExtension` ext)
  xlp <- readReachFile srcp
  writeFile (out "xl") (show (pretty xlp))
  let xilp = inline xlp
  writeFile (out "xil") (show (pretty xilp))
  let ilp = anf xilp
  writeFile (out "il") (show (pretty ilp))
  let blp = epp ilp
  writeFile (out "bl") (show (pretty blp))
  verify_z3 (out "z3") ilp blp
  cs <- compile_sol (out "sol") blp
  writeFile (out "mjs") (show (emit_js blp cs))
  exitSuccess
