{-# LANGUAGE OverloadedStrings #-}

module Reach.Compiler where

import Control.Monad.State.Lazy
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

{- Inliner

   We remove XL_FunApp and convert XL_If into IF-THEN-ELSE where
   possible.

 -}

type XLFuns = M.Map XLVar ([XLVar], XLExpr)
type XLIFuns = M.Map XLVar (Bool, ([XLVar], XLExpr))
type InlineMonad a = State (XLFuns, XLIFuns) (Bool, a)

inline_fun :: XLVar -> InlineMonad ([XLVar], XLExpr)
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

inline_exprs :: [XLExpr] -> InlineMonad [XLExpr]
inline_exprs es = foldM (\(tp, es') e -> do
                            (ep, e') <- inline_expr e
                            return (tp && ep, e' : es'))
                  (True, []) (reverse es)

inline_expr :: XLExpr -> InlineMonad XLExpr
inline_expr e =
  case e of
    XL_Con _ -> return (True, e)
    XL_Var _ -> return (True, e)
    XL_PrimApp p es -> inline_exprs es >>= \(ep, es') -> return (ep, XL_PrimApp p es')
    XL_If _ ce te fe -> do
      (cp, ce') <- inline_expr ce
      (tp, te') <- inline_expr te
      (fp, fe') <- inline_expr fe
      return (cp && tp && fp, XL_If (tp && fp) ce' te' fe')
    XL_Claim ct ae -> do
      (_, ae') <- inline_expr ae
      --- Assert is impure because it could fail
      return (False, XL_Claim ct ae')
    XL_ToConsensus p ins pe ce -> do
      (_, pe') <- inline_expr pe
      (_, ce') <- inline_expr ce
      return (False, XL_ToConsensus p ins pe' ce')
    XL_FromConsensus be -> do
      (_, be') <- inline_expr be
      return (False, XL_FromConsensus be')
    XL_Values es -> inline_exprs es >>= \(ep, es') -> return (ep, XL_Values es')
    XL_Transfer to te -> do
      (_, tp') <- inline_expr te
      return (False, XL_Transfer to tp')
    XL_Declassify de -> do
      (dp, de') <- inline_expr de
      return (dp, XL_Declassify de')
    XL_Let mp mvs ve be -> do
      (vp, ve') <- inline_expr ve
      (bp, be') <- inline_expr be
      return (vp && bp, XL_Let mp mvs ve' be')
    XL_FunApp f args -> do
      (arp, args') <- inline_exprs args
      (fp, (formals, fun_body')) <- inline_fun f
      return (arp && fp, XL_Let Nothing (Just formals) (XL_Values args') fun_body')
    XL_While lv ie ce inve be ke -> do
      (_, ie') <- inline_expr ie
      (_, ce') <- inline_expr ce
      (_, inve') <- inline_expr inve
      (_, be') <- inline_expr be
      (_, ke') <- inline_expr ke
      return (False, XL_While lv ie' ce' inve' be' ke')
    XL_Continue ne -> do
      (_, ne') <- inline_expr ne
      return (False, XL_Continue ne')

inline_defs :: [XLDef] -> XLFuns -> XLExpr -> XLExpr
inline_defs [] σ me = me'
  where ((_, me'), _) = runState (inline_expr me) (σ, M.empty)
inline_defs (XL_DefineFun f args body : ds) σ me = inline_defs ds σ' me
  where σ' = M.insert f (args,body) σ
inline_defs (XL_DefineValues vs e : ds) σ me = inline_defs ds σ me'
  where me'= XL_Let Nothing (Just vs) e me

inline :: XLProgram -> XLInlinedProgram
inline (XL_Prog defs ps m) = XL_InlinedProg ps (inline_defs defs M.empty m)

{- ANF

   See AST for a description of the job of this pass.

   The ANF monad stores the next available variable and the list of
   defined variables.

 -}

data ANFElem
  = ANFExpr Role ILVar ILExpr
  | ANFStmt Role ILStmt
type ANFMonad a = State (Int, S.Seq ANFElem) a

runANF :: ANFMonad a -> a
runANF am = if null vs then a else error "ANF: Left variables in state!"
  where
    (a, (_, vs)) = runState am (0, S.Empty)

--- Run an ANF computation, with local new variables, but a global new
--- variable counter.
collectANF :: (ANFElem -> a -> a) -> (ANFMonad a) -> ANFMonad a
collectANF f ma = do
  (v0, vs0) <- get
  let (a, (v1, vs1)) = runState ma (v0, S.Empty)
  put (v1, vs0)
  return (foldr f a vs1)

consumeANF :: String -> ANFMonad ILVar
consumeANF s = do
  (nv, vs) <- get
  put (nv+1, vs)
  return (nv, s)

consumeANF_N :: Int -> ANFMonad [ILVar]
consumeANF_N 0 = return []
consumeANF_N n = do
  v <- consumeANF ("ANF_N" ++ show n)
  vs <- consumeANF_N (n - 1)
  return $ v : vs

appendANF :: Role -> ILStmt -> ANFMonad ()
appendANF r s = do
  (nvi, vs) <- get
  put (nvi, vs S.|> (ANFStmt r s))
  return ()

allocANF :: Role -> String -> ILExpr -> ANFMonad ILVar
allocANF r s e = do
  (nvi, vs) <- get
  let nv = (nvi, s)
  put (nvi + 1, vs S.|> (ANFExpr r nv e))
  return nv

allocANFs :: Role -> String -> [ILExpr] -> ANFMonad [ILVar]
allocANFs mp s es = mapM (allocANF mp s) es

type XLRenaming = M.Map XLVar ILArg

makeRename :: XLRenaming -> XLVar -> ANFMonad (XLRenaming, ILVar)
makeRename ρ v = do
  nv <- consumeANF v
  return (M.insert v (IL_Var nv) ρ, nv)

anf_parg :: (XLRenaming, [(ILVar, BaseType)]) -> (XLVar, BaseType) -> ANFMonad (XLRenaming, [(ILVar, BaseType)])
anf_parg (ρ, args) (v, t) =
  case M.lookup v ρ of
    Nothing -> do
      (ρ', nv) <- makeRename ρ v
      return (ρ', args' nv)
    Just (IL_Var nv) -> return (ρ, args' nv)
    Just _ -> error $ "ANF: Participant argument not bound to variable: " ++ v
  where args' nv = args ++ [(nv,t)]

anf_part :: (XLRenaming, ILPartInfo) -> (Participant, [(XLVar, BaseType)]) -> ANFMonad (XLRenaming, ILPartInfo)
anf_part (ρ, ips) (p, args) = do
  (ρ', args') <- foldM anf_parg (ρ, []) args
  let ips' = M.insert p args' ips
  return (ρ', ips')

anf_parts :: XLPartInfo -> ANFMonad (XLRenaming, ILPartInfo)
anf_parts ps = foldM anf_part (M.empty, M.empty) (M.toList ps)

anf_exprs :: Role -> XLRenaming -> [XLExpr] -> ([ILArg] -> ANFMonad (Int, ILTail)) -> ANFMonad (Int, ILTail)
anf_exprs me ρ es mk =
  case es of
    [] -> mk []
    e : more ->
      anf_expr me ρ e k1
      where k1 [ e' ] = anf_exprs me ρ more k2
              where k2 es' = mk $ e' : es'
            k1 evs = error $ "anf_exprs, expect 1, got " ++ show evs

vsOnly :: [ILArg] -> [ILVar]
vsOnly [] = []
vsOnly (IL_Var v : m) = v : vsOnly m
vsOnly (_ : m) = vsOnly m

anf_renamed_to :: XLRenaming -> XLVar -> ILArg
anf_renamed_to ρ v =
  case M.lookup v ρ of
    Nothing -> error ("ANF: Variable unbound: " ++ (show v))
    Just a -> a

anf_out_rename :: XLRenaming -> [XLVar] -> ANFMonad (XLRenaming, [ILVar])
anf_out_rename ρ outs = foldM aor1 (ρ, []) (reverse outs)
  where aor1 (ρ', outs') ov =
          case M.lookup ov ρ' of
            Just (IL_Var iv) -> return (ρ', iv:outs')
            Just (IL_Con _) -> return (ρ', outs')
            Nothing -> do
              (ρ'', iv) <- makeRename ρ ov
              return (ρ'', iv:outs')

anf_expr :: Role -> XLRenaming -> XLExpr -> ([ILArg] -> ANFMonad (Int, ILTail)) -> ANFMonad (Int, ILTail)
anf_expr me ρ e mk =
  case e of
    XL_Con b ->
      mk [ IL_Con b ]
    XL_Var v -> mk [ anf_renamed_to ρ v ]
    XL_PrimApp p args ->
      anf_exprs me ρ args (\args' -> ret_expr "PrimApp" (IL_PrimApp p args'))
    XL_If is_pure ce te fe ->
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
                             ks <- allocANFs me "PureIf" $ zipWith (\ t f -> IL_PrimApp (CP IF_THEN_ELSE) [ ca, t, f ]) tvs fvs
                             mk $ map IL_Var ks))
              else do
                (tn, tt) <- anf_tail me ρ te mk
                (fn, ft) <- anf_tail me ρ fe mk
                unless (tn == fn) $ error "ANF: If branches don't have same continuation arity"
                return (tn, IL_If ca tt ft)
            k _ = error "anf_expr XL_If ce doesn't return 1"
    XL_Claim ct ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_stmt (IL_Claim ct aa))
    XL_FromConsensus le -> do
      (ln, lt) <- anf_tail RoleContract ρ le mk
      return (ln, IL_FromConsensus lt)
    XL_ToConsensus from ins pe ce ->
      anf_expr (RolePart from) ρ pe
      (\ [ pa ] -> do
         let ins' = vsOnly $ map (anf_renamed_to ρ) ins
         (cn, ct) <- anf_tail RoleContract ρ ce mk
         return (cn, IL_ToConsensus from ins' pa ct))
    XL_Values args ->
      anf_exprs me ρ args (\args' -> mk args')
    XL_Transfer to ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_stmt (IL_Transfer to aa))
    XL_Declassify ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_expr "Declassify" (IL_Declassify aa))
    XL_Let mwho mvs ve be ->
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
    XL_While loopv inite untile inve bodye ke ->
      anf_expr me ρ inite k
      where k [ inita ] = do
              (ρ', loopv') <- makeRename ρ loopv
              (untilc, untilt) <- anf_tail me ρ' untile anf_ktop
              error_unless untilc 1 (return ())
              (invc, invt) <- anf_tail me ρ' inve anf_ktop
              error_unless invc 1 (return ())
              (bodyc, bodyt) <- anf_tail me ρ' bodye anf_knocontinue
              error_unless bodyc 0 (return ())
              (kn, kt) <- anf_tail me ρ' ke mk
              return (kn, (IL_While loopv' inita untilt invt bodyt kt))
            k _ = error $ "XL_While initial expression must return 1"
            anf_knocontinue _ = error $ "ANF XL_While not terminated by XL_Continue"
    XL_Continue nve -> 
      anf_expr me ρ nve k
      where k [ nva ] = do
              return (0, (IL_Continue nva))
            k _ = error "anf_expr XL_Continue nve doesn't return 1"
    XL_FunApp _ _ -> error $ "ANF XL_FunApp, impossible after inliner"
  where ret_expr s ne = do
          nv <- allocANF me s ne
          mk [ IL_Var nv ]
        ret_stmt s = do
          appendANF me s
          mk [ IL_Con (Con_B True) ]

error_unless :: Eq a => Show a => a -> a -> b -> b
error_unless x y r =
  if x == y then r
  else error $ show x ++ " not equal to " ++ show y

anf_addVar :: ANFElem -> (Int, ILTail) -> (Int, ILTail)
anf_addVar (ANFExpr mp v e) (c, t) = (c, IL_Let mp v e t)
anf_addVar (ANFStmt mp s) (c, t) = (c, IL_Do mp s t)

anf_tail :: Role -> XLRenaming -> XLExpr -> ([ILArg] -> ANFMonad (Int, ILTail)) -> ANFMonad (Int, ILTail)
anf_tail me ρ e mk = do
  collectANF anf_addVar (anf_expr me ρ e mk)

anf_ktop :: [ILArg] -> ANFMonad (Int, ILTail)
anf_ktop args = return (length args, IL_Ret args)

anf :: XLInlinedProgram -> ILProgram
anf xilp = IL_Prog ips xt
  where
    XL_InlinedProg ps main = xilp
    (ips, xt) = runANF xm
    xm :: ANFMonad (ILPartInfo, ILTail)
    xm = do
      (ρ, nps) <- anf_parts ps
      (_, mt) <- anf_tail RoleContract ρ main anf_ktop
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
type EPPRes = (Set.Set BLVar, CTail, M.Map Participant EPTail, Int, [CHandler])

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

epp_arg :: String -> EPPEnv -> Role -> ILArg -> ((Set.Set BLVar, BLArg), SType)
epp_arg _ _ _ (IL_Con c) = ((Set.empty, BL_Con c), (conType c, Public))
epp_arg dbg γ r (IL_Var iv) = ((Set.singleton bv, BL_Var bv), st)
  where (bv, st) = epp_var dbg γ r iv

epp_args :: String -> EPPEnv -> Role -> [ILArg] -> (Set.Set BLVar, [(BLArg, SType)])
epp_args dbg γ r ivs = (svs, args)
  where cmb = map (epp_arg dbg γ r) ivs
        svs = Set.unions $ map (\((a,_),_) -> a) cmb
        args = map (\((_,b),c) -> (b,c)) cmb

epp_e_ctc :: EPPEnv -> ILExpr -> (SType, Set.Set BLVar, CExpr)
epp_e_ctc γ e = case e of
  IL_Declassify _ -> error "EPP: Contract cannot declassify"
  IL_PrimApp p@(CP cp) args -> (sRet, fvs, C_PrimApp cp args')
    where (fvs, args0) = epp_args ("ctc PrimApp " ++ show cp ++ " " ++ show args) γ RoleContract args
          args'st = map must_be_public $ args0
          args' = map fst args'st
          args't = map snd args'st
          ret = checkFun (primType p) args't
          sRet = (ret, Public)
  IL_PrimApp p _ -> error $ "EPP: Contract cannot execute: " ++ show p

epp_e_loc :: EPPEnv -> Participant -> ILExpr -> (SType, Set.Set BLVar, EPExpr)
epp_e_loc γ p e = case e of
  IL_Declassify a -> ((et, Public), fvs, EP_Arg a')
    where ((fvs, a'), (et, _)) = earg "loc Declassify" a
  IL_PrimApp pr args -> ((ret, slvl), fvs, EP_PrimApp pr args')
    where (fvs, args'st) = epp_args "loc PrimApp" γ (RolePart p) args
          args't = map (fst . snd) args'st
          args' = map fst args'st
          ret = checkFun (primType pr) args't
          slvl = case pr of
                   INTERACT -> Secret
                   _ -> mconcat $ map (snd . snd) args'st
 where earg dbg = epp_arg dbg γ (RolePart p)

epp_s_ctc :: EPPEnv -> ILStmt -> (Set.Set BLVar, CStmt)
epp_s_ctc γ e = case e of
  IL_Transfer r am -> (fvs, C_Transfer r am')
    where (fvs, am') = eargt "ctc Transfer" am AT_UInt256
  IL_Claim ct a -> (fvs, C_Claim ct a')
    where (fvs, a') = eargt "ctc Claim" a AT_Bool
 where earg dbg = epp_arg dbg γ RoleContract
       eargt dbg a expected = epp_expect (expected, Public) $ earg dbg a

epp_s_loc :: EPPEnv -> Participant -> ILStmt -> (Set.Set BLVar, EPStmt)
epp_s_loc γ p e = case e of
  IL_Transfer _ _ -> error "EPP: Local cannot transfer"
  IL_Claim ct a -> case bt of
                   AT_Bool -> (fvs, EP_Claim ct a')
                   _ -> error "EPP: Assert argument not bool"
    where ((fvs, a'), (bt, _)) = earg "loc Claim" a
          earg dbg = epp_arg dbg γ (RolePart p)

epp_e_ctc2loc :: CExpr -> EPExpr
epp_e_ctc2loc (C_PrimApp cp al) = (EP_PrimApp (CP cp) al)

epp_s_ctc2loc :: CStmt -> Maybe EPStmt
epp_s_ctc2loc (C_Claim ct a) = Just (EP_Claim ct a)
epp_s_ctc2loc (C_Transfer _ _) = Nothing

il2bl_var :: ILVar -> SType -> BLVar
il2bl_var (n, s) (et, _)  = (n, s, et)

data EPPCtxt
  = EC_Top
  | EC_Invariant
  | EC_WhileUntil (Int -> EPPRes) (Int -> EPPRes)
  | EC_WhileTrial
  | EC_WhileBody Int BaseType (Set.Set BLVar)

epp_it_ctc_do_if :: [Participant] -> Int -> (EPPEnv, ILArg) -> (Int -> EPPRes) -> (Int -> EPPRes) -> EPPRes
epp_it_ctc_do_if ps hn0 (γc, ca) tres fres =
  (svs, C_If cca' ctt' cft', ts3, hn2, hs3)
  where (svs_ca, cca') = epp_expect (AT_Bool, Public) $ epp_arg "ctc If cond" γc RoleContract ca
        (svs_t, ctt', ts1, hn1, hs1) = tres hn0
        (svs_f, cft', ts2, hn2, hs2) = fres hn1
        svs = Set.unions [ svs_ca, svs_t, svs_f ]
        hs3 = hs1 ++ hs2
        ts3 = M.fromList $ map mkt ps
        mkt p = (p, EP_If ca' tt' ft')
          where (_,ca') = epp_expect (AT_Bool, Public) $ epp_arg "ctc If Cond" γc (RolePart p) ca
                tt' = ts1 M.! p
                ft' = ts2 M.! p

epp_it_ctc :: [Participant] -> EPPEnv -> Int -> EPPCtxt -> ILTail -> EPPRes
epp_it_ctc ps γ hn0 ctxt it = case it of
  IL_Ret args ->
    case ctxt of
      EC_WhileUntil kres bres ->
        epp_it_ctc_do_if ps hn0 (γ, arg0) kres bres
        where [ arg0 ] = args
      EC_Invariant ->
        (mempty, C_Halt, mempty, hn0, [])
      _ ->
        error "EPP: CTC cannot return"
  IL_If ca tt ft ->
    epp_it_ctc_do_if ps hn0 (γ, ca) (dres tt) (dres ft)
    where dres wt hn = epp_it_ctc ps γ hn ctxt wt
  IL_Let RoleContract what how next -> (svs, C_Let what' how_ctc next', ts2, hn1, hs1)
    where (svs1, next', ts1, hn1, hs1) = epp_it_ctc ps γ' hn0 ctxt next
          svs = Set.union (Set.difference svs1 (boundBLVar what')) svs_how
          (st, svs_how, how_ctc) = epp_e_ctc γ how
          what' = il2bl_var what st
          what'env = M.singleton what st
          γ' = M.map (M.union what'env) γ
          how_ep = epp_e_ctc2loc how_ctc
          ts2 = M.map (EP_Let what' how_ep) ts1
  IL_Let (RolePart _) _ _ _ ->
    error "EPP: Cannot perform local binding in consensus"
  IL_Do RoleContract how next -> (svs, ct2, ts2, hn1, hs1)
    where (svs1, ct1, ts1, hn1, hs1) = epp_it_ctc ps γ hn0 ctxt next
          (svs2, how') = epp_s_ctc γ how
          svs = Set.union svs1 svs2
          ct2 = C_Do how' ct1
          ts2 = case epp_s_ctc2loc how' of
                  Nothing -> ts1
                  Just how'_ep -> M.map (EP_Do how'_ep) ts1
  IL_Do (RolePart _) _ _ ->
    error "EPP: Cannot perform local action in consensus"
  IL_ToConsensus _ _ _ _ ->
    error "EPP: Cannot transition to consensus from consensus"
  IL_FromConsensus bt -> epp_it_loc ps γ hn0 ctxt bt
  IL_While loopv inita untilt invt bodyt kt ->
    --- _invt is ignored because we'll verify it later and don't need to run it.
    (svs, ct, ts, hn2, hs)
    where
      which = hn0
      hn1 = hn0 + 1
      nh = C_Loop svs2l loopv' ct_inv ct1
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
      ts = M.map (EP_Loop which loopv' inita') ts1
      ct = C_Jump which svs2l inita'
  IL_Continue na ->
    case ctxt of
      EC_WhileTrial ->
        (svs, trial "ct", ts, hn, hs)
        where svs = fvs_a
              ((fvs_a, _), _) = epp_arg "ctc continue" γ RoleContract na
              trial msg = error $ "EPP: WhileTrial: Cannot inspect " ++ msg
              ts = M.fromList $ map mkt ps
              mkt p = (p, EP_Continue 0 $ trial "continue arg")
      EC_WhileBody which loopv_ty fvs_loop ->
        (svs, ct, ts, hn, hs)
        where (fvs_a, inita') = epp_expect (loopv_ty, Public) $ epp_arg "ctc continue" γ RoleContract na
              svs = Set.union fvs_loop fvs_a
              fvs_loopl = Set.toList fvs_loop
              ct = C_Jump which fvs_loopl inita'
              ts = M.fromList $ map mkt ps
              mkt p = (p, EP_Continue which $ snd . fst $ epp_arg "ctc continue loc" γ (RolePart p) na)
      _ ->
        error $ "EPP: Continue not in while body"
    where hn = hn0
          hs = []

epp_it_loc :: [Participant] -> EPPEnv -> Int -> EPPCtxt -> ILTail -> EPPRes
epp_it_loc ps γ hn0 ctxt it = case it of
  IL_Ret al -> ( Set.empty, C_Halt, ts, hn0, [] )
    where ts = M.fromList $ map mkt ps
          mkt p = (p, EP_Ret $ map fst $ snd $ epp_args "loc ret" γ (RolePart p) al)
  IL_If _ _ _ ->
    error "EPP: Ifs must be consensual"
  IL_Let who what how next -> (svs1, ct1, ts2, hn1, hs1)
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
              where t' = EP_Let mbv how' t
                    mst' = Just st
                    (st, _, how') = epp_e_loc γ p how
                    (et, _) = st
                    (n,s) = what
                    mbv = (n, s, et)
  IL_Do who how next -> (svs1, ct1, ts2, hn1, hs1)
    where (svs1, ct1, ts1, hn1, hs1) = epp_it_loc ps γ hn0 ctxt next
          ts2 = M.mapWithKey addhow ts1
          addhow p t =
            if not (role_me (RolePart p) who) then t
            else EP_Do s' t
            where (_, s') = epp_s_loc γ p how
  IL_ToConsensus from what howmuch next -> (svs2, ct2, ts2, hn2, hs2)
    where fromr = RolePart from
          what' = map fst $ map must_be_public $ epp_vars "loc toconsensus" γ fromr what
          (_, howmuch') = epp_expect (AT_UInt256, Public) $ epp_arg "loc howmuch" γ fromr howmuch
          what'env = M.fromList $ map (\(n, s, et) -> ((n,s),(et,Public))) what'
          γ' = M.map (M.union what'env) γ
          hn1 = hn0 + 1
          (svs1, ct1, ts1, hn2, hs1) = epp_it_ctc ps γ' hn1 ctxt next
          svs2 = Set.difference svs1 (boundBLVars what')
          svs2l = Set.toList svs2
          nh = C_Handler from svs2l what' ct1
          hs2 = nh : hs1
          ts2 = M.mapWithKey addTail ts1
          ct2 = C_Wait hn0 svs2l
          es = EP_Send hn0 svs2l what' howmuch'
          addTail p pt1 = pt3
            where pt2 me = EP_Recv me hn0 svs2l what' pt1
                  pt3 = if p /= from then pt2 False
                        else EP_Do es $ pt2 True
  IL_FromConsensus _ ->
    error "EPP: Cannot transition to local from local"
  IL_While _ _ _ _ _ _ -> error $ "EPP: While illegal outside consensus"
  IL_Continue _ -> error $ "EPP: Continue illegal outside consensus"

epp :: ILProgram -> BLProgram
epp (IL_Prog ips it) = BL_Prog bps cp
  where cp = C_Prog ps chs
        ps = M.keys ips
        bps = M.mapWithKey mkep ets
        mkep p ept = EP_Prog args ept
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
