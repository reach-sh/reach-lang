{-# LANGUAGE OverloadedStrings #-}

module Reach.Compiler where

import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Sequence as S
import Text.Pretty.Simple
import qualified Data.Text.Lazy as L
import Data.Text.Prettyprint.Doc
import System.Exit
import qualified Filesystem.Path.CurrentOS as FP
import Data.Monoid

import Reach.AST
import Reach.Pretty()
import Reach.Parser
import Reach.EmitJS
import Reach.EmitSol
import Reach.VerifyZ3

{- -}

zipEq :: [a] -> [b] -> [(a, b)]
zipEq x y = if length x == length y then zip x y
            else error "zipEq: Unequal lists"

zipWithEq :: (a -> b ->c) -> [a] -> [b] -> [c]
zipWithEq f x y = map (\(a,b)->f a b) $ zipEq x y

zipWithEqM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithEqM f x y = if length x == length y then zipWithM f x y
                   else error "zipWithEqM: Unequal lists"

{- Basic Type checking
 -}

type TypeVarEnv = M.Map String BaseType

checkFun :: Show a => a -> FunctionType -> [BaseType] -> BaseType
checkFun h top topdom = toprng
  where
    toprng = case runExcept mrng of
      Left err -> error err
      Right v -> v
    mrng = hFun [] M.empty top topdom
    hTy :: TypeVarEnv -> ExprType -> Except String BaseType
    hTy γ et = case et of
      TY_Con bt -> return bt
      TY_Var v -> case M.lookup v γ of
        Nothing -> throwError $ "checkFun: Unconstrained/bound type variable: " ++ show v ++ " at " ++ show h
        Just et' -> return et'
    hExpr :: [String] -> TypeVarEnv -> ExprType -> BaseType -> Except String TypeVarEnv
    hExpr vs γ et at = case et of
      TY_Con bt ->
        if at == bt then return γ
        else throwError $ "checkFun: Expected " ++ show bt ++ ", got: " ++ show at ++ " at " ++ show h
      TY_Var v ->
        if not $ elem v vs then
          throwError $ "checkFun: Unbound type variable: " ++ show v ++ " at " ++ show h
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
          _ -> throwError $ "checkFun: Received more than expected" ++ " at " ++ show h
      e1 : esl' ->
        case asl of
          [] -> throwError $ "checkFun: Received fewer than expected" ++ " at " ++ show h
          a1 : asl' -> do
            γ' <- (hExprs vs γ esl' asl')
            hExpr vs γ' e1 a1

{- Inliner -}

data InlineV a
  = IV_Con a Constant
  | IV_Values a [InlineV a]
  | IV_Prim a EP_Prim
  | IV_Var a XILVar
  | IV_XIL Bool [BaseType] (XILExpr a)
  | IV_Clo (a, [XLVar], (XLExpr a)) (ILEnv a)

type IVType = [BaseType]

type ILEnv a = M.Map XLVar (InlineV a)

ienv_insert :: Show a => a -> XLVar -> InlineV a -> ILEnv a -> ILEnv a
ienv_insert a x v σ =
  case M.lookup x σ of
    Nothing -> M.insert x v σ
    Just _ ->
      error $ "inline: shadowed binding of " ++ show x ++ " at : " ++ show a

type_count_expect :: Show a => a -> Int -> IVType -> IVType
type_count_expect a cnt t =
  if l == cnt then t
  else error $ "inline: expected " ++ show cnt ++ " values but got " ++ show l ++ " at: " ++ show a
  where l = length t

type_equal :: Show a => a -> IVType -> IVType -> IVType
type_equal a at et =
  if at == et then et
  else error $ "inline: expect " ++ show et ++ " but got " ++ show at ++ " at: " ++ show a

type_all_single :: Show a => a -> [IVType] -> IVType
type_all_single a ts = concat $ map (type_count_expect a 1) ts

iv_single :: Show a => a -> InlineV a -> InlineV a
iv_single a (IV_Values _ vs) = error $ "inline: expected one value but got " ++ show (length vs) ++ " at: " ++ show a
iv_single _ iv = iv

purePrim :: EP_Prim -> Bool
purePrim RANDOM = False
purePrim (CP BALANCE) = False
purePrim (CP TXN_VALUE) = False
purePrim _ = True

iv_expr :: Show a => a -> InlineV a -> (Bool, IVType, XILExpr a)
iv_expr _ (IV_Con a c) = (True, [conType c], (XIL_Con a c))
iv_expr _ (IV_Values a vs) = (vsp, ts, (XIL_Values a es))
  where (vsp, ts, es) = iv_exprs a vs
iv_expr _ (IV_Var a (v,t)) = (True, [t], (XIL_Var a (v,t)))
iv_expr _ (IV_XIL isPure ts x) = (isPure, ts, x)
iv_expr ra (IV_Clo (ca, _, _) _) = error $ "inline: Cannot use lambda " ++ show ca ++ " as expression at: " ++ show ra
iv_expr ra (IV_Prim pa _) = error $ "inline: Cannot use primitive " ++ show pa ++ " as expression at: " ++ show ra

iv_expr_expect :: Show a => a -> [BaseType] -> InlineV a -> (Bool, XILExpr a)
iv_expr_expect ra et iv =
  if et == at then (p, e)
  else error $ "inline: expect type " ++ show et ++ " but got " ++ show at ++ " at: " ++ show ra
  where (p, at, e) = iv_expr ra iv

iv_exprs :: Show a => a -> [InlineV a] -> (Bool, IVType, [XILExpr a])
iv_exprs ra ivs = (iep, its, ies)
  where pies = map (iv_expr ra) ivs
        ies = map (\(_,_,z)->z) pies
        its = type_all_single ra $ map (\(_,y,_)->y) pies
        iep = getAll $ mconcat (map (All . (\(x,_,_)->x)) pies)
        
iv_can_copy :: InlineV a -> Bool
iv_can_copy (IV_XIL _ _ _) = False
iv_can_copy _ = True

id_map :: Show a => a -> [XLVar] -> [BaseType] -> ILEnv a
id_map a vs ts = (M.fromList (zipWithEq iv_id vs ts))
  where iv_id x bt = (x, IV_Var a (x,bt))
  
do_inline_funcall :: Show a => Maybe BaseType -> a -> Maybe Participant -> InlineV a -> [InlineV a] -> InlineV a
do_inline_funcall outer_loopt ch who f argivs =
  case f of
    IV_Con _ _ -> error $ "inline: Cannot call constant as function at: " ++ show ch
    IV_Values _ _ -> error $ "inline: Cannot call values as function at: " ++ show ch
    IV_XIL _ _ _ -> error $ "inline: Cannot call expression as function at: " ++ show ch
    IV_Var _ _ -> error $ "inline: Cannot call variable as function at: " ++ show ch
    IV_Prim _ p ->
      IV_XIL (arp && purePrim p) [ pt ] (XIL_PrimApp ch p pt argies)
      where (arp, argts, argies) = iv_exprs ch argivs
            pt = checkFun ch (primType p) argts
    IV_Clo (lh, formals, orig_body) cloenv ->
      IV_XIL (arp && bp) bt eff_body'
      where (σ', arp, eff_formals, eff_argies) =
              foldr proc_clo_arg (cloenv, True, [], []) $ zipEq formals argivs
            proc_clo_arg (formal, argiv) (i_σ, i_arp, i_eff_formals, i_eff_argies) =
              if (iv_can_copy argiv) then
                (o_σ_copy, i_arp, i_eff_formals, i_eff_argies)
              else
                (o_σ_let, o_arp, o_eff_formals, o_eff_argies)
              where o_σ_copy = ienv_insert lh formal argiv i_σ
                    (this_p, this_ts, this_x) = iv_expr ch argiv
                    [ this_t ] = (type_count_expect ch 1 this_ts)
                    o_σ_let = ienv_insert lh formal (IV_Var ch (formal, this_t)) i_σ
                    o_arp = i_arp && this_p
                    o_eff_formals = (formal, this_t) : i_eff_formals
                    o_eff_argies = this_x : i_eff_argies
            eff_body' = XIL_Let lh who (Just eff_formals) (XIL_Values ch eff_argies) body'
            (bp, bt, body') = iv_expr lh $ peval outer_loopt σ' orig_body

peval :: Show a => Maybe BaseType -> ILEnv a -> XLExpr a -> InlineV a
peval outer_loopt σ e =
  case e of
    XL_Con a c ->
      IV_Con a c
    XL_Var _a v ->
      case M.lookup v σ of
        Nothing -> error $ "inline: Unbound variable: " ++ show e
        Just iv -> iv
    XL_Prim a p ->
      IV_Prim a p
    XL_If a c t f ->
      case peval outer_loopt σ c of
        IV_Con _ (Con_B b) ->
          peval outer_loopt σ (if b then t else f)
        civ ->
          IV_XIL (cp && (tp && fp)) it (XIL_If a (tp && fp) c' it t' f')
          where (cp, c') = iv_expr_expect a [BT_Bool] civ
                (tp, tt, t') = r a t
                (fp, ft, f') = r a f
                it = (type_equal a tt ft)
    XL_Claim a ct ae ->
      --- Claim is impure because it could fail
      IV_XIL False [] (XIL_Claim a ct (sr a [BT_Bool] ae))
    XL_ToConsensus a p vs ae be ->
      IV_XIL False bt (XIL_ToConsensus a p vilvs (sr a [BT_UInt256] ae) be')
      where (_, bt, be') = r a be
            vilvs = zip vs vts
            (_,vts,_) = iv_exprs a $ map def $ map (XL_Var a) vs
    XL_FromConsensus a be ->
      IV_XIL False bt (XIL_FromConsensus a be')
      where (_, bt, be') = r a be
    XL_Values a es ->
      case es of
        [ e1 ] -> def e1
        _ -> IV_Values a (map (iv_single a) $ map def es)
    XL_Transfer a p ae ->
      IV_XIL False [] (XIL_Transfer a p (sr a [BT_UInt256] ae))
    XL_Declassify a de ->
      IV_XIL dp [dt] (XIL_Declassify a dt de')
      where (dp, dts, de') = r a de
            [dt] = (type_count_expect a 1 dts)
    XL_Let a mp mvs ve be ->
      case mvs of
        Just [ v1 ] ->
          do_inline_funcall outer_loopt a mp (IV_Clo (a, [ v1 ], be) σ) [ peval outer_loopt σ ve ]
        _ ->
          IV_XIL (vp && bp) bt (XIL_Let a mp mvs' ve' be')
          where (vp, ts, ve') = r a ve
                (bp, bt, be') = iv_expr a $ peval outer_loopt σ' be
                σ' = M.union σ_new σ
                mvs' = case mvs of
                  Nothing -> Nothing
                  Just vs -> Just (zipEq vs ts)
                σ_new = case mvs of
                  Nothing -> M.empty
                  Just vs -> id_map a vs ts
    XL_While a lv ie ce inve be ke ->
      IV_XIL False ket (XIL_While a (lv, lvt) ie' (sr' [BT_Bool] ce) (sr' [BT_Bool] inve) (sr' [] be) ke')
      where sr' bt x = snd $ iv_expr_expect a bt $ peval (Just lvt) σ' x
            (_, ket, ke') = iv_expr a $ peval outer_loopt σ' ke
            (_, iet, ie') = r a ie
            [lvt] = type_count_expect a 1 iet
            σ' = ienv_insert a lv (IV_Var a (lv, lvt)) σ
    XL_Continue a ne ->
      case outer_loopt of
        Just lvt ->
          IV_XIL False [] (XIL_Continue a (sr a [lvt] ne))
        Nothing ->
          error $ "inline: cannot use continue unless inside loop at: " ++ show a
    XL_Interact a m bt args ->
      IV_XIL False [bt] (XIL_Interact a m bt args')
      where (_, _, args') = iv_exprs a $ map def args
    XL_Lambda a formals body ->
      IV_Clo (a, formals, body) σ
    XL_FunApp a fe es ->
      do_inline_funcall outer_loopt a Nothing (peval outer_loopt σ fe) (map (peval outer_loopt σ) es) 
  where def = peval outer_loopt σ
        r h ne = iv_expr h $ def ne
        sr h bt ne = snd $ iv_expr_expect h bt $ def ne

inline :: Show a => XLProgram a -> XILProgram a
inline (XL_Prog ph defs ps m) = XIL_Prog ph ps' (add_to_m' m')
  where (_, _, m') = iv_expr ph iv
        ps' = M.map (\(prh, vs) -> (prh, map (\(vh,v,vt)->(vh,(v,vt))) vs))  ps
        iv = peval Nothing σ_top_and_ps m
        σ_top_and_ps = M.union σ_ps σ_top
        σ_ps = foldr add_ps M.empty ps
        add_ps (_ph, vs) σ = foldr add_pvs σ vs
        add_pvs (vh, v, bt) σ = ienv_insert vh v (IV_Var vh (v,bt)) σ
        (add_to_m', σ_top) = foldr add_tops ((\x->x), M.empty) defs
        add_tops d (adder, σ) =
          case d of
            XL_DefineValues h vs ve ->
              (adder', M.union (id_map h vs ts) σ)
              where adder' im = XIL_Let h Nothing (Just ivs) ve' (adder im)
                    ivs = zipWithEq (,) vs ts
                    (_, ts, ve') = iv_expr h $ peval Nothing σ ve
            XL_DefineFun h f args body ->
              (adder, ienv_insert h f (IV_Clo (h, args, body) σ_top) σ)

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

consumeANF :: XILVar -> ANFMonad ann ILVar
consumeANF s = do
  (nv, vs) <- get
  put (nv+1, vs)
  return (nv, s)

appendANF :: ann -> Role -> ILStmt ann -> ANFMonad ann ()
appendANF h r s = do
  (nvi, vs) <- get
  put (nvi, vs S.|> (ANFStmt h r s))
  return ()

allocANF :: ann -> Role -> String -> BaseType -> ILExpr ann -> ANFMonad ann ILVar
allocANF h r s t e = do
  (nvi, vs) <- get
  let nv = (nvi, (s, t))
  put (nvi + 1, vs S.|> (ANFExpr h r nv e))
  return nv

allocANFs :: ann -> Role -> String -> [BaseType] -> [ILExpr ann] -> ANFMonad ann [ILVar]
allocANFs h mp s ts es = zipWithEqM (allocANF h mp s) ts es

type XILRenaming ann = M.Map XILVar (ILArg ann)

makeRename :: ann -> XILRenaming ann -> XILVar -> ANFMonad ann (XILRenaming ann, ILVar)
makeRename h ρ v = do
  nv <- consumeANF v
  return (M.insert v (IL_Var h nv) ρ, nv)

anf_parg :: (XILRenaming ann, [ILVar]) -> (ann, XILVar) -> ANFMonad ann (XILRenaming ann, [ILVar])
anf_parg (ρ, args) (h, v) =
  case M.lookup v ρ of
    Nothing -> do
      (ρ', nv) <- makeRename h ρ v
      return (ρ', args' nv)
    Just (IL_Var _ nv) -> return (ρ, args' nv)
    Just _ -> error $ "ANF: Participant argument not bound to variable: " ++ show v
  where args' nv = args ++ [nv]

anf_part :: (XILRenaming ann, ILPartInfo ann) -> (Participant, (ann, [(ann, XILVar)])) -> ANFMonad ann (XILRenaming ann, ILPartInfo ann)
anf_part (ρ, ips) (p, (_h, args)) = do
  (ρ', args') <- foldM anf_parg (ρ, []) args
  let ips' = M.insert p args' ips
  return (ρ', ips')

anf_parts :: XILPartInfo ann -> ANFMonad ann (XILRenaming ann, ILPartInfo ann)
anf_parts ps = foldM anf_part (M.empty, M.empty) (M.toList ps)

anf_exprs :: Show ann => ann -> Role -> XILRenaming ann -> [XILExpr ann] -> (ann -> [ILArg ann] -> ANFMonad ann (Int, (ILTail ann))) -> ANFMonad ann (Int, (ILTail ann))
anf_exprs h0 me ρ es mk =
  case es of
    [] -> mk h0 []
    e : more ->
      anf_expr me ρ e k1
      where k1 h1 [ e' ] = anf_exprs h1 me ρ more k2
              where k2 h2 es' = mk h2 $ e' : es'
            k1 _ evs = error $ "anf_exprs, expect 1, got " ++ show evs

vsOnly :: [ILArg ann] -> [ILVar]
vsOnly [] = []
vsOnly (IL_Var _ v : m) = v : vsOnly m
vsOnly (_ : m) = vsOnly m

anf_renamed_to :: XILRenaming ann -> XILVar -> ILArg ann
anf_renamed_to ρ v =
  case M.lookup v ρ of
    Nothing -> error ("ANF: Variable unbound: " ++ (show v))
    Just a -> a

anf_expr :: Show ann => Role -> XILRenaming ann -> XILExpr ann -> (ann -> [ILArg ann] -> ANFMonad ann (Int, ILTail ann)) -> ANFMonad ann (Int, ILTail ann)
anf_expr me ρ e mk =
  case e of
    XIL_Con h b ->
      mk h [ IL_Con h b ]
    XIL_Var h v -> mk h [ anf_renamed_to ρ v ]
    XIL_PrimApp h p pt args ->
      anf_exprs h me ρ args (\_ args' -> ret_expr h "PrimApp" pt (IL_PrimApp h p args'))
    XIL_If h is_pure ce its te fe ->
      anf_expr me ρ ce k
      where k _ [ ca ] =
              if is_pure then
                anf_expr me ρ te
                  (\ _ tvs ->
                      anf_expr me ρ fe
                        (\ _ fvs -> do
                            ks <- allocANFs h me "PureIf" its $ zipWithEq (\ t f -> IL_PrimApp h (CP IF_THEN_ELSE) [ ca, t, f ]) tvs fvs
                            mk h $ map (IL_Var h) ks))
              else do
                (tn, tt) <- anf_tail me ρ te mk
                (fn, ft) <- anf_tail me ρ fe mk
                unless (tn == fn) $ error "ANF: If branches don't have same continuation arity"
                return (tn, IL_If h ca tt ft)
            k _ _ = error "anf_expr XL_If ce doesn't return 1"
    XIL_Claim h ct ae ->
      anf_expr me ρ ae (\_ [ aa ] -> ret_stmt h (IL_Claim h ct aa))
    XIL_FromConsensus h le -> do
      (ln, lt) <- anf_tail RoleContract ρ le mk
      return (ln, IL_FromConsensus h lt)
    XIL_ToConsensus h from ins pe ce ->
      anf_expr (RolePart from) ρ pe
      (\ _ [ pa ] -> do
         let ins' = vsOnly $ map (anf_renamed_to ρ) ins
         (cn, ct) <- anf_tail RoleContract ρ ce mk
         return (cn, IL_ToConsensus h from ins' pa ct))
    XIL_Values h args ->
      anf_exprs h me ρ args mk
    XIL_Transfer h to ae ->
      anf_expr me ρ ae (\_ [ aa ] -> ret_stmt h (IL_Transfer h to aa))
    XIL_Declassify h dt ae ->
      anf_expr me ρ ae (\_ [ aa ] -> ret_expr h "Declassify" dt (IL_Declassify h aa))
    XIL_Let _h mwho mvs ve be ->
      anf_expr who ρ ve k
      where who = case mwho of
                    Nothing -> me
                    Just p -> RolePart p
            k _ nvs = anf_expr me ρ' be mk
              where ρ' = M.union ρvs ρ
                    ρvs = case mvs of
                      Nothing -> ρ
                      Just ovs -> (M.fromList $ zipEq ovs nvs)
    XIL_While h loopv inite untile inve bodye ke ->
      anf_expr me ρ inite k
      where k _ [ inita ] = do
              (ρ', loopv') <- makeRename h ρ loopv
              (untilc, untilt) <- anf_tail me ρ' untile anf_ktop
              error_unless untilc 1 (return ())
              (invc, invt) <- anf_tail me ρ' inve anf_ktop
              error_unless invc 1 (return ())
              (bodyc, bodyt) <- anf_tail me ρ' bodye anf_knocontinue
              error_unless bodyc 0 (return ())
              (kn, kt) <- anf_tail me ρ' ke mk
              return (kn, (IL_While h loopv' inita untilt invt bodyt kt))
            k _ _ = error $ "XL_While initial expression must return 1"
            anf_knocontinue _ = error $ "ANF XL_While not terminated by XL_Continue"
    XIL_Continue h nve -> 
      anf_expr me ρ nve k
      where k _ [ nva ] = do
              return (0, (IL_Continue h nva))
            k _ _ = error "anf_expr XL_Continue nve doesn't return 1"
    XIL_Interact h m bt args ->
      anf_exprs h me ρ args (\_ args' -> ret_expr h "Interact" bt (IL_Interact h m bt args'))
  where ret_expr h s t ne = do
          nv <- allocANF h me s t ne
          mk h [ IL_Var h nv ]
        ret_stmt h s = do
          appendANF h me s
          mk h [ IL_Con h (Con_B True) ]

error_unless :: Eq a => Show a => a -> a -> b -> b
error_unless x y r =
  if x == y then r
  else error $ show x ++ " not equal to " ++ show y

anf_addVar :: ANFElem ann -> (Int, ILTail ann) -> (Int, ILTail ann)
anf_addVar (ANFExpr h mp v e) (c, t) = (c, IL_Let h mp v e t)
anf_addVar (ANFStmt h mp s) (c, t) = (c, IL_Do h mp s t)

anf_tail :: Show ann => Role -> XILRenaming ann -> XILExpr ann -> (ann -> [ILArg ann] -> ANFMonad ann (Int, ILTail ann)) -> ANFMonad ann (Int, ILTail ann)
anf_tail me ρ e mk = do
  collectANF anf_addVar (anf_expr me ρ e mk)

anf_ktop :: ann -> [ILArg ann] -> ANFMonad ann (Int, ILTail ann)
anf_ktop h args = return (length args, IL_Ret h args)

anf :: Show ann => XILProgram ann -> ILProgram ann
anf xilp = IL_Prog h ips xt
  where
    XIL_Prog h ps main = xilp
    (ips, xt) = runANF xm
    xm = do
      (ρ, nps) <- anf_parts ps
      (_, mt) <- anf_tail RoleContract ρ main anf_ktop
      return (nps, mt)

--- End-Point Projection

{-

This stage needs to generate the sub-programs and verify the following
properties:

1. The contract does not execute illegal primitives. The participants
   do not execute transfer.

2. No secret information is shared. (By default, all participants'
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

type EPPEnv = M.Map Role (M.Map ILVar SecurityLevel)
type EPPMonad ann a = State (Int, M.Map Int (Maybe (CHandler ann))) a
type EPPRes ann = EPPMonad ann (Set.Set BLVar, CTail ann, M.Map Participant (EPTail ann))

runEPP :: EPPMonad ann a -> (a, [CHandler ann])
runEPP am = (a, hs)
  where (a, (_, hs_as_s)) = runState am (0, M.empty)
        hs = map force $ M.toAscList hs_as_s
        force (_, Just h) = h
        force _ = error "EPP: Handler never set!"

localEPP :: EPPMonad ann a -> EPPMonad ann a
localEPP am = do
  (nh, hs) <- get
  let (a, _) = runState am (nh, hs)
  return a

acquireEPP :: EPPMonad ann Int
acquireEPP = do
  (nh, hs) <- get
  put (nh + 1, M.insert nh Nothing hs)
  return nh

setEPP :: Int -> CHandler ann -> EPPMonad ann ()
setEPP which h = do
  (nh, hs) <- get
  let hs' = case M.lookup which hs of
              Nothing ->
                error "EPP: Handler not acquired!"
              Just Nothing ->
                M.insert which (Just h) hs
              Just (Just _) ->
                error "EPP: Handler already set!"
  put (nh, hs')
  return ()

must_be_public :: (a, SecurityLevel) -> a
must_be_public (v, Public) = v
must_be_public (_, Secret) = error "EPP: Must be public"

boundBLVar :: BLVar -> Set.Set BLVar
boundBLVar bv = Set.singleton bv

boundBLVars :: [BLVar] -> Set.Set BLVar
boundBLVars vs = Set.fromList vs

epp_var :: String -> EPPEnv -> Role -> ILVar -> (BLVar, SecurityLevel)
epp_var dbg γ r iv = (iv, st)
  where env = case M.lookup r γ of
          Nothing -> error $ "EPP: Unknown role: " ++ show r
          Just v -> v
        st = case M.lookup iv env of
          Nothing -> error $ "EPP: Role " ++ show r ++ " does not know " ++ show iv ++ " at " ++ dbg ++ " but does know " ++ show env
          Just v -> v

epp_vars :: String -> EPPEnv -> Role -> [ILVar] -> [(BLVar, SecurityLevel)]
epp_vars dbg γ r ivs = map (epp_var dbg γ r) ivs

epp_arg :: String -> EPPEnv -> Role -> ILArg ann -> ((Set.Set BLVar, BLArg ann), SecurityLevel)
epp_arg _ _ _ (IL_Con h c) = ((Set.empty, BL_Con h c), Public)
epp_arg dbg γ r (IL_Var h iv) = ((Set.singleton bv, BL_Var h bv), st)
  where (bv, st) = epp_var dbg γ r iv

epp_args :: String -> EPPEnv -> Role -> [ILArg ann] -> (Set.Set BLVar, [(BLArg ann, SecurityLevel)])
epp_args dbg γ r ivs = (svs, args)
  where cmb = map (epp_arg dbg γ r) ivs
        svs = Set.unions $ map (\((a,_),_) -> a) cmb
        args = map (\((_,b),c) -> (b,c)) cmb

epp_e_ctc :: Show ann => EPPEnv -> ILExpr ann -> (SecurityLevel, Set.Set BLVar, CExpr ann)
epp_e_ctc γ e = case e of
  IL_Declassify _ _ -> error "EPP: Contract cannot declassify"
  IL_PrimApp h (CP cp) args -> (Public, fvs, C_PrimApp h cp args')
    where (fvs, args0) = epp_args ("ctc PrimApp " ++ show cp ++ " " ++ show args) γ RoleContract args
          args' = map must_be_public $ args0
  IL_PrimApp h p _ -> error $ "EPP: Contract cannot execute: " ++ show p ++ " at: " ++ show h
  IL_Interact h _ _ _ -> error $ "EPP: Contract cannot execute interact at: " ++ show h

epp_e_loc :: Show ann => EPPEnv -> Participant -> ILExpr ann -> (SecurityLevel, Set.Set BLVar, EPExpr ann)
epp_e_loc γ p e = case e of
  IL_Declassify h a -> (Public, fvs, EP_Arg h a')
    where ((fvs, a'), _) = earg "loc Declassify" a
  IL_PrimApp h pr args -> (slvl, fvs, EP_PrimApp h pr args')
    where (fvs, args'st) = epp_args "loc PrimApp" γ (RolePart p) args
          args' = map fst args'st
          slvl = mconcat $ map snd args'st
  IL_Interact h m bt args -> (Secret, fvs, EP_Interact h m bt args')
    where (fvs, args'st) = epp_args "loc Interact" γ (RolePart p) args
          args' = map fst args'st
 where earg dbg = epp_arg dbg γ (RolePart p)

epp_s_ctc :: EPPEnv -> ILStmt ann -> (Set.Set BLVar, CStmt ann)
epp_s_ctc γ e = case e of
  IL_Transfer h r am -> (fvs, C_Transfer h r am')
    where (fvs, am') = eargt "ctc Transfer" am
  IL_Claim h ct a -> (fvs, C_Claim h ct a')
    where (fvs, a') = eargt "ctc Claim" a
 where earg dbg = epp_arg dbg γ RoleContract
       eargt dbg a = must_be_public $ earg dbg a

epp_s_loc :: EPPEnv -> Participant -> ILStmt ann -> (Set.Set BLVar, EPStmt ann)
epp_s_loc γ p e = case e of
  IL_Transfer _ _ _ -> error "EPP: Local cannot transfer"
  IL_Claim h ct a -> (fvs, EP_Claim h ct a')
    where ((fvs, a'), _) = earg "loc Claim" a
          earg dbg = epp_arg dbg γ (RolePart p)

epp_e_ctc2loc :: CExpr ann -> EPExpr ann
epp_e_ctc2loc (C_PrimApp h cp al) = (EP_PrimApp h (CP cp) al)

epp_s_ctc2loc :: CStmt ann -> Maybe (EPStmt ann)
epp_s_ctc2loc (C_Claim h ct a) = Just (EP_Claim h ct a)
epp_s_ctc2loc (C_Transfer _ _ _) = Nothing

data EPPCtxt ann
  = EC_Top
  | EC_Invariant
  | EC_WhileUntil (EPPRes ann) (EPPRes ann)
  | EC_WhileTrial
  | EC_WhileBody Int (Set.Set BLVar)

epp_it_ctc_do_if :: ann -> [Participant] -> (EPPEnv, ILArg ann) -> EPPRes ann -> EPPRes ann -> EPPRes ann
epp_it_ctc_do_if h ps (γc, ca) tres fres = do
  let (svs_ca, cca') = must_be_public $ epp_arg "ctc If cond" γc RoleContract ca
  (svs_t, ctt', ts1) <- tres
  (svs_f, cft', ts2) <- fres
  let svs = Set.unions [ svs_ca, svs_t, svs_f ]
  let ts3 = M.fromList $ map mkt ps
            where mkt p = (p, EP_If h ca' tt' ft')
                    where (_,ca') = must_be_public $ epp_arg "ctc If Cond" γc (RolePart p) ca
                          tt' = ts1 M.! p
                          ft' = ts2 M.! p
  return (svs, C_If h cca' ctt' cft', ts3) 

epp_it_ctc :: Show ann => [Participant] -> EPPEnv -> EPPCtxt ann -> ILTail ann -> EPPRes ann
epp_it_ctc ps γ ctxt it = case it of
  IL_Ret h args ->
    case ctxt of
      EC_WhileUntil kres bres -> do
        epp_it_ctc_do_if h ps (γ, arg0) kres bres
        where [ arg0 ] = args
      EC_Invariant -> do
        return (mempty, C_Halt h, mempty)
      _ ->
        error "EPP: CTC cannot return"
  IL_If h ca tt ft -> do
    epp_it_ctc_do_if h ps (γ, ca) (dres tt) (dres ft)
    where dres wt = epp_it_ctc ps γ ctxt wt
  IL_Let h RoleContract what how next -> do
    let (st, svs_how, how_ctc) = epp_e_ctc γ how
    let whatenv = M.singleton what st
    let γ' = M.map (M.union whatenv) γ
    (svs1, next', ts1) <- epp_it_ctc ps γ' ctxt next
    let svs = Set.union (Set.difference svs1 (boundBLVar what)) svs_how
    let how_ep = epp_e_ctc2loc how_ctc
    let ts2 = M.map (EP_Let h what how_ep) ts1
    return (svs, C_Let h what how_ctc next', ts2)
  IL_Let _ (RolePart _) _ _ _ ->
    error "EPP: Cannot perform local binding in consensus"
  IL_Do h RoleContract how next -> do
    let (svs2, how') = epp_s_ctc γ how
    (svs1, ct1, ts1) <- epp_it_ctc ps γ ctxt next
    let svs = Set.union svs1 svs2
    let ct2 = C_Do h how' ct1
    let ts2 = case epp_s_ctc2loc how' of
                Nothing -> ts1
                Just how'_ep -> M.map (EP_Do h how'_ep) ts1
    return (svs, ct2, ts2)
  IL_Do _ (RolePart _) _ _ ->
    error "EPP: Cannot perform local action in consensus"
  IL_ToConsensus _ _ _ _ _ ->
    error "EPP: Cannot transition to consensus from consensus"
  IL_FromConsensus _ bt -> epp_it_loc ps γ ctxt bt
  IL_While h loopv inita untilt invt bodyt kt -> do
    let ((fvs_a, inita'), st_a) = epp_arg "ctc While init" γ RoleContract inita
    let loopvenv = M.singleton loopv st_a
    let γ' = M.map (M.union loopvenv) γ
    (_, ct_inv, _) <- epp_it_ctc ps γ' EC_Invariant invt
    let kres = epp_it_ctc ps γ' ctxt kt
    let bres_trial = epp_it_ctc ps γ' EC_WhileTrial bodyt
    (svs1_trial, _, _) <- localEPP $ epp_it_ctc ps γ' (EC_WhileUntil kres bres_trial) untilt
    let svs1_trial' = Set.difference svs1_trial (boundBLVar loopv)
    which <- acquireEPP
    let bres_real = epp_it_ctc ps γ' (EC_WhileBody which svs1_trial') bodyt
    (svs1, ct1, ts1) <- epp_it_ctc ps γ' (EC_WhileUntil kres bres_real) untilt
    let svs2 = Set.difference svs1 (boundBLVar loopv)
    let svs2l = Set.toList svs2
    setEPP which $ C_Loop h svs2l loopv ct_inv ct1
    let ts = M.map (EP_Loop h which loopv inita') ts1
    let svs = Set.union fvs_a svs2
    let ct = C_Jump h which svs2l inita'
    return (svs, ct, ts)
  IL_Continue h na ->
    case ctxt of
      EC_WhileTrial -> do
        return (svs, trial "ct", ts)
        where svs = fvs_a
              ((fvs_a, _), _) = epp_arg "ctc continue" γ RoleContract na
              trial msg = error $ "EPP: WhileTrial: Cannot inspect " ++ msg
              ts = M.fromList $ map mkt ps
              mkt p = (p, EP_Continue h 0 $ trial "continue arg")
      EC_WhileBody which fvs_loop -> do
        return (svs, ct, ts)
        where (fvs_a, inita') = must_be_public $ epp_arg "ctc continue" γ RoleContract na
              svs = Set.union fvs_loop fvs_a
              fvs_loopl = Set.toList fvs_loop
              ct = C_Jump h which fvs_loopl inita'
              ts = M.fromList $ map mkt ps
              mkt p = (p, EP_Continue h which $ snd . fst $ epp_arg "ctc continue loc" γ (RolePart p) na)
      _ ->
        error $ "EPP: Continue not in while body"

epp_it_loc :: Show ann => [Participant] -> EPPEnv -> EPPCtxt ann -> ILTail ann -> EPPRes ann
epp_it_loc ps γ ctxt it = case it of
  IL_Ret h al -> return ( Set.empty, C_Halt h, ts)
    where ts = M.fromList $ map mkt ps
          mkt p = (p, EP_Ret h $ map fst $ snd $ epp_args "loc ret" γ (RolePart p) al)
  IL_If _ _ _ _ ->
    error "EPP: Ifs must be consensual"
  IL_Let h who what how next -> do
    let (fmst, extend_ts) =
          foldr addhow (Nothing, (\x->x)) ps
          where addhow p (mst, do_extend) =
                  if not (role_me (RolePart p) who) then
                    (mst, do_extend)
                  else
                    (Just st, do_extend')
                  where (st, _, how') = epp_e_loc γ p how
                        do_extend' ts =
                          M.insert p t' ts'
                          where t' = EP_Let h what how' t
                                t = ts' M.! p
                                ts' = do_extend ts
    let γ' = M.mapWithKey addwhat γ
             where addwhat r env = if role_me r who then
                                     M.insert what lst env
                                   else
                                     env
                   lst = case fmst of
                     Nothing -> error "EPP: Let not local to any participant"
                     Just v -> v
    (svs1, ct1, ts1) <- epp_it_loc ps γ' ctxt next
    return (svs1, ct1, extend_ts ts1)
  IL_Do h who how next -> do
    (svs1, ct1, ts1) <- epp_it_loc ps γ ctxt next
    let ts2 = M.mapWithKey addhow ts1
              where addhow p t =
                      if not (role_me (RolePart p) who) then t
                      else EP_Do h s' t
                      where (_, s') = epp_s_loc γ p how
    return (svs1, ct1, ts2)
  IL_ToConsensus h from what howmuch next -> do
    hn0 <- acquireEPP
    let fromr = RolePart from
    let (_, howmuch') = must_be_public $ epp_arg "loc howmuch" γ fromr howmuch
    let what' = map must_be_public $ epp_vars "loc toconsensus" γ fromr what
    let what'env = M.fromList $ map (\v -> (v,Public)) what'
    let γ' = M.map (M.union what'env) γ
    (svs1, ct1, ts1) <- epp_it_ctc ps γ' ctxt next
    let svs2 = Set.difference svs1 (boundBLVars what')
    let svs2l = Set.toList svs2
    setEPP hn0 $ C_Handler h from svs2l what' ct1
    let es = EP_Send h hn0 svs2l what' howmuch'
    let ct2 = C_Wait h hn0 svs2l
    let ts2 = M.mapWithKey addTail ts1
              where addTail p pt1 = pt3
                      where pt2 me = EP_Recv h me hn0 svs2l what' pt1
                            pt3 = if p /= from then pt2 False
                              else EP_Do h es $ pt2 True
    return (svs2, ct2, ts2)
  IL_FromConsensus _ _ -> error "EPP: Cannot transition to local from local"
  IL_While _ _ _ _ _ _ _ -> error $ "EPP: While illegal outside consensus"
  IL_Continue _ _ -> error $ "EPP: Continue illegal outside consensus"

epp :: Show ann => ILProgram ann -> BLProgram ann
epp (IL_Prog h ips it) = BL_Prog h bps cp
  where cp = C_Prog h ps chs
        ps = M.keys ips
        bps = M.mapWithKey mkep ets
        mkep p ept = EP_Prog h (ips M.! p) ept
        ((_, _, ets), chs) = runEPP $ epp_it_loc ps γ EC_Top it
        γi = M.fromList $ map initγ $ M.toList ips
        initγ (p, args) = (RolePart p, M.fromList $ map (\v->(v, Secret)) args)
        γ = M.insert RoleContract M.empty γi

data CompilerOpts = CompilerOpts
  { output_dir :: FilePath
  , source :: FilePath }

compile :: CompilerOpts -> IO ()
compile copts = do
  let srcp = source copts
  let out ext = FP.encodeString $ FP.append (FP.decodeString $ output_dir copts) (FP.basename (FP.decodeString srcp) `FP.addExtension` ext)
  xlp <- readReachFile srcp
  writeFile (out "xl") (L.unpack (pShow xlp))
  let xilp = inline xlp
  writeFile (out "xil") (L.unpack (pShow xilp))
  let ilp = anf xilp
  writeFile (out "il") (show (pretty ilp))
  verify_z3 (out "z3") ilp
  let blp = epp ilp
  writeFile (out "bl") (show (pretty blp))
  cs <- compile_sol (out "sol") blp
  writeFile (out "mjs") (show (emit_js blp cs))
  exitSuccess
