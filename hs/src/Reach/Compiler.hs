module Reach.Compiler where

import Algebra.Lattice
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Bits
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Reach.AST
import Reach.Backend.Go
import Reach.Backend.JS
import Reach.Connector.ALGO
import Reach.Connector.ETH_EVM
import Reach.Connector.ETH_Solidity
import Reach.ConsensusNetworkProgram
import Reach.Parser
import Reach.Pretty ()
import Reach.Util
import Reach.VerifyYices
import Reach.VerifyZ3
import System.Exit
import Test.SmallCheck.Series (Serial)
import Text.Pretty.Simple

{- Err -}

data CompileErr
  = CE_Shadowed
  | CE_VariableNotParticipant
  | CE_UnboundTypeVariable
  | CE_TypeMismatch
  | CE_TypeCount
  | CE_HigherOrder
  | CE_CannotApply
  | CE_UnboundVariable
  | CE_ContinueNotInLoop
  | CE_ContractLimitation
  | CE_ContractReturns
  | CE_LocalLimitation
  | CE_WhileNoContinue
  | CE_UnknownRole
  | CE_ExpectedPublic
  | CE_UnknownVar
  | CE_Unreachable
  | CE_ArrayUnitNotBT
  | CE_ArrayLenNotConstant
  | CE_ExpectArray
  deriving (Generic, Show)

instance Monad m => Serial m CompileErr

expect_throw :: Show a => Show b => CompileErr -> a -> b -> c
expect_throw ce w x = error $ show w ++ ": " ++ msg ++ ": " ++ show x
  where
    msg = case ce of
      CE_Shadowed -> "shadowed (duplicated) binding of variables are disallowed"
      CE_VariableNotParticipant -> "variable used as participant, but not bound to participant"
      --- This is impossible because all of the type definitions
      --- are correct and don't mention unbound variables.
      CE_UnboundTypeVariable -> impossible $ "unbound type variable in primitive type"
      CE_TypeMismatch -> "wrong type"
      CE_TypeCount -> "wrong number of types"
      CE_HigherOrder -> "cannot reference higher-order value"
      CE_CannotApply -> "cannot apply non-higher-order value"
      CE_UnboundVariable -> "unbound variable"
      --- This is impossible, because the decoder checks to make
      --- sure the variables match and it is actually inside a
      --- loop.
      CE_ContinueNotInLoop -> impossible $ "continue not inside loop"
      --- This should be impossible, because there is no terminator
      --- except for return and continue, and the body of a loop
      --- has to return nothing.
      --- XXX Implement a way for while loops to be exited
      CE_WhileNoContinue -> "while does not terminate in continue"
      CE_ContractLimitation -> "contract cannot"
      CE_ContractReturns -> "consensus step not committed"
      CE_LocalLimitation -> "local cannot"
      CE_UnknownRole -> "unknown role"
      CE_ExpectedPublic -> "expected a public value"
      CE_UnknownVar -> "variable not know by role"
      CE_Unreachable -> "Hack: This is used by VerifyZ3"
      CE_ArrayLenNotConstant -> "array length is not constant"
      CE_ArrayUnitNotBT -> "array element is not base type"
      CE_ExpectArray -> "expected an array"

{- -}

map_throw :: Ord k => Show k => Show w => CompileErr -> w -> M.Map k v -> k -> v
map_throw ce w m k =
  case M.lookup k m of
    Just v -> v
    Nothing -> expect_throw ce w k

zipEq :: Show c => CompileErr -> c -> [a] -> [b] -> [(a, b)]
zipEq ce w x y =
  if lx == ly
    then zip x y
    else expect_throw ce w (show lx ++ " vs " ++ show ly)
  where
    lx = length x
    ly = length y

zipWithEq :: Show d => CompileErr -> d -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithEq ce w f x y = map (\(a, b) -> f a b) $ zipEq ce w x y

zipWithEqM :: Monad m => Show d => CompileErr -> d -> (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithEqM ce w f x y =
  if lx == ly
    then zipWithM f x y
    else expect_throw ce w (show lx ++ " vs " ++ show ly)
  where
    lx = length x
    ly = length y

{- Basic Type checking
 -}

type TypeVarEnv = M.Map String LType

checkFun :: Show a => a -> FunctionType -> [LType] -> LType
checkFun h topft topdom = toprng
  where
    toprng = case runExcept mrng of
      Left (ce, x) -> expect_throw ce h x
      Right v -> v
    mrng = hFun [] M.empty topft topdom
    hTy :: TypeVarEnv -> ExprType -> Except (CompileErr, String) LType
    hTy γ et = case et of
      TY_Con bt -> return bt
      TY_Var v -> case M.lookup v γ of
        Nothing -> throwError (CE_UnboundTypeVariable, v)
        Just et' -> return et'
    hExpr :: [String] -> TypeVarEnv -> ExprType -> LType -> Except (CompileErr, String) TypeVarEnv
    hExpr vs γ et at = case et of
      TY_Con bt ->
        if at == bt
          then return γ
          else throwError (CE_TypeMismatch, ("expected " ++ show bt ++ ", but got: " ++ show at))
      TY_Var v ->
        if not $ elem v vs
          then throwError (CE_UnboundTypeVariable, v)
          else case M.lookup v γ of
            Just bt -> hExpr vs γ (TY_Con bt) at
            Nothing -> return $ M.insert v at γ
    hFun :: [String] -> TypeVarEnv -> FunctionType -> [LType] -> Except (CompileErr, String) LType
    hFun vs γ ft adom = case ft of
      TY_Forall nvs ft' -> hFun (vs ++ nvs) γ ft' adom
      TY_Arrow edom rng -> do
        γ' <- hExprs vs γ edom adom
        hTy γ' rng
    hExprs :: [String] -> TypeVarEnv -> [ExprType] -> [LType] -> Except (CompileErr, String) TypeVarEnv
    hExprs vs γ esl asl = case esl of
      [] ->
        case asl of
          [] -> return γ
          _ -> throwError (CE_TypeCount, "too many")
      e1 : esl' ->
        case asl of
          [] -> throwError (CE_TypeCount, "too few")
          a1 : asl' -> do
            γ' <- (hExprs vs γ esl' asl')
            hExpr vs γ' e1 a1

{- Inliner -}

data InlineV a
  = IV_Con a Constant
  | IV_Values a [InlineV a]
  | IV_Prim a EP_Prim
  | IV_Var a XILVar
  | IV_XIL Effects [LType] (XILExpr a)
  | IV_Clo (a, [XLVar], (XLExpr a)) ~(ILEnv a)
  | IV_CloBody a Effects ((XILExpr a) -> (XILExpr a)) (InlineV a)
  deriving ()

type IVType = [LType]

type ILEnv a = M.Map XLVar (InlineV a)

ienv_insert :: Show a => a -> XLVar -> InlineV a -> ILEnv a -> ILEnv a
ienv_insert a x v σ =
  case M.lookup x σ of
    Nothing -> M.insert x v σ
    Just _ ->
      expect_throw CE_Shadowed a x

ienv_check_part_absent :: Show a => a -> XLVar -> ILEnv a -> Bool
ienv_check_part_absent a p σ =
  case M.lookup p σ of
    Nothing -> True
    Just v ->
      case v of
        IV_Var _ (_, LT_BT BT_Address) -> False
        _ -> expect_throw CE_VariableNotParticipant a p

type_count_expect :: Show a => a -> Int -> IVType -> IVType
type_count_expect a cnt t =
  if l == cnt
    then t
    else expect_throw CE_TypeCount a (cnt, l)
  where
    l = length t

type_count_expect_one :: Show a => a -> IVType -> LType
type_count_expect_one a t = case t of
  [lt] -> lt
  _ -> expect_throw CE_TypeCount a (1 :: Int, length t)

type_equal :: Show a => a -> IVType -> IVType -> IVType
type_equal a at et =
  if at == et
    then et
    else expect_throw CE_TypeMismatch a (et, at)

type_all_single :: Show a => a -> [IVType] -> IVType
type_all_single a ts = concat $ map (type_count_expect a 1) ts

type_array_elem :: Show a => a -> IVType -> LType
type_array_elem a lt =
  case (type_count_expect a 1 lt) of
    [LT_FixedArray bt _] -> LT_BT bt
    _ -> expect_throw CE_ExpectArray a lt

iv_single :: Show a => a -> InlineV a -> InlineV a
iv_single a (IV_Values _ vs) = expect_throw CE_TypeCount a (1 :: Integer, (length vs))
iv_single _ iv = iv

purePrim :: EP_Prim -> Effects
purePrim RANDOM = Set.singleton Eff_Comm
purePrim (CP BALANCE) = Set.singleton Eff_Comm
purePrim (CP TXN_VALUE) = Set.singleton Eff_Comm
purePrim _ = Set.empty

iv_expr :: Show a => a -> InlineV a -> (Effects, IVType, XILExpr a)
iv_expr _ (IV_Con a c) = (Set.empty, [LT_BT $ conType c], (XIL_Con a c))
iv_expr _ (IV_Values a vs) = (vsp, ts, (XIL_Values a es))
  where
    (vsp, ts, es) = iv_exprs a vs
iv_expr _ (IV_Var a (v, t)) = (Set.empty, [t], (XIL_Var a (v, t)))
iv_expr _ (IV_XIL isPure ts x) = (isPure, ts, x)
iv_expr ra (IV_Clo (ca, _, _) _) = expect_throw CE_HigherOrder ra ca
iv_expr ra (IV_Prim pa _) = expect_throw CE_HigherOrder ra pa
iv_expr _ (IV_CloBody h arg_ef make_body body_iv) = (arg_ef \/ body_ef, body_t, make_body body_e)
  where
    (body_ef, body_t, body_e) = iv_expr h body_iv

iv_expr_expect :: Show a => a -> [LType] -> InlineV a -> (Effects, XILExpr a)
iv_expr_expect ra et iv =
  if et == at
    then (p, e)
    else expect_throw CE_TypeCount ra (et, at)
  where
    (p, at, e) = iv_expr ra iv

iv_exprs :: Show a => a -> [InlineV a] -> (Effects, IVType, [XILExpr a])
iv_exprs ra ivs = (iep, its, ies)
  where
    pies = map (iv_expr ra) ivs
    ies = map (\(_, _, z) -> z) pies
    its = type_all_single ra $ map (\(_, y, _) -> y) pies
    iep = joins (map (\(x, _, _) -> x) pies)

iv_can_copy :: InlineV a -> Bool
iv_can_copy (IV_XIL _ _ _) = False
iv_can_copy (IV_CloBody _ _ _ _) = False
iv_can_copy _ = True

id_map :: Show a => a -> [XLVar] -> [LType] -> ILEnv a
id_map a vs ts = (M.fromList (zipWithEq CE_TypeCount a iv_id vs ts))
  where
    iv_id x bt = (x, IV_Var a (x, bt))

copy_map :: Show a => a -> [XLVar] -> InlineV a -> ILEnv a
copy_map a vs iv =
  case iv of
    IV_Values _ ivs -> M.fromList (zipEq CE_TypeCount a vs ivs)
    _ -> case vs of
      [v] -> M.singleton v iv
      _ -> expect_throw CE_TypeCount a (1 :: Integer, length vs)

do_static_prim :: a -> EP_Prim -> [InlineV a] -> Maybe (InlineV a)
do_static_prim h p argivs =
  case p of
    --- XXX Perhaps these should be sensitive to bit widths
    CP ADD -> nn2n (+)
    CP SUB -> nn2n (-)
    CP MUL -> nn2n (*)
    CP LSH -> nn2n (\a b -> shift a (fromIntegral b))
    CP RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    CP BAND -> nn2n (.&.)
    CP BIOR -> nn2n (.|.)
    CP BXOR -> nn2n (xor)
    CP PLT -> nn2b (<)
    CP PLE -> nn2b (<=)
    CP PEQ -> nn2b (==)
    CP PGE -> nn2b (>=)
    CP PGT -> nn2b (>)
    _ -> Nothing
  where
    nn2b op =
      case argivs of
        [IV_Con _ (Con_I lhs), IV_Con _ (Con_I rhs)] -> Just $ IV_Con h $ Con_B $ op lhs rhs
        _ -> Nothing
    nn2n op =
      case argivs of
        [IV_Con _ (Con_I lhs), IV_Con _ (Con_I rhs)] -> Just $ IV_Con h $ Con_I $ op lhs rhs
        _ -> Nothing

type LoopTy = (IVType, IVType)

do_inline_funcall :: Show a => Maybe LoopTy -> a -> (Maybe XILVar) -> InlineV a -> [InlineV a] -> InlineV a
do_inline_funcall outer_loopt ch who f argivs =
  case f of
    IV_Con vh _ -> expect_throw CE_CannotApply ch vh
    IV_Values vh _ -> expect_throw CE_CannotApply ch vh
    IV_XIL vh _ _ -> expect_throw CE_CannotApply ch vh
    IV_Var vh _ -> expect_throw CE_CannotApply ch vh
    IV_Prim h p ->
      case do_static_prim h p argivs of
        Just iv -> iv
        Nothing ->
          IV_XIL (arp \/ purePrim p) [pt] (XIL_PrimApp ch p pt argies)
          where
            (arp, argts, argies) = iv_exprs ch argivs
            pt = checkFun ch (primType p) argts
    IV_Clo (lh, formals, orig_body) cloenv ->
      case eff_formals of
        [] -> body_iv
        _ ->
          IV_CloBody lh arp make_eff_body' body_iv
      where
        (σ', arp, eff_formals, eff_argies) =
          foldr proc_clo_arg (cloenv, bottom, [], []) $ zipEq CE_TypeCount ch formals argivs
        proc_clo_arg (formal, argiv) (i_σ, i_arp, i_eff_formals, i_eff_argies) =
          if (iv_can_copy argiv)
            then
              let o_σ_copy = ienv_insert lh formal argiv i_σ
               in (o_σ_copy, i_arp, i_eff_formals, i_eff_argies)
            else
              let (this_p, this_ts, this_x) = iv_expr ch argiv
                  this_t = (type_count_expect_one ch this_ts)
                  o_σ_let = ienv_insert lh formal (IV_Var ch (formal, this_t)) i_σ
                  o_arp = i_arp \/ this_p
                  o_eff_formals = (formal, this_t) : i_eff_formals
                  o_eff_argies = this_x : i_eff_argies
               in (o_σ_let, o_arp, o_eff_formals, o_eff_argies)
        make_eff_body' = XIL_Let lh who (Just eff_formals) (XIL_Values ch eff_argies)
        body_iv = peval outer_loopt σ' orig_body
    IV_CloBody h arg_ef make_body body_iv ->
      IV_CloBody h arg_ef make_body (do_inline_funcall outer_loopt ch who body_iv argivs)

peval_ensure_var :: Show a => a -> LType -> XLVar -> ILEnv a -> XILVar
peval_ensure_var a bt v σ = iv
  where
    iv = case iv_expr_expect a [bt] (peval Nothing σ (XL_Var a v)) of
      (_, XIL_Var _ iv') -> iv'
      _ -> error "Expected XIL_VAR" -- XXX

teval :: Show a => ILEnv a -> XLType a -> LType
teval σ xt =
  case xt of
    XLT_BT _a bt -> LT_BT bt
    XLT_Array a xte unit -> LT_FixedArray bt how_many
      where
        bt = case lte of
          LT_BT x -> x
          _ -> expect_throw CE_ArrayUnitNotBT a lte
        lte = teval σ xte
        how_many =
          case peval Nothing σ unit of
            IV_Con _ (Con_I x) -> x
            _ ->
              expect_throw CE_ArrayLenNotConstant a ("XXX Implement show for part of InlineV" :: String)

peval :: Show a => Maybe LoopTy -> ILEnv a -> XLExpr a -> InlineV a
peval outer_loopt σ e =
  case e of
    XL_Con a c ->
      IV_Con a c
    XL_Var va v ->
      case M.lookup v σ of
        Nothing -> expect_throw CE_UnboundVariable va ("peval" :: String, v)
        Just iv -> iv
    XL_Prim a p ->
      IV_Prim a p
    XL_If a c t f ->
      let (cp, c') = tr a [LT_BT BT_Bool] c
       in case c' of
            (XIL_Con _ (Con_B b)) ->
              peval outer_loopt σ (if b then t else f)
            _ ->
              IV_XIL (cp \/ tfp) it (XIL_If a tfp c' it t' f')
              where
                tfp = (tp \/ fp)
                (tp, tt, t') = r a t
                (fp, ft, f') = r a f
                it = (type_equal a tt ft)
    XL_Claim a ct ae ->
      let ae' = (sr a [LT_BT BT_Bool] ae)
       in IV_XIL eff_claim [] (XIL_Claim a ct ae')
    XL_ToConsensus a (ok_p, vs, ae) mto be ->
      IV_XIL eff_comm rt (XIL_ToConsensus a ok_info mto_info be')
      where
        ok_info = (ok_ij, ok_piv, vilvs, (sr a [LT_BT BT_UInt256] ae))
        (rt, mto_info) =
          case mto of
            Nothing -> (bt, Nothing)
            Just (de, te) -> ((type_equal a tt bt), Just (de', te'))
              where
                de' = sr a [LT_BT BT_UInt256] de
                (_, tt, te') = iv_expr a $ peval outer_loopt σ te
        ok_piv = peval_ensure_var a (LT_BT BT_Address) ok_p σ_ok
        ok_ij = ienv_check_part_absent a ok_p σ
        σ_ok = if ok_ij then ienv_insert a ok_p (IV_Var a (ok_p, (LT_BT BT_Address))) σ else σ
        (_, bt, be') = iv_expr a $ peval outer_loopt σ_ok be
        vilvs = zip vs vts
        (_, vts, _) = iv_exprs a $ map def $ map (XL_Var a) vs
    XL_FromConsensus a be ->
      IV_XIL eff_comm bt (XIL_FromConsensus a be')
      where
        (_, bt, be') = r a be
    XL_Values a es ->
      case es of
        [e1] -> def e1
        _ -> IV_Values a (map (iv_single a) $ map def es)
    XL_Transfer a p ae ->
      IV_XIL eff_comm [] (XIL_Transfer a (peval_ensure_var a (LT_BT BT_Address) p σ) (sr a [LT_BT BT_UInt256] ae))
    XL_Declassify a de ->
      IV_XIL dp [dt] (XIL_Declassify a dt de')
      where
        (dp, dts, de') = r a de
        dt = (type_count_expect_one a dts)
    XL_Let a mp mvs ve be ->
      let mip =
            mp
              >>= ( \p ->
                      Just $
                        if ienv_check_part_absent a p σ
                          then (p, (LT_BT BT_Address))
                          else peval_ensure_var a (LT_BT BT_Address) p σ
                  )
       in case mvs of
            Just [v1] ->
              do_inline_funcall outer_loopt a mip (IV_Clo (a, [v1], be) σ) [peval outer_loopt σ ve]
            _ ->
              IV_XIL (vp \/ bp) bt (XIL_Let a mip mvs' ve' be')
              where
                (vp, ts, ve') = r a ve
                (bp, bt, be') = iv_expr a $ peval outer_loopt σ' be
                σ' = M.union σ_new σ
                mvs' = case mvs of
                  Nothing -> Nothing
                  Just vs -> Just (zipEq CE_TypeCount a vs ts)
                σ_new = case mvs of
                  Nothing -> M.empty
                  Just vs -> id_map a vs ts
    XL_While a lvs ie ce inve be ke ->
      IV_XIL eff_comm (type_equal a bet ket) (XIL_While a lvvs ie' (sr' [LT_BT BT_Bool] ce) (sr' [LT_BT BT_Bool] inve) be' ke')
      where
        sr' bt x = snd $ iv_expr_expect a bt $ peval this_loopt σ' x
        this_loopt = Just (ket, lvts)
        (_, bet, be') = iv_expr a $ peval this_loopt σ' be
        (_, ket, ke') = iv_expr a $ peval outer_loopt σ' ke
        (_, iet, ie') = r a ie
        lvts = type_count_expect a (length lvs) iet
        lvvs = zip lvs lvts
        σ' = foldl' (\σ0 (lv, lvv) -> ienv_insert a lv (IV_Var a lvv) σ0) σ $ zip lvs lvvs
    XL_Continue a ne ->
      case outer_loopt of
        Just (ket, lvts) ->
          IV_XIL eff_comm ket (XIL_Continue a (sr a lvts ne))
        Nothing ->
          expect_throw CE_ContinueNotInLoop a ("XIL" :: String)
    XL_Interact a m xt args ->
      IV_XIL eff_comm [bt] (XIL_Interact a m bt args')
      where
        (_, _, args') = iv_exprs a $ map def args
        bt = teval σ xt
    XL_Lambda a formals body ->
      IV_Clo (a, formals, body) σ
    XL_FunApp a fe es ->
      do_inline_funcall outer_loopt a Nothing (peval outer_loopt σ fe) (map (peval outer_loopt σ) es)
    XL_Digest a args ->
      IV_XIL argsp [LT_BT BT_UInt256] (XIL_Digest a args')
      where
        (argsp, _, args') = iv_exprs a $ map def args
    XL_ArrayRef a ae ee ->
      IV_XIL cp [et] (XIL_ArrayRef a et ae' ee')
      where
        (aep, at, ae') = r a ae
        et = type_array_elem a at
        (eep, ee') = tr a [(LT_BT BT_UInt256)] ee
        cp = aep \/ eep
  where
    def = peval outer_loopt σ
    r h ne = iv_expr h $ def ne
    tr h bt ne = iv_expr_expect h bt $ def ne
    sr h bt ne = snd $ tr h bt ne
    eff_comm = Set.singleton Eff_Comm
    eff_claim = Set.singleton Eff_Claim

inline :: Show a => XLProgram a -> XILProgram a
inline (XL_Prog ph defs ps m) = XIL_Prog ph rts ps' (add_to_m' m')
  where
    (_, rts, m') = iv_expr ph iv
    ps' = M.mapKeys (\p -> (p, (LT_BT BT_Address))) ps
    iv = peval Nothing σ_top m
    (add_to_m', σ_top) = foldl' add_tops ((\x -> x), M.empty) defs
    add_tops (adder, σ) d =
      case d of
        XL_DefineValues h vs ve ->
          if iv_can_copy ve_iv
            then (adder, M.union (copy_map h vs ve_iv) σ)
            else (adder', M.union (id_map h vs ts) σ)
          where
            ve_iv = peval Nothing σ ve
            adder' im =
              XIL_Let h Nothing (Just ivs) ve' (adder im)
            ivs = zipWithEq CE_TypeCount h (,) vs ts
            (_, ts, ve') = iv_expr h ve_iv
        XL_DefineFun h f args body ->
          (adder, ienv_insert h f (IV_Clo (h, args, body) σ_top) σ)

{- ANF

   See AST for a description of the job of this pass.

   The ANF monad stores the next available variable and the list of
   defined variables.

 -}

data ANFElem a
  = ANFExpr a (Role ILVar) ILVar (ILExpr a)
  | ANFStmt a (Role ILVar) (ILStmt a)

type ANFMonad ann a = State (Int, S.Seq (ANFElem ann)) a

runANF :: ANFMonad ann a -> a
runANF am = if null vs then a else impossible "runANF: Variables left in store"
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
  put (nv + 1, vs)
  return (nv, s)

appendANF :: ann -> Role ILVar -> ILStmt ann -> ANFMonad ann ()
appendANF h r s = do
  (nvi, vs) <- get
  put (nvi, vs S.|> (ANFStmt h r s))
  return ()

allocANF :: ann -> Role ILVar -> String -> LType -> ILExpr ann -> ANFMonad ann ILVar
allocANF h r s t e = do
  (nvi, vs) <- get
  let nv = (nvi, (s, t))
  put (nvi + 1, vs S.|> (ANFExpr h r nv e))
  return nv

allocANFs :: Show ann => ann -> Role ILVar -> String -> [LType] -> [ILExpr ann] -> ANFMonad ann [ILVar]
allocANFs h mp s ts es = zipWithEqM (impossible "allocANFs") h (allocANF h mp s) ts es

type XILRenaming ann = M.Map XILVar (ILArg ann)

makeRename :: ann -> XILRenaming ann -> XILVar -> ANFMonad ann (XILRenaming ann, ILVar)
makeRename h ρ v = do
  nv <- consumeANF v
  return (M.insert v (IL_Var h nv) ρ, nv)

makeRenames :: ann -> XILRenaming ann -> [XILVar] -> ANFMonad ann (XILRenaming ann, [ILVar])
makeRenames h ρ vs = case vs of
  [] -> return $ (ρ, [])
  v : more -> do
    (ρ', v') <- makeRename h ρ v
    (ρ'', more') <- makeRenames h ρ' more
    return $ (ρ'', v' : more')

anf_may_rename :: Show ann => ann -> Bool -> XILVar -> XILRenaming ann -> ANFMonad ann (XILRenaming ann, ILVar)
anf_may_rename h ij fromv ρ =
  case ij of
    False -> return (ρ, anf_renamed_to_var h ρ fromv)
    True -> makeRename h ρ fromv

anf_parg :: Show ann => (XILRenaming ann, [ILVar]) -> (ann, XILVar) -> ANFMonad ann (XILRenaming ann, [ILVar])
anf_parg (ρ, args) (h, v) =
  case M.lookup v ρ of
    Nothing -> do
      (ρ', nv) <- makeRename h ρ v
      return (ρ', args' nv)
    Just (IL_Var _ nv) -> return (ρ, args' nv)
    Just _ -> expect_throw CE_UnknownRole h v
  where
    args' nv = args ++ [nv]

anf_part :: (XILRenaming ann, ILPartInfo ann) -> (XILVar, ann) -> ANFMonad ann (XILRenaming ann, ILPartInfo ann)
anf_part (ρ, ips) (p, h) = do
  (ρ', p') <- makeRename h ρ p
  return (ρ', Set.insert p' ips)

anf_parts :: XILPartInfo ann -> ANFMonad ann (XILRenaming ann, ILPartInfo ann)
anf_parts ps = foldM anf_part (M.empty, Set.empty) (M.toList ps)

anf_exprs :: Show ann => ann -> Role ILVar -> XILRenaming ann -> [XILExpr ann] -> (ann -> [ILArg ann] -> ANFMonad ann (ILTail ann)) -> ANFMonad ann (ILTail ann)
anf_exprs h0 me ρ es mk =
  case es of
    [] -> mk h0 []
    e : more ->
      anf_expr me ρ e k1
      where
        k1 h1 [e'] = anf_exprs h1 me ρ more k2
          where
            k2 h2 es' = mk h2 $ e' : es'
        k1 h1 evs = expect_throw CE_TypeCount h1 (h0, length evs)

vsOnly :: [ILArg ann] -> [ILVar]
vsOnly [] = []
vsOnly (IL_Var _ v : m) = v : vsOnly m
vsOnly (_ : m) = vsOnly m

anf_renamed_to :: Show ann => ann -> XILRenaming ann -> XILVar -> ILArg ann
anf_renamed_to h ρ v =
  case M.lookup v ρ of
    Nothing -> expect_throw CE_UnboundVariable h ("anf" :: String, v)
    Just a -> a

anf_renamed_to_var :: Show ann => ann -> XILRenaming ann -> XILVar -> ILVar
anf_renamed_to_var h ρ v =
  case anf_renamed_to h ρ v of
    IL_Var _ nv -> nv
    _ -> expect_throw CE_VariableNotParticipant h v

anf_expr :: Show ann => Role ILVar -> XILRenaming ann -> XILExpr ann -> (ann -> [ILArg ann] -> ANFMonad ann (ILTail ann)) -> ANFMonad ann (ILTail ann)
anf_expr me ρ e mk =
  case e of
    XIL_Con h b ->
      mk h [IL_Con h b]
    XIL_Var h v -> mk h [anf_renamed_to h ρ v]
    XIL_PrimApp h p pt args ->
      anf_exprs h me ρ args (\_ args' -> ret_expr h "PrimApp" pt (IL_PrimApp h p args'))
    XIL_If h effs ce its te fe ->
      anf_expr me ρ ce k
      where
        k _ [ca] =
          case Set.null effs of
            True ->
              anf_expr
                me
                ρ
                te
                ( \_ tvs ->
                    anf_expr
                      me
                      ρ
                      fe
                      ( \_ fvs -> do
                          ks <- allocANFs h me "PureIf" its $ zipWithEq CE_TypeCount h (\t f -> IL_PrimApp h (CP IF_THEN_ELSE) [ca, t, f]) tvs fvs
                          mk h $ map (IL_Var h) ks
                      )
                )
            False -> comm_case
          where
            comm_case = do
              tt <- anf_tail me ρ te mk
              ft <- anf_tail me ρ fe mk
              return $ IL_If h ca tt ft
        k _ es = expect_throw CE_TypeCount h (1 :: Integer, (length es))
    XIL_Claim h ct ae ->
      anf_expr me ρ ae $ \_ aas -> case aas of
        [aa] -> ret_stmt h (IL_Claim h ct aa)
        _ -> error "Expected exactly one ILArg" -- XXX
    XIL_FromConsensus h le -> do
      lt <- anf_tail RoleContract ρ le mk
      return $ IL_FromConsensus h lt
    XIL_ToConsensus h (ok_ij, from, ins, pe) mto ce -> do
      let from' = anf_renamed_to_var h ρ from
      anf_expr
        (RolePart from')
        ρ
        pe
        ( \_ pas -> case pas of
            [pa] -> do
              let ins' = vsOnly $ map (anf_renamed_to h ρ) ins
              ct <- anf_tail RoleContract ρ ce mk
              let done mto' = return $ IL_ToConsensus h (ok_ij, from', ins', pa) mto' ct
              case mto of
                Nothing -> done Nothing
                Just (de, te) ->
                  anf_expr
                    RoleContract
                    ρ
                    de
                    ( \_ das -> case das of
                        [da] -> do
                          tt <- anf_tail RoleContract ρ te anf_ktop
                          done $ Just (da, tt)
                        _ -> error "Expected [ILArg] to have exactly one element" -- XXX
                    )
            _ -> error "Expected [ILArg] to have exactly one element" -- XXX
        )
    XIL_Values h args ->
      anf_exprs h me ρ args mk
    XIL_Transfer h to ae ->
      anf_expr
        me
        ρ
        ae
        ( \_ aas -> case aas of
            [aa] -> case map_throw CE_UnknownRole h ρ to of
              (IL_Var _ tov) -> ret_stmt h (IL_Transfer h tov aa)
              _ -> error "Expected an IL_Var" -- XXX
            _ -> error "Expected [ILArg] to have exactly one element" -- XXX
        )
    XIL_Declassify h dt ae ->
      anf_expr
        me
        ρ
        ae
        ( \_ aas -> case aas of
            [aa] -> ret_expr h "Declassify" dt (IL_Declassify h aa)
            _ -> error "Expected [ILArg] to have exactly one element" -- XXX
        )
    XIL_Let h mwho mvs ve be ->
      anf_expr who ρ ve k
      where
        who = case mwho of
          Nothing -> me
          Just p -> RolePart $ anf_renamed_to_var h ρ p
        k _ nvs = anf_expr me ρ' be mk
          where
            ρ' = M.union ρvs ρ
            ρvs = case mvs of
              Nothing -> ρ
              Just ovs -> (M.fromList $ zipEq CE_TypeCount h ovs nvs)
    XIL_While h loopvs inite untile inve bodye ke ->
      anf_expr me ρ inite k
      where
        k _ initas = do
          (ρ', loopvs') <- makeRenames h ρ loopvs
          untilt <- anf_tail me ρ' untile anf_ktop
          invt <- anf_tail me ρ' inve anf_ktop
          bodyt <- anf_tail me ρ' bodye anf_knocontinue
          kt <- anf_tail me ρ' ke mk
          return $ IL_While h loopvs' initas untilt invt bodyt kt
        anf_knocontinue kh _ = expect_throw CE_WhileNoContinue kh h
    XIL_Continue h nve ->
      anf_expr me ρ nve (\_ nvas -> return $ IL_Continue h nvas)
    XIL_Interact h m bt args ->
      anf_exprs h me ρ args (\_ args' -> ret_expr h "Interact" bt (IL_Interact h m bt args'))
    XIL_Digest h args ->
      anf_exprs h me ρ args (\_ args' -> ret_expr h "Digest" (LT_BT BT_UInt256) (IL_Digest h args'))
    XIL_ArrayRef h bt ae ee ->
      anf_exprs
        h
        me
        ρ
        [ae, ee]
        ( \_ ae'_ee' -> case ae'_ee' of
            [ae', ee'] -> ret_expr h "ArrayRef" bt (IL_ArrayRef h ae' ee')
            _ -> error "Expected [ILArg] to have exactly 2 elements" -- XXX
        )
  where
    ret_expr h s t ne = do
      nv <- allocANF h me s t ne
      mk h [IL_Var h nv]
    ret_stmt h s = do
      appendANF h me s
      mk h [IL_Con h (Con_B True)]

anf_addVar :: ANFElem ann -> ILTail ann -> ILTail ann
anf_addVar (ANFExpr h mp v e) t = IL_Let h mp v e t
anf_addVar (ANFStmt h mp s) t = IL_Do h mp s t

anf_tail :: Show ann => Role ILVar -> XILRenaming ann -> XILExpr ann -> (ann -> [ILArg ann] -> ANFMonad ann (ILTail ann)) -> ANFMonad ann (ILTail ann)
anf_tail me ρ e mk = do
  collectANF anf_addVar (anf_expr me ρ e mk)

anf_ktop :: ann -> [ILArg ann] -> ANFMonad ann (ILTail ann)
anf_ktop h args = return $ IL_Ret h args

anf :: Show ann => XILProgram ann -> ILProgram ann
anf xilp = IL_Prog h rt ips xt
  where
    XIL_Prog h rt ps main = xilp
    (ips, xt) = runANF xm
    xm = do
      (ρ, nps) <- anf_parts ps
      mt <- anf_tail RoleContract ρ main anf_ktop
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
  deriving (Show, Eq)

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

instance Monoid SecurityLevel where
  mempty = Public

type EPPEnv = M.Map (Role BLVar) (M.Map ILVar SecurityLevel)

type EPPMonad ann a = State (Int, M.Map Int (Maybe (CHandler ann))) a

type EPPRes ann = EPPMonad ann (Set.Set BLVar, CTail ann, M.Map BLVar (EPTail ann))

runEPP :: EPPMonad ann a -> (a, [CHandler ann])
runEPP am = (a, hs)
  where
    (a, (_, hs_as_s)) = runState am (1, M.empty)
    hs = map force $ M.toAscList hs_as_s
    force (_, Just h) = h
    force _ = impossible "EPP: Handler never set!"

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

setEPP :: Int -> (Int -> CHandler ann) -> EPPMonad ann ()
setEPP which mh = do
  (nh, hs) <- get
  let hs' = case M.lookup which hs of
        Nothing ->
          impossible "EPP: Handler not acquired!"
        Just Nothing ->
          M.insert which (Just $ mh which) hs
        Just (Just _) ->
          impossible "EPP: Handler already set!"
  put (nh, hs')
  return ()

must_be_public :: Show h => Show v => h -> (v, SecurityLevel) -> v
must_be_public _ (v, Public) = v
must_be_public h (v, Secret) = expect_throw CE_ExpectedPublic h v

boundBLVar :: BLVar -> Set.Set BLVar
boundBLVar bv = Set.singleton bv

boundBLVars :: [BLVar] -> Set.Set BLVar
boundBLVars vs = Set.fromList vs

epp_var :: Show ann => ann -> EPPEnv -> Role BLVar -> ILVar -> (BLVar, SecurityLevel)
epp_var h γ r iv = (iv, st)
  where
    env = case M.lookup r γ of
      Nothing -> expect_throw CE_UnknownRole h r
      Just v -> v
    st = case M.lookup iv env of
      Nothing -> expect_throw CE_UnknownVar h (r, iv)
      Just v -> v

epp_vars :: Show ann => ann -> EPPEnv -> Role BLVar -> [ILVar] -> [(BLVar, SecurityLevel)]
epp_vars h γ r ivs = map (epp_var h γ r) ivs

epp_arg :: Show ann => ann -> EPPEnv -> Role BLVar -> ILArg ann -> ((Set.Set BLVar, BLArg ann), SecurityLevel)
epp_arg _ _ _ (IL_Con h c) = ((Set.empty, BL_Con h c), Public)
epp_arg _ γ r (IL_Var h iv) = ((Set.singleton bv, BL_Var h bv), st)
  where
    (bv, st) = epp_var h γ r iv

epp_args :: Show ann => ann -> EPPEnv -> Role BLVar -> [ILArg ann] -> (Set.Set BLVar, [(BLArg ann, SecurityLevel)])
epp_args h γ r ivs = (svs, args)
  where
    cmb = map (epp_arg h γ r) ivs
    svs = Set.unions $ map (\((a, _), _) -> a) cmb
    args = map (\((_, b), c) -> (b, c)) cmb

epp_e_ctc :: Show ann => EPPEnv -> ILExpr ann -> (SecurityLevel, Set.Set BLVar, CExpr ann)
epp_e_ctc γ e = case e of
  IL_Declassify h _ -> expect_throw CE_ContractLimitation h ("declassify" :: String)
  IL_PrimApp h (CP cp) args -> (Public, fvs, C_PrimApp h cp args')
    where
      (fvs, args') = args_help h args
  IL_PrimApp h p _ -> expect_throw CE_ContractLimitation h p
  IL_Interact h _ _ _ -> expect_throw CE_ContractLimitation h ("interact" :: String)
  IL_Digest h args -> (Public, fvs, C_Digest h args')
    where
      (fvs, args') = args_help h args
  IL_ArrayRef h ae ee -> (Public, fvs, C_ArrayRef h ae' ee')
    where
      (fvs, ae', ee') = case args_help h [ae, ee] of
        (fvs_, [ae'_, ee'_]) -> (fvs_, ae'_, ee'_)
        _ -> error "Expected list to have exactly 2 elements" -- XXX
  where
    args_help h args = (fvs, args')
      where
        (fvs, args0) = epp_args h γ RoleContract args
        args' = map (must_be_public h) $ args0

epp_e_loc :: Show ann => EPPEnv -> ILVar -> ILExpr ann -> (SecurityLevel, Set.Set BLVar, EPExpr ann)
epp_e_loc γ p e = case e of
  IL_Declassify h a -> (Public, fvs, EP_Arg h a')
    where
      ((fvs, a'), _) = earg h a
  IL_PrimApp h pr args -> (slvl, fvs, EP_PrimApp h pr args')
    where
      (slvl, fvs, args') = args_help h args
  IL_Interact h m bt args -> (Secret, fvs, EP_Interact h m bt args')
    where
      (_, fvs, args') = args_help h args
  IL_Digest h args -> (slvl, fvs, EP_Digest h args')
    where
      (slvl, fvs, args') = args_help h args
  IL_ArrayRef h ae ee -> (slvl, fvs, EP_ArrayRef h ae' ee')
    where
      (slvl, fvs, ae', ee') = case args_help h [ae, ee] of
        (slvl_, fvs_, [ae'_, ee'_]) -> (slvl_, fvs_, ae'_, ee'_)
        _ -> error "Expected list to have exactly 2 elements" -- XXX
  where
    earg h = epp_arg h γ (RolePart p)
    args_help h args = (slvl, fvs, args')
      where
        (fvs, args'st) = epp_args h γ (RolePart p) args
        args' = map fst args'st
        slvl = mconcat $ map snd args'st

epp_s_ctc :: Show ann => EPPEnv -> ILStmt ann -> (Set.Set BLVar, CStmt ann)
epp_s_ctc γ e = case e of
  IL_Transfer h r am -> (Set.insert r fvs, C_Transfer h r am')
    where
      (fvs, am') = eargt h am
  IL_Claim h ct a -> (fvs, C_Claim h ct a')
    where
      (fvs, a') = eargt h a
  where
    earg h = epp_arg h γ RoleContract
    eargt h a = must_be_public h $ earg h a

epp_s_loc :: Show ann => EPPEnv -> ILVar -> ILStmt ann -> (Set.Set BLVar, EPStmt ann)
epp_s_loc γ p e = case e of
  IL_Transfer h _ _ -> expect_throw CE_LocalLimitation h ("transfer" :: String)
  IL_Claim h ct a -> (fvs, EP_Claim h ct a')
    where
      ((fvs, a'), _) = epp_arg h γ (RolePart p) a

epp_e_ctc2loc :: CExpr ann -> EPExpr ann
epp_e_ctc2loc (C_PrimApp h cp al) = (EP_PrimApp h (CP cp) al)
epp_e_ctc2loc (C_Digest h al) = (EP_Digest h al)
epp_e_ctc2loc (C_ArrayRef h ae ee) = (EP_ArrayRef h ae ee)

epp_s_ctc2loc :: CStmt ann -> EPStmt ann
epp_s_ctc2loc (C_Claim h ct a) = EP_Claim h ct a
epp_s_ctc2loc (C_Transfer h p a) = EP_Transfer h p a

data EPPCtxt ann
  = EC_Top
  | EC_Invariant
  | EC_WhileUntil (EPPRes ann) (EPPRes ann)
  | EC_WhileTrial [BLVar]
  | EC_WhileBody Int [BLVar] (Set.Set BLVar)

combine_maps :: Ord k => (k -> u -> v -> w) -> [k] -> M.Map k u -> M.Map k v -> M.Map k w
combine_maps f ks m1 m2 = M.fromList $ map cmb ks
  where
    cmb k = (k, f k (m1 M.! k) (m2 M.! k))

epp_it_ctc_do_if :: Show ann => ann -> [BLVar] -> (EPPEnv, ILArg ann) -> EPPRes ann -> EPPRes ann -> EPPRes ann
epp_it_ctc_do_if h ps (γc, ca) tres fres = do
  let (svs_ca, cca') = must_be_public h $ epp_arg h γc RoleContract ca
  (svs_t, ctt', ts1) <- tres
  (svs_f, cft', ts2) <- fres
  let svs = Set.unions [svs_ca, svs_t, svs_f]
  let ts3 = combine_maps mkt ps ts1 ts2
        where
          mkt p tt' ft' = EP_If h ca' tt' ft'
            where
              (_, ca') = must_be_public h $ epp_arg h γc (RolePart p) ca
  return (svs, C_If h cca' ctt' cft', ts3)

epp_it_ctc :: Show ann => [BLVar] -> Int -> EPPEnv -> EPPCtxt ann -> ILTail ann -> EPPRes ann
epp_it_ctc ps this_h γ ctxt it = case it of
  IL_Ret h args ->
    case ctxt of
      EC_WhileUntil kres bres -> do
        epp_it_ctc_do_if h ps (γ, arg0) kres bres
        where
          arg0 = case args of
            [arg] -> arg
            _ -> error "Expected exactly one arg" -- XXX
      EC_Invariant -> do
        return (mempty, C_Halt h, mempty)
      _ ->
        expect_throw CE_ContractReturns h ()
  IL_If h ca tt ft -> do
    epp_it_ctc_do_if h ps (γ, ca) (dres tt) (dres ft)
    where
      dres wt = epp_it_ctc ps this_h γ ctxt wt
  IL_Let h RoleContract what how next -> do
    let (st, svs_how, how_ctc) = epp_e_ctc γ how
    let whatenv = M.singleton what st
    let γ' = M.map (M.union whatenv) γ
    (svs1, next', ts1) <- epp_it_ctc ps this_h γ' ctxt next
    let svs = Set.union (Set.difference svs1 (boundBLVar what)) svs_how
    let how_ep = epp_e_ctc2loc how_ctc
    let ts2 = M.map (EP_Let h what how_ep) ts1
    return (svs, C_Let h what how_ctc next', ts2)
  IL_Let h (RolePart _) _ _ _ ->
    expect_throw CE_ContractLimitation h ("local binding" :: String)
  IL_Do h RoleContract how next -> do
    let (svs2, how') = epp_s_ctc γ how
    (svs1, ct1, ts1) <- epp_it_ctc ps this_h γ ctxt next
    let svs = Set.union svs1 svs2
    let ct2 = C_Do h how' ct1
    let ts2 = M.map (EP_Do h how'_ep) ts1
          where
            how'_ep = epp_s_ctc2loc how'
    return (svs, ct2, ts2)
  IL_Do h (RolePart _) _ _ ->
    expect_throw CE_ContractLimitation h ("local action" :: String)
  IL_ToConsensus h _ _ _ ->
    expect_throw CE_ContractLimitation h ("transition to consensus" :: String)
  IL_FromConsensus h bt -> do
    (svs0, next0, ts0) <- epp_it_loc ps (this_h, mempty) γ ctxt default_interval bt
    let ts1 = M.map (EP_FromConsensus h) ts0
    return (svs0, next0, ts1)
  IL_While h loopvs initas untilt invt bodyt kt -> do
    let (fvs_as, inita'_infos) = epp_args h γ RoleContract initas
    let initas' = map fst inita'_infos
    let loopvenv = M.fromList $ zip loopvs $ map snd inita'_infos
    let γ' = M.map (M.union loopvenv) γ
    (_, ct_inv, _) <- epp_it_ctc ps this_h γ' EC_Invariant invt
    let kres = epp_it_ctc ps this_h γ' ctxt kt
    let bres_trial = epp_it_ctc ps this_h γ' (EC_WhileTrial loopvs) bodyt
    (svs1_trial, _, _) <- localEPP $ epp_it_ctc ps this_h γ' (EC_WhileUntil kres bres_trial) untilt
    let svs1_trial' = Set.difference svs1_trial (boundBLVars loopvs)
    which <- acquireEPP
    let bres_real = epp_it_ctc ps this_h γ' (EC_WhileBody which loopvs svs1_trial') bodyt
    (svs1, ct1, ts1) <- epp_it_ctc ps this_h γ' (EC_WhileUntil kres bres_real) untilt
    let svs2 = Set.difference svs1 (boundBLVars loopvs)
    let svs2l = Set.toList svs2
    setEPP which $ C_Loop h svs2l loopvs ct_inv ct1
    let ts = M.map (EP_Loop h which loopvs initas') ts1
    let svs = Set.union fvs_as svs2
    let ct = C_Jump h which svs2l loopvs initas'
    return (svs, ct, ts)
  IL_Continue h nas ->
    case ctxt of
      EC_WhileTrial loopvs -> do
        return (svs, trial "ct", ts)
        where
          svs = fvs_as
          (fvs_as, _) = epp_args h γ RoleContract nas
          trial msg = impossible $ "EPP: WhileTrial: Cannot inspect " ++ msg
          ts = M.fromList $ map mkt ps
          mkt p = (p, EP_Continue h 0 loopvs $ trial "continue arg")
      EC_WhileBody which loopvs fvs_loop -> do
        return (svs, ct, ts)
        where
          (fvs_as, inita'_infos) = epp_args h γ RoleContract nas
          initas' = map (must_be_public h) inita'_infos
          svs = Set.union fvs_loop fvs_as
          fvs_loopl = Set.toList fvs_loop
          ct = C_Jump h which fvs_loopl loopvs initas'
          ts = M.fromList $ map mkt ps
          mkt p = (p, EP_Continue h which loopvs $ (map fst) . snd $ epp_args h γ (RolePart p) nas)
      _ ->
        expect_throw CE_ContinueNotInLoop h ("EPP" :: String)

epp_it_loc :: Show ann => [BLVar] -> (Int, Set.Set BLVar) -> EPPEnv -> EPPCtxt ann -> CInterval ann -> ILTail ann -> EPPRes ann
epp_it_loc ps last_hNvs γ ctxt toint it = case it of
  IL_Ret h al -> return (Set.empty, C_Halt h, ts)
    where
      ts = M.fromList $ map mkt ps
      mkt p = (p, EP_Ret h $ map fst $ snd $ epp_args h γ (RolePart p) al)
  IL_If h _ _ _ ->
    expect_throw CE_LocalLimitation h ("impure if" :: String)
  IL_Let h who what how next -> do
    let (fmst, extend_ts) =
          foldr addhow (Nothing, (\x -> x)) ps
          where
            addhow p (mst, do_extend) =
              if not (role_me (RolePart p) who)
                then (mst, do_extend)
                else (Just st, do_extend')
              where
                (st, _, how') = epp_e_loc γ p how
                do_extend' ts =
                  M.insert p t' ts'
                  where
                    t' = EP_Let h what how' t
                    t = ts' M.! p
                    ts' = do_extend ts
    let γ' = M.mapWithKey addwhat γ
          where
            addwhat r env =
              if role_me r who
                then M.insert what lst env
                else env
            lst = case fmst of
              Nothing -> expect_throw CE_UnknownRole h who
              Just v -> v
    (svs1, ct1, ts1) <- epp_it_loc ps last_hNvs γ' ctxt toint next
    return (svs1, ct1, extend_ts ts1)
  IL_Do h who how next -> do
    (svs1, ct1, ts1) <- epp_it_loc ps last_hNvs γ ctxt toint next
    let ts2 = M.mapWithKey addhow ts1
          where
            addhow p t =
              if not (role_me (RolePart p) who)
                then t
                else EP_Do h s' t
              where
                (_, s') = epp_s_loc γ p how
    return (svs1, ct1, ts2)
  IL_ToConsensus h (ok_ij, fromp, what, howmuch) mto next -> do
    hn_okay <- acquireEPP
    let fromr = RolePart fromp
    let (_, howmuch') = must_be_public h $ epp_arg h γ fromr howmuch
    let what' = map (must_be_public h) $ epp_vars h γ fromr what
    let what'env = M.fromList $ map (\v -> (v, Public)) what'
    let γ' = M.map (M.union what'env) γ
    (svs_okay, ct_okay, ts_okay) <- epp_it_ctc ps hn_okay γ' ctxt next
    let part_may_add True p x = Set.delete p x
        part_may_add False p x = Set.insert p x
    let svs_ok_needs = part_may_add ok_ij fromp (Set.difference svs_okay (boundBLVars what'))
    let (last_h, last_h_fvs) = last_hNvs
    (toint_ok, mdelay, svs_timeout, ts_timeout) <-
      case mto of
        Nothing ->
          return (toint, Nothing, mempty, mempty)
        Just (delay, timeout) -> do
          let (delay_vs, delay') = must_be_public h $ epp_arg h γ RoleContract delay
          let mdelay = Just delay'
          let toint_to = interval_add_from toint delay'
          let toint_ok = interval_add_to toint delay'
          let last_h_fvs' = Set.union svs_ok_needs last_h_fvs
          (svs_timeout, _, ts_timeout) <- epp_it_loc ps (last_h, last_h_fvs') γ ctxt toint_to timeout
          return (toint_ok, mdelay, (Set.union delay_vs svs_timeout), ts_timeout)
    let svs_all = Set.unions [last_h_fvs, svs_timeout, svs_ok_needs]
    let svs_all_l = Set.toList svs_all
    let ok_fs = (if ok_ij then FS_Join else FS_From) fromp
    setEPP hn_okay $ C_Handler h ok_fs toint_ok (last_h, svs_all_l) what' ct_okay
    let ct2 = C_Wait h last_h svs_all_l
    let ts2 = combine_maps mkt ps ts_okay ts_timeout
          where
            mkt p pt1 pt2 =
              let to_info = case mdelay of
                    Nothing -> Nothing
                    Just delay' -> Just (delay', pt2)
                  m_send_amt =
                    if (p == fromp)
                      then Just howmuch'
                      else Nothing
               in EP_SendRecv h svs_all_l m_send_amt (ok_fs, hn_okay, what', pt1) to_info
    return (svs_all, ct2, ts2)
  IL_FromConsensus h _ -> expect_throw CE_LocalLimitation h ("transition from consensus" :: String)
  IL_While h _ _ _ _ _ _ -> expect_throw CE_LocalLimitation h ("while" :: String)
  IL_Continue h _ -> expect_throw CE_LocalLimitation h ("continue" :: String)

epp :: Show ann => ILProgram ann -> BLProgram ann
epp (IL_Prog h rt ips it) = BL_Prog h rt bps cp
  where
    cp = C_Prog h chs
    ps = Set.toList ips
    bps = M.mapWithKey mkep ets
    mkep _ ept = EP_Prog h ept
    ((_, _, ets), chs) = runEPP $ epp_it_loc ps (0, mempty) γ EC_Top default_interval it
    γi = M.fromList $ map initγ $ Set.toList ips
    initγ p = (RolePart p, mempty)
    γ = M.insert RoleContract M.empty γi

data Verifier = Yices | Z3
  deriving (Read, Show, Eq)

data CompilerOpts = CompilerOpts
  { output :: T.Text -> String -> IO (),
    output_name :: T.Text -> String,
    source :: FilePath,
    -- | Enable experimental connectors
    expCon :: Bool,
    verifier :: Verifier
  }

compile :: CompilerOpts -> IO ()
compile copts = do
  let srcp = source copts
  let out = output copts
  let outn = output_name copts
  xlp <- readReachFile srcp
  out "xl" (L.unpack (pShow xlp))
  let xilp = inline xlp
  out "xil" (L.unpack (pShow xilp))
  let ilp = anf xilp
  out "il" (show (pretty ilp))
  case verifier copts of
    Z3 -> verify_z3 (outn "z3") ilp
    Yices -> verify_yices (outn "yi") ilp
  let blp = epp ilp
  out "bl" (show (pretty blp))
  cs <- compile_sol (outn "sol") blp
  let cnps_reg = M.fromList [(ETH, CNP_ETH cs)]
  cnps_exp <- case expCon copts of
    False -> return M.empty
    True -> do
      ebc <- emit_evm (outn "evm") blp
      tbc <- emit_teal (outn "teal") blp
      return $ M.fromList [(ETH_EVM, CNP_ETH (fst cs, ebc)), (ALGO, CNP_ALGO tbc)]
  let cnp_tm = cnpToFieldMap $ cnps_reg <> cnps_exp
  out "mjs" (show (emit_js blp cnp_tm))
  out "go" (show (emit_go blp cnp_tm))
  exitSuccess
