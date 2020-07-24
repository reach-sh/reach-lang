module Reach.Connector.ALGO where

import Control.Monad.Extra
import Control.Monad.State.Lazy
import Data.ByteString.Base64 (encodeBase64')
---import Data.ByteString.Internal (unpackChars)
import qualified Data.ByteString.Char8 as C
import Data.List
import qualified Data.Map.Strict as M
---import qualified Data.ByteString as BS

import Reach.AST
import Reach.Connector.ETH_Solidity
  ( CCounts,
    usesCTail,
  )
import Reach.Util
import System.Exit
import System.IO
import System.Process

type CompiledTeal = (String, String, String)

type LabelM ann a = State LabelSt a

type LabelSt = Int

labelRun :: LabelM ann a -> a
labelRun lm = ans
  where
    (ans, _) = runState lm 0

data TEAL
  = TEAL TEALs

type TEALs = [TEALLine]

instance Show TEAL where
  show (TEAL tls) = concat $ intersperse "\n" $ map show tls

data TEALLine
  = TL_Comment String
  | TL_Label String
  | TL_Code [String]

instance Show TEALLine where
  show (TL_Comment c) = "// " ++ c
  show (TL_Label l) = l ++ ":"
  show (TL_Code s) = concat $ intersperse " " s

type CompileSt a =
  (String, Int, M.Map BLVar (VarRHS a))

data VarRHS a
  = VR_Expr (CExpr a)
  | VR_Slot Int
  | VR_Arg Int LType
  | VR_Code TEALs

type TxnSt =
  Int

txn_init :: TxnSt
txn_init = 2

txn_alloc :: TxnSt -> (Int, TxnSt)
txn_alloc i = (i, i + 1)

cs_init :: String -> CompileSt a
cs_init lab = (lab, 0, M.empty)

cs_var_loc :: CompileSt a -> BLVar -> (Int, CompileSt a)
cs_var_loc cs bv = (loc, cs')
  where
    cs' = (lab, loc + 1, M.insert bv (VR_Slot loc) vmap)
    (lab, loc, vmap) = cs

cs_var_locs :: CompileSt a -> [BLVar] -> CompileSt a
cs_var_locs cs bvs = foldl' h cs bvs
  where
    h cs0 bv = snd $ cs_var_loc cs0 bv

cs_var_set :: CompileSt a -> BLVar -> VarRHS a -> CompileSt a
cs_var_set cs bv vr = cs'
  where
    cs' = (lab, loc, M.insert bv vr vmap)
    (lab, loc, vmap) = cs

cs_var_args :: CompileSt a -> [BLVar] -> CompileSt a
cs_var_args cs as = cs'
  where
    h (cs0, i) a = ((cs_var_set cs0 a (VR_Arg i (blvar_type a))), i + 1)
    --- The first argument is 3 because 0 is the handler and 1 is
    --- the last time and 2 is the value
    (cs', _) = foldl' h (cs, 3) as

cs_label :: CompileSt a -> String
cs_label (lab, _, _) = lab

label_ext :: String -> Int -> String
label_ext lab ext = lab ++ "_" ++ (show ext)

alloc_lab :: CompileSt a -> LabelM ann String
alloc_lab cs = do
  let (lab, _, _) = cs
  lab_i <- get
  put $ lab_i + 1
  return $ label_ext lab lab_i

bracket :: String -> TEALs -> TEALs
bracket lab ls = TL_Comment ("<" ++ lab ++ ">") : ls ++ [TL_Comment ("</" ++ lab ++ ">")]

xxx :: String -> TEALs
xxx x = [TL_Comment $ "XXX " ++ x]

label :: String -> TEALs
label lab = [TL_Label lab]

code :: String -> [String] -> TEALs
code op args = [TL_Code $ op : args]

comp_con :: Constant -> TEALs
comp_con c =
  case c of
    Con_I i ->
      code "int" [show i]
    Con_B t ->
      comp_con $ Con_I $ if t then 1 else 0
    Con_BS bs ->
      code "byte" ["base64(" ++ (C.unpack (encodeBase64' bs)) ++ ")"]

comp_arg :: Int -> TEALs
comp_arg a = code "txna" ["ApplicationArgs", show a]

comp_blvar :: CompileSt a -> BLVar -> LabelM ann TEALs
comp_blvar cs bv =
  case M.lookup bv vmap of
    Nothing ->
      impossible $ "unbound variable: " ++ show bv
    Just (VR_Arg a lt) ->
      return $
        comp_arg a
          ++ case lt of
            LT_BT BT_UInt256 -> code "btoi" []
            LT_BT BT_Bool -> code "btoi" []
            _ -> []
    Just (VR_Slot s) ->
      return $ code "load" [show s]
    Just (VR_Code ls) ->
      return ls
    Just (VR_Expr ce) ->
      comp_cexpr cs ce
  where
    (_, _, vmap) = cs

comp_blarg_ty :: CompileSt a -> BLArg a -> LabelM ann (LType, TEALs)
comp_blarg_ty cs a =
  case a of
    BL_Con _ c -> return ((LT_BT $ conType c), comp_con c)
    BL_Var _ v -> do
      ls <- comp_blvar cs v
      let (_, (_, ty)) = v
      return (ty, ls)

comp_blarg :: CompileSt a -> BLArg a -> LabelM ann TEALs
comp_blarg cs a = do
  (_, ls) <- comp_blarg_ty cs a
  return ls

data HashMode
  = HM_Digest
  | HM_State Int Bool
  deriving (Show, Eq, Ord)

comp_blarg_for_hash :: CompileSt a -> Int -> BLArg a -> LabelM ann TEALs
comp_blarg_for_hash cs n a = do
  (ty, ls) <- comp_blarg_ty cs a
  let convert_ls =
        case ty of
          LT_BT BT_UInt256 -> code "itob" []
          LT_BT BT_Bool -> code "itob" []
          LT_BT BT_Bytes -> []
          LT_BT BT_Address -> []
          LT_FixedArray _bt _hm ->
            xxx "FixedArray comp_blarg_for_hash"
      combine = if n == 0 then [] else code "concat" []
  return $ ls ++ convert_ls ++ combine

comp_hash :: HashMode -> CompileSt a -> [BLArg a] -> LabelM ann TEALs
comp_hash hm cs as = do
  let (pre_len, pre_ls) =
        case hm of
          HM_Digest -> (0, [])
          HM_State i use_this_block -> (hm_len, hm_ls)
            where
              hm_len = 2
              hm_ls =
                comp_con (Con_I $ fromIntegral i)
                  ++ code "itob" []
                  ++ ( if use_this_block
                         then
                           ( code "global" ["Round"]
                               ++ code "itob" []
                           )
                         else comp_arg 1
                     )
                  ++ code "concat" []
  as_ls <- concatMapM (uncurry $ comp_blarg_for_hash cs) (zip [pre_len ..] as)
  return $
    pre_ls
      ++ as_ls
      ++ code "keccak256" []
      ++ code "btoi" []

comp_cexpr :: CompileSt a -> CExpr a -> LabelM ann TEALs
comp_cexpr cs e =
  case e of
    C_Digest _ as ->
      comp_hash HM_Digest cs as
    C_ArrayRef _ _ae _ee ->
      return $ xxx "comp_cexpr C_ArrayRef - use substring?"
    C_PrimApp _ cp as ->
      case cp of
        ADD -> p_op "+"
        SUB -> p_op "-"
        MUL -> p_op "*"
        DIV -> p_op "/"
        MOD -> p_op "%"
        PLT -> p_op "<"
        PLE -> p_op "<="
        PEQ -> p_op "=="
        PGT -> p_op ">"
        PGE -> p_op ">="
        LSH -> impossible "XXX TEAL doesn't support LSH"
        RSH -> impossible "XXX TEAL doesn't support RSH"
        BAND -> p_op "&"
        BIOR -> p_op "|"
        BXOR -> p_op "^"
        IF_THEN_ELSE -> do
          cond_ls <- comp_blarg cs a_cond
          true_lab <- alloc_lab cs
          false_ls <- comp_blarg cs a_false
          true_ls <- comp_blarg cs a_true
          cont_lab <- alloc_lab cs
          return $
            cond_ls
              ++ code "bnz" [true_lab]
              ++ false_ls
              ++ code "b" [cont_lab]
              ++ label true_lab
              ++ true_ls
              ++ label cont_lab
          where
            (a_cond, a_true, a_false) = case as of
              [c, t, f] -> (c, t, f)
              _ -> error "Expected exactly three elements" -- XXX
        BALANCE ->
          return $
            comp_con (Con_I 0)
              ++ code "balance" []
        TXN_VALUE ->
          return $ code "gtxn" ["1", "Amount"]
        BYTES_EQ -> p_op "=="
      where
        p_op o = do
          asl <- concatMapM (comp_blarg cs) as
          return $ asl ++ code o []

comp_cstmt :: CompileSt a -> TxnSt -> CStmt a -> LabelM ann (TxnSt, TEALs)
comp_cstmt cs ts s =
  case s of
    C_Claim _ CT_Possible _ -> return $ (ts, [])
    C_Claim _ CT_Assert _ -> return $ (ts, [])
    C_Claim _ _ a -> do
      a_ls <- comp_blarg cs a
      return $
        ( ts,
          a_ls
            ++ code "bz" ["revert"]
        )
    C_Transfer _ p a -> do
      a_ls <- comp_blarg cs a
      p_ls <- comp_blvar cs p
      let (txn_i, ts') = txn_alloc ts
      let txn_is = show txn_i
      return $
        ( ts',
          comp_con (Con_I $ fromIntegral $ txn_i)
            ++ code "global" ["GroupSize"]
            ++ code ">" []
            ++ code "bnz" ["revert"]
            ++ code "gtxn" [txn_is, "TypeEnum"]
            ++ code "int" ["pay"]
            ++ code "!=" []
            ++ code "bnz" ["revert"]
            ++ code "gtxn" [txn_is, "Amount"]
            ++ a_ls
            ++ code "!=" []
            ++ code "bnz" ["revert"]
            ++ code "gtxn" [txn_is, "Receiver"]
            ++ p_ls
            ++ code "!=" []
            ++ code "bnz" ["revert"]
            ++ code "gtxn" [txn_is, "Sender"]
            ++ comp_con (Con_BS "me")
            ++ code "app_global_get" []
            ++ code "!=" []
            ++ code "bnz" ["revert"]
        )

txn_ensure_size :: TxnSt -> TEALs
txn_ensure_size sz =
  comp_con (Con_I $ fromIntegral sz)
    ++ code "global" ["GroupSize"]
    ++ code "!=" []
    ++ code "bnz" ["revert"]

comp_ctail :: CCounts -> CompileSt a -> TxnSt -> CTail a -> LabelM ann TEALs
comp_ctail ccs cs ts t =
  case t of
    C_Halt _ ->
      return $
        txn_ensure_size ts
          ++ code "b" ["halt"]
    C_Wait loc i svs -> do
      hash_ls <- comp_hash (HM_State i True) cs (map (BL_Var loc) svs)
      return $
        txn_ensure_size ts
          ++ comp_con (Con_BS "state")
          ++ hash_ls
          ++ code "app_global_put" []
          ++ code "b" ["done"]
    C_If _ ca tt ft -> do
      ca_ls <- comp_blarg cs ca
      true_lab <- alloc_lab cs
      ft_ls <- comp_ctail ccs cs ts ft
      tt_ls <- comp_ctail ccs cs ts tt
      return $
        ca_ls
          ++ code "bnz" [true_lab]
          ++ ft_ls
          ++ label true_lab
          ++ tt_ls
    C_Let _ bv ce kt ->
      case M.lookup bv ccs of
        Just 0 -> comp_ctail ccs cs ts kt
        Just 1 -> comp_ctail ccs cs_later ts kt
        _ -> do
          ce_ls <- comp_cexpr cs ce
          kt_ls <- comp_ctail ccs cs_scratch ts kt
          return $
            ce_ls
              ++ code "store" [show bv_loc]
              ++ kt_ls
      where
        cs_later = cs_var_set cs bv (VR_Expr ce)
        (bv_loc, cs_scratch) = cs_var_loc cs bv
    C_Do _ ds kt -> do
      (ts', ds_ls) <- comp_cstmt cs ts ds
      kt_ls <- comp_ctail ccs cs ts' kt
      return $ ds_ls ++ kt_ls
    C_Jump loc which vs _ as ->
      if ts /= txn_init
        then impossible $ "jump preceded by transfers"
        else do
          args <- concatMapM (comp_blarg cs) $ (map (BL_Var loc) vs) ++ as
          return $
            args
              ++ stack_to_slot ((length vs) + (length as))
              ++ code "b" ["l" ++ show which]
      where
        stack_to_slot n =
          if n == 0
            then []
            else
              code "store" [show $ n - 1]
                ++ stack_to_slot (n - 1)

comp_chandler :: String -> CHandler a -> (String, TEALs)
comp_chandler next_lab (C_Handler loc from_spec interval (last_i, svs) msg body i) = (lab, bracket ("Handler " ++ show i) $ label lab ++ pre_ls ++ labelRun bodym)
  where
    lab = "h" ++ show i
    cs0 = cs_init lab
    all_args = svs ++ msg
    cs1 = cs_var_args cs0 $ all_args
    cs = case from_spec of
      FS_Join from ->
        cs_var_set cs1 from (VR_Code (code "gtxn" ["0", "Sender"]))
      FS_From _ -> cs1
    pre_ls =
      --- FIXME some of these checks could be combined across all handlers
      --- check account 0 is me
      comp_con (Con_BS "me")
        ++ code "txna" ["Accounts", "0"]
        ++ code "==" []
        ++ code "bz" ["revert"]
        --- check number of arguments
        ++ code "txn" ["NumAppArgs"]
        ++ (comp_con $ Con_I $ fromIntegral $ 3 + length all_args)
        ++ code "==" []
        ++ code "bz" [next_lab]
        --- check message number
        ++ comp_arg 0
        ++ code "btoi" []
        ++ (comp_con $ Con_I $ fromIntegral i)
        ++ code "!=" []
        ++ code "bnz" [next_lab]
        --- check that this is an app txn
        ++ code "gtxn" ["0", "TypeEnum"]
        ++ code "int" ["appl"]
        ++ code "==" []
        ++ code "bz" ["revert"]
        --- check that txn 1 is a pay to me
        ++ code "gtxn" ["1", "TypeEnum"]
        ++ code "int" ["pay"]
        ++ code "==" []
        ++ code "bz" ["revert"]
        ++ code "gtxn" ["1", "Receiver"]
        ++ comp_con (Con_BS "me")
        ++ code "app_global_get" []
        ++ code "==" []
        ++ code "bz" ["revert"]
        --- check that the amount agrees with argument 2
        ++ code "gtxn" ["1", "Amount"]
        ++ comp_arg 2
        ++ code "btoi" []
        ++ code "==" []
        ++ code "bz" ["revert"]
    bodym = do
      sender_ls <-
        case from_spec of
          FS_Join _ -> return []
          FS_From from -> do
            from_ls <- comp_blvar cs from
            return $
              code "gtxn" ["0", "Sender"]
                ++ from_ls
                ++ code "!=" []
                ++ code "bnz" ["revert"]
      let C_Between int_from int_to = interval
      let comp_int_side _sign [] = return $ []
          comp_int_side sign delays = do
            delays_ls <- mapM (comp_blarg cs) delays
            return $
              []
                -- begin timeout checking
                ++ ( foldl'
                       (\x y -> x ++ y ++ code "+" [])
                       (comp_arg 1 ++ code "btoi" [])
                       delays_ls
                   )
                -- the stack contains the deadline
                ++ code "global" ["Round"]
                ++ ( if sign
                       then -- if this is a timeout, then the round must be
                       -- after the deadline: deadline < running-time
                         code "<" []
                       else -- if this is not a timeout, then the round must be
                       -- before the deadline: running-time <= deadline ==
                       -- deadline >= running-time
                         code ">=" []
                   )
                ++ code "bz" ["revert"]
      -- end of timeout checking
      int_from_ls <- comp_int_side True int_from
      int_to_ls <- comp_int_side False int_to
      hash_ls <- comp_hash (HM_State last_i False) cs (map (BL_Var loc) svs)
      body_ls <- comp_ctail (usesCTail body) cs txn_init body
      return $
        sender_ls
          ++ int_from_ls
          ++ int_to_ls
          ++ hash_ls
          ++ comp_con (Con_BS "state")
          ++ code "app_global_get" []
          ++ code "!=" []
          ++ code "bnz" ["revert"]
          ++ body_ls
comp_chandler next_lab (C_Loop _ _svs _args _inv _body _i) = (next_lab, [])

comp_cloop :: CHandler a -> TEALs
comp_cloop (C_Handler _ _from_spec _interval _ _msg _body _i) = []
comp_cloop (C_Loop _ svs args _inv body i) = bracket ("Loop " ++ show i) $ label lab ++ labelRun (comp_ctail (usesCTail body) cs txn_init body)
  where
    lab = "l" ++ show i
    cs0 = cs_init lab
    cs = cs_var_locs cs0 $ svs ++ args

cp_to_teal :: CProgram a -> TEAL
cp_to_teal (C_Prog _ hs) = TEAL ls
  where
    ls = dispatch_ls ++ handlers_ls ++ loop_ls ++ standard_ls
    dispatch_ls =
      bracket "Constructor / Dispatcher" $
        code "txn" ["NumAppArgs"]
          ++ (comp_con $ Con_I $ 0)
          ++ code "==" []
          ++ code "bz" [next_lab]
          --- Check that global(me) is null
          ++ comp_con (Con_BS "me")
          ++ code "app_global_get" []
          ++ comp_con (Con_BS "")
          ++ code "==" []
          ++ code "bnz" ["revert"]
          --- Set global(me)
          ++ comp_con (Con_BS "me")
          ++ code "txna" ["Accounts", "0"]
          ++ code "app_global_put" []
          --- Set global(state)
          ++ comp_con (Con_BS "state")
          ++ labelRun (comp_hash (HM_State 0 True) (impossible "TEAL constructor does not use vars") [])
          ++ code "app_global_put" []
          ++ comp_con (Con_I 1)
          ++ code "return" []
      where
        next_lab = "h1"
    handlers_ls = bracket "Handlers" $ snd $ foldl' fh ("revert", []) (reverse hs)
      where
        fh (next_lab, prev_ls) h = (this_lab, both_ls)
          where
            (this_lab, this_ls) = comp_chandler next_lab h
            both_ls = this_ls ++ prev_ls
    loop_ls = bracket "Loops" $ concatMap comp_cloop hs
    standard_ls = bracket "Standard" $ revert_ls ++ halt_ls ++ done_ls
    halt_ls =
      label "halt"
        ++ comp_con (Con_BS "state")
        ++ code "app_global_del" []
        ++ comp_con (Con_BS "me")
        ++ code "app_global_del" []
        ++ code "b" ["done"]

revert_ls :: TEALs
revert_ls =
  label "revert"
    ++ comp_con (Con_I 0)
    ++ code "return" []

done_ls :: TEALs
done_ls =
  label "done"
    ++ comp_con (Con_I 1)
    ++ code "return" []

-- FIXME Do something like a "peep-hole optimizer" so we can detect "btoi -> itob" sequences

compile_teal :: String -> TEAL -> IO String
compile_teal _which t = do
  let ts = show t
  (Just hin, Just hout, Just herr, hP) <-
    createProcess (proc "goal" ["clerk", "compile", "-"]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hPutStr hin ts
  hClose hin
  hSetBinaryMode hout True
  stdout_bs <- C.hGetContents hout
  stderr_s <- hGetContents herr
  ec <- waitForProcess hP
  case ec of
    ExitFailure _ ->
      die $
        "goal clerk compile failed:\n"
          ++ "STDOUT:\n"
          ++ (C.unpack stdout_bs)
          ++ "\n"
          ++ "STDERR:\n"
          ++ stderr_s
          ++ "\n"
    ExitSuccess -> do
      return $ C.unpack $ encodeBase64' stdout_bs

lsp_bc :: TEAL
lsp_bc =
  TEAL $
    --- check that group size is at least 2
    (comp_con $ Con_I 2)
      ++ code "global" ["GroupSize"]
      ++ code "<=" []
      ++ code "bz" ["revert"]
      --- check that 1st txn is appl
      ++ code "gtxn" ["0", "TypeEnum"]
      ++ code "int" ["appl"]
      ++ code "==" []
      ++ code "bz" ["revert"]
      --- check that the app id is our first argument
      ++ code "gtxn" ["0", "ApplicationID"]
      --- FIXME this should be "compile argument", see https://github.com/algorand/go-algorand/issues/1051
      ++ code "arg" ["0"]
      ++ code "==" []
      ++ code "bz" ["revert"]
      ++ code "b" ["done"]
      ++ revert_ls
      ++ done_ls

emit_teal :: FilePath -> BLProgram a -> IO CompiledTeal
emit_teal tf (BL_Prog _ _ _ cp) = do
  let ap_bc = cp_to_teal cp
  writeFile tf (show ap_bc)
  writeFile (tf ++ ".lsp") (show lsp_bc)
  tc_lsp <- compile_teal "LSP" lsp_bc
  tc_ap <- compile_teal "AP" ap_bc
  tc_csp <- compile_teal "CSP" (TEAL $ comp_con (Con_I 1))
  return (tc_lsp, tc_ap, tc_csp)
