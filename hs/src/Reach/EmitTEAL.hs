module Reach.EmitTEAL where

import qualified Data.Map.Strict as M
import Data.List

import Reach.AST
import Reach.Util
import Reach.EmitSol
  ( CCounts
  , usesCTail )

data TEAL
  = TEAL TEALs

type TEALs = [ TEALLine ]

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

type CompileSt a
  = ( String, Int, M.Map BLVar (VarRHS a) )
data VarRHS a
  = VR_Expr (CExpr a)
  | VR_Slot Int
  | VR_Arg Int

cs_init :: String -> CompileSt a
cs_init lab = ( lab, 0, M.empty )

cs_var_loc :: CompileSt a -> BLVar -> (Int, CompileSt a)
cs_var_loc cs bv = (loc, cs')
  where cs' = ( lab, loc + 1, M.insert bv (VR_Slot loc) vmap )
        ( lab, loc, vmap ) = cs

cs_var_set :: CompileSt a -> BLVar -> VarRHS a -> CompileSt a
cs_var_set cs bv vr = cs'
  where cs' = ( lab, loc, M.insert bv vr vmap )
        ( lab, loc, vmap ) = cs

cs_var_expr :: CompileSt a -> BLVar -> CExpr a -> CompileSt a
cs_var_expr cs bv ce = cs_var_set cs bv (VR_Expr ce)

cs_var_arg :: CompileSt a -> BLVar -> Int -> CompileSt a
cs_var_arg cs bv i = cs_var_set cs bv (VR_Arg i)

cs_var_args :: CompileSt a -> [ BLVar ] -> CompileSt a
cs_var_args cs as = cs'
  where h (cs0, i) a = ((cs_var_arg cs0 a i), i+1)
        (cs', _) = foldl h (cs, 0) as

cs_label :: CompileSt a -> String
cs_label ( lab, _, _ ) = lab

label_ext :: String -> String -> String
label_ext lab ext = lab ++ "_" ++ ext

--- XXX have these be a kind of global monadic state
cs_label_ext :: CompileSt a -> String -> CompileSt a
cs_label_ext cs ext = cs'
  where ( lab, loc, vmap ) = cs
        lab' = label_ext lab ext
        cs' = ( lab', loc, vmap )

bracket :: String -> TEALs -> TEALs
bracket lab ls = TL_Comment ("<" ++ lab ++ ">") : ls ++ [ TL_Comment ("</" ++ lab ++ ">") ]

xxx :: String -> TEALs
xxx x = [ TL_Comment $ "XXX " ++ x ]

label :: String -> TEALs
label lab = [ TL_Label lab ]

code :: String -> [ String ] -> TEALs
code op args = [ TL_Code $ op : args ]

comp_con :: Constant -> TEALs
comp_con c =
  case c of
    Con_I i ->
      code "int" [ show i ]
    Con_B t ->
      comp_con $ Con_I $ if t then 1 else 0
    Con_BS bs ->
      code "byte" [ show bs ]

comp_blvar :: CompileSt a -> BLVar -> TEALs
comp_blvar cs bv = 
  case M.lookup bv vmap of
    Nothing ->
      impossible $ "unbound variable: " ++ show bv
    Just (VR_Arg a) ->
      code "arg" [ show a ]
    Just (VR_Slot s) ->
      code "load" [ show s ]
    Just (VR_Expr ce) ->
      comp_cexpr cs ce
  where ( _, _, vmap ) = cs

comp_blarg :: CompileSt a -> BLArg a -> TEALs
comp_blarg cs a =
  case a of
    BL_Con _ c -> comp_con c
    BL_Var _ v -> comp_blvar cs v

comp_cexpr :: CompileSt a -> CExpr a -> TEALs
comp_cexpr cs e =
  case e of
    C_Digest _ as ->
      concatMap (comp_blarg cs) as
      ++ xxx "digest"
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
        IF_THEN_ELSE ->
          comp_blarg cs a_cond
          ++ code "bnz" [true_lab]
          ++ comp_blarg cs a_false
          ++ code "jump" [cont_lab]
          --- XXX alloc lab
          ++ label true_lab
          ++ comp_blarg cs a_true
          --- XXX alloc lab
          ++ label cont_lab
          where [ a_cond, a_true, a_false ] = as
                lab = cs_label cs
                true_lab = label_ext lab "ITET"
                cont_lab = label_ext lab "ITEK"
        BALANCE -> xxx "balance"
        TXN_VALUE -> xxx "txn value"
        BYTES_EQ -> xxx "bytes_eq"
     where p_op o = concatMap (comp_blarg cs) as ++ code o []

comp_cstmt :: CompileSt a -> CStmt a -> TEALs
comp_cstmt cs s =
  case s of
    C_Claim _ CT_Possible _ -> []
    C_Claim _ CT_Assert _ -> []
    C_Claim _ _ a -> do
      comp_blarg cs a
      ++ code "!" []
      ++ code "bnz" [ "revert" ]
    C_Transfer _ p a ->
      comp_blarg cs a
      ++ comp_blvar cs p
      ++ xxx "transfer"

comp_ctail :: CCounts -> CompileSt a -> CTail a -> TEALs
comp_ctail ccs cs t =
  case t of
    C_Halt _ ->
      code "jump" ["halt"]
    C_Wait _ _i _svs ->
      xxx "wait"
    C_If _ ca tt ft ->
      comp_blarg cs ca
      ++ code "bnz" [true_lab]
      ++ comp_ctail ccs false_cs ft
      --- XXX alloc lab
      ++ label true_lab
      ++ comp_ctail ccs true_cs tt
      where false_cs = cs_label_ext cs "IF"
            true_cs = cs_label_ext cs "IT"
            true_lab = cs_label true_cs
    C_Let _ bv ce kt ->
      case M.lookup bv ccs of
        Just 0 -> comp_ctail ccs cs kt
        Just 1 -> comp_ctail ccs cs_later kt
        _ ->
          comp_cexpr cs ce
          ++ code "store" [show bv_loc]
          ++ comp_ctail ccs cs_scratch kt
      where cs_later = cs_var_expr cs bv ce
            (bv_loc, cs_scratch) = cs_var_loc cs bv
    C_Do _ ds kt ->
      comp_cstmt cs ds
      ++ comp_ctail ccs cs kt
    C_Jump _ _which _vs _ _as ->
      xxx "jump"

comp_chandler :: CHandler a -> TEALs
comp_chandler (C_Handler _ _from_spec _is_timeout (_last_i, svs) msg _delay body i) = bracket ("Handler " ++ show i) $ label lab ++ (xxx "handler") ++ comp_ctail (usesCTail body) cs body
  where lab = "h" ++ show i
        cs0 = cs_init lab
        cs = cs_var_args cs0 $ svs ++ msg
comp_chandler (C_Loop _ _svs _args _inv _body _i) = []

comp_cloop :: CHandler a -> TEALs
comp_cloop (C_Handler _ _from_spec _is_timeout _ _msg _delay _body _i) = []
comp_cloop (C_Loop _ svs args _inv body i) = bracket ("Loop " ++ show i) $ label lab ++ (xxx "loop pre") ++ comp_ctail (usesCTail body) cs body
  where lab = "l" ++ show i
        cs0 = cs_init lab
        cs = cs_var_args cs0 $ svs ++ args

cp_to_teal :: CProgram a -> TEAL
cp_to_teal (C_Prog _ hs) = TEAL ls
  where ls = dispatch_ls ++ handlers_ls ++ loop_ls ++ standard_ls
        dispatch_ls = bracket "Dispatcher" $ xxx "dispatcher"
        handlers_ls = bracket "Handlers" $ concatMap comp_chandler hs
        loop_ls = bracket "Loops" $ concatMap comp_cloop hs
        standard_ls = bracket "Standard" $ revert_ls ++ halt_ls
        revert_ls = label "revert"
                    ++ comp_con (Con_I 0)
                    ++ code "halt" []
        halt_ls = label "halt"
                  ++ comp_con (Con_I 1)
                  ++ code "halt" []

-- FIXME We use "halt" and "jump", which don't really exist

emit_teal :: FilePath -> BLProgram a -> IO String
emit_teal tf (BL_Prog _ _ cp) = do
  let bc = cp_to_teal cp
  writeFile tf (show bc)
  return ""
