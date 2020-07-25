{-# LANGUAGE NoOverloadedStrings #-}

module Reach.Backend.Go where

import qualified Data.ByteString.Char8 as B
import Data.List (inits, intersperse)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Version (showVersion)
import Paths_reach (version)
import Reach.AST
import Reach.Connector.ETH_Solidity
  ( solMsg_evt
  , solMsg_fun
  )
import Reach.Util

goBType :: BaseType -> String
goBType BT_UInt256 = "uint256"
goBType BT_Bool = "bool"
goBType BT_Bytes = "bytes"
goBType BT_Address = "address"

goType' :: LType -> String
goType' (LT_BT bt) = goBType bt
goType' (LT_FixedArray bt _hm) = goBType bt ++ "arr"

goType :: LType -> Doc a
goType lt = pretty ("stdlib.Type_" ++ goType' lt)

gopart_name :: BLVar -> String
gopart_name b = "Part_" ++ (blvar_name b)

goString :: String -> Doc a
goString s = dquotes $ pretty s

goText :: T.Text -> Doc a
goText t = dquotes $ pretty t

goBacktickText :: T.Text -> Doc a
goBacktickText t = pretty "`" <> pretty t <> pretty "`"

goVar :: BLVar -> Doc a
goVar (n, _) = pretty $ "v" ++ show n

goLoopVar :: Int -> Doc a
goLoopVar i = pretty $ "l" ++ show i

goVarType_short_str :: BLVar -> String
goVarType_short_str (_, (_, bt)) = goType' bt

goVarType_short_strp :: BLVar -> Doc a
goVarType_short_strp v = goString $ goVarType_short_str v

goVarType_longp :: BLVar -> Doc a
goVarType_longp (_, (_, bt)) = goType bt

goVarAndType :: BLVar -> Doc a
goVarAndType v = goVar v <+> goVarType_longp v

goCon :: Constant -> Doc a
goCon (Con_I i) = pretty i
goCon (Con_B True) = pretty "true"
goCon (Con_B False) = pretty "false"
goCon (Con_BS s) = goString $ B.unpack s

goArg :: BLArg b -> (Doc a, Set.Set BLVar)
goArg (BL_Var _ v) = (goVar v, Set.singleton v)
goArg (BL_Con _ c) = (goCon c, Set.empty)

goVarDecl :: BLVar -> Doc a
goVarDecl bv = pretty "var" <+> goVar bv

goBraces :: Doc a -> Doc a
goBraces body = braces (nest 2 $ hardline <> body <> space)

goStructMake :: String -> [Doc a] -> Doc a
goStructMake ty elems = (pretty ty) <> (braces $ hcat $ intersperse (comma <> space) elems)

goApply :: String -> [Doc a] -> Doc a
goApply f args = pretty f <> parens (hcat $ intersperse (comma <> space) args)

goFunction :: String -> [Doc a] -> Doc a -> Doc a
goFunction name args body =
  pretty "func" <+> goApply name args <+> pretty "Ret" <+> goBraces body

goLambda :: [Doc a] -> Doc a -> Doc a
goLambda args body = goApply "" args <+> pretty "=>" <+> goBraces body

goWhile :: Doc a -> Doc a -> Doc a
goWhile cond body = pretty "for" <+> parens cond <+> goBraces body

goReturn :: Doc a -> Doc a
goReturn a = pretty "return" <+> a <> semi

goObject :: [(String, Doc a)] -> Doc a
goObject kvs = goBraces $ vsep $ (intersperse comma) $ map goObjField kvs
  where
    goObjField (k, v) = pretty (k ++ ":") <> hardline <> v

goBinOp :: String -> Doc a -> Doc a -> Doc a
goBinOp o l r = l <+> pretty o <+> r

goIf :: Doc a -> Doc a -> Doc a -> Doc a
goIf cap ttp ftp = pretty "if" <+> parens cap <+> pretty "{" <> (nest 2 $ hardline <> ttp) <> hardline <> pretty "} else {" <> (nest 2 $ hardline <> ftp) <+> pretty "}"

goTxn :: Int -> Doc a
goTxn n = pretty $ "txn" ++ show n

goTimeoutFlag :: Int -> Doc a
goTimeoutFlag n = goTxn n <> pretty ".DidTimeout"

goMsgEncode :: [BLVar] -> Doc a
goMsgEncode [] = pretty "stdlib.Msg0"
goMsgEncode (v : vs) =
  goApply
    ("stdlib.MsgEncode_" ++ goVarType_short_str v)
    [ goMsgEncode vs
    , goVar v
    ]

goMsgDecodePaths :: [BLVar] -> [Doc a]
goMsgDecodePaths vs = map h $ inits vs
  where
    h prevs = pretty "[]string{" <> hcat (intersperse (comma <> space) (map goVarType_short_strp prevs)) <> pretty "}"

goPrimApply :: Int -> EP_Prim -> [BLArg b] -> [Doc a] -> Doc a
goPrimApply tn pr al =
  case pr of
    CP ADD -> goApply "stdlib.Add"
    CP SUB -> goApply "stdlib.Sub"
    CP MUL -> goApply "stdlib.Mul"
    CP DIV -> goApply "stdlib.Div"
    CP MOD -> goApply "stdlib.Mod"
    CP PLT -> goApply "stdlib.Lt"
    CP PLE -> goApply "stdlib.Le"
    CP PEQ -> goApply "stdlib.Eq"
    CP PGE -> goApply "stdlib.Ge"
    CP PGT -> goApply "stdlib.Gt"
    CP LSH -> goApply "stdlib.Lsh"
    CP RSH -> goApply "stdlib.Rsh"
    CP BAND -> goApply "stdlib.Band"
    CP BIOR -> goApply "stdlib.Bior"
    CP BXOR -> goApply "stdlib.Bxor"
    CP IF_THEN_ELSE -> case al of
      [_, t, _] ->
        goApply ("stdlib.Ite_" ++ goType' (blarg_type t))
      _ -> impossible "ite not called with three args"
    CP BYTES_EQ -> goApply "stdlib.Bytes_eq"
    CP BALANCE -> \_ -> goTxn tn <> pretty ".Balance"
    CP TXN_VALUE -> \_ -> goTxn tn <> pretty ".Value"
    RANDOM -> goApply "stdlib.Random_uint256"

goEPExpr :: Int -> EPExpr b -> (Bool, (Doc a, Set.Set BLVar))
goEPExpr _tn (EP_Arg _ a) = (False, goArg a)
goEPExpr tn (EP_PrimApp _ pr al) = (False, ((goPrimApply tn pr al $ map fst alp), (Set.unions $ map snd alp)))
  where
    alp = map goArg al
goEPExpr _ (EP_Interact _ m _ al) = (True, (ip, (Set.unions $ map snd alp)))
  where
    alp = map goArg al
    ip = pretty "<-" <+> goApply ("interact." ++ m) (map fst alp)
goEPExpr _ (EP_Digest _ al) = (False, ((goApply "stdlib.Keccak256" $ map fst alp), (Set.unions $ map snd alp)))
  where
    alp = map goArg al
goEPExpr _ (EP_ArrayRef _ ae ee) = (False, ((ae_doc <> pretty "[" <> ee_doc <> pretty "]"), (Set.unions [ae_set, ee_set])))
  where
    (ae_doc, ae_set) = goArg ae
    (ee_doc, ee_set) = goArg ee

goAssert :: Doc a -> Doc a
goAssert a = goApply "stdlib.Assert" [a] <> semi

goTransfer :: BLVar -> Doc a -> Doc a
goTransfer to a = pretty "// XXX " <> goApply "txn.transfer" [goVar to, a] <> semi

goEPStmt :: EPStmt b -> Doc a -> (Doc a, Set.Set BLVar)
goEPStmt (EP_Claim _ CT_Possible _) kp = (kp, Set.empty)
goEPStmt (EP_Claim _ CT_Assert _) kp = (kp, Set.empty)
goEPStmt (EP_Claim _ _ a) kp = (vsep [goAssert ap, kp], afvs)
  where
    (ap, afvs) = goArg a
goEPStmt (EP_Transfer _ to a) kp = (vsep [goTransfer to ap, kp], fvs)
  where
    (ap, afvs) = goArg a
    fvs = Set.insert to afvs

add_from :: Int -> FromSpec -> (Doc a, Set.Set BLVar) -> (Doc a, Set.Set BLVar)
add_from tn (FS_Join p) (x, s) =
  ( vsep
      [ pretty "var" <+> goVar p <+> pretty "=" <+> goTxn tn <> pretty ".From" <> semi
      , x
      ]
  , s
  )
add_from _ (FS_From p) (x, s) = (x, Set.insert p s)

goEPTail :: Int -> BLVar -> EPTail b -> (Doc a, Set.Set BLVar)
goEPTail _tn _who (EP_Ret _ al) = ((goReturn $ goStructMake "Ret" $ map fst alp), Set.unions $ map snd alp)
  where
    alp = map goArg al
goEPTail tn who (EP_If _ ca tt ft) = (tp, tfvs)
  where
    (ttp', ttfvs) = goEPTail tn who tt
    (ftp', ftfvs) = goEPTail tn who ft
    (cap, cafvs) = goArg ca
    tp = goIf cap ttp' ftp'
    tfvs = Set.unions [cafvs, ttfvs, ftfvs]
goEPTail tn who (EP_Let _ bv ee kt) = (tp, tfvs)
  where
    used = elem bv ktfvs
    tp =
      if keep
        then vsep [bvdeclp, ktp]
        else ktp
    keep = used || forcep
    tfvs' = Set.difference ktfvs (Set.singleton bv)
    tfvs = if keep then Set.union eefvs tfvs' else tfvs'
    bvdeclp = goVarDecl bv <+> pretty "=" <+> eep <> semi
    (forcep, (eep, eefvs)) = goEPExpr tn ee
    (ktp, ktfvs) = goEPTail tn who kt
goEPTail tn who (EP_Do _ es kt) = (tp, tfvs)
  where
    (tp, esfvs) = goEPStmt es ktp
    tfvs = Set.union esfvs kfvs
    (ktp, kfvs) = goEPTail tn who kt
goEPTail tn who (EP_SendRecv _ svs m_send_amt (fs_ok, i_ok, msg, k_ok) mto) = (tp, tfvs)
  where
    tp = vsep [dp, k_p]
    k_okp' = mk_k_okp' k_okp
    (delayp, to_fvss, k_p) =
      case mto of
        Nothing -> (pretty "false", mempty, k_okp')
        Just (delay, k_to) -> (t_delayp, Set.unions [tofvs, delayfvs], kp)
          where
            (k_top, tofvs) = goEPTail tn' who k_to
            (t_delayp, delayfvs) = goArg delay
            kp = goIf (goTimeoutFlag tn') k_top k_okp'
    tfvs = Set.unions [Set.fromList svs, Set.fromList msg, k_fvs, extra_fvs]
    vs = goMsgEncode $ svs ++ msg
    (k_okp, k_fvs) = add_from tn' fs_ok $ goEPTail tn' who k_ok
    tn' = tn + 1
    (dp, extra_fvs, mk_k_okp') =
      case m_send_amt of
        Just amt -> (t_dp, t_extra_fvs, t_mk_k_okp')
          where
            t_dp =
              pretty "var" <+> goTxn tn' <+> pretty "="
                <+> pretty "<-"
                <+> (goApply
                       "ctc.SendRecv"
                       [ goString $ gopart_name who
                       , goString (solMsg_fun i_ok)
                       , vs
                       , amtp
                       , goString (solMsg_evt i_ok)
                       , delayp
                       ])
                  <> semi
            t_mk_k_okp' x = x
            (amtp, amtfvs) = goArg amt
            t_extra_fvs = Set.unions [to_fvss, amtfvs]
        Nothing -> (t_dp, t_extra_fvs, t_mk_k_okp')
          where
            t_dp =
              pretty "var" <+> goTxn tn' <+> pretty "="
                <+> pretty "<-"
                <+> (goApply
                       "ctc.Recv"
                       [ goString $ gopart_name who
                       , goString (solMsg_evt i_ok)
                       , delayp
                       ])
                  <> semi
            t_extra_fvs = mempty
            t_mk_k_okp' x = vsep $ msg_vsps ++ [x]
            msg_vsps = zipWith (\v vpre -> pretty "var" <+> (goVar v) <+> pretty "=" <+> goApply ("stdlib.MsgDecode_" ++ goVarType_short_str v) [(goTxn tn') <> pretty ".Data", vpre] <> semi) msg (goMsgDecodePaths msg)
goEPTail tn who (EP_Loop _ _which loopvs initas bt) = (tp, tfvs)
  where
    tp = vsep $ defsp ++ [loopp, pretty "panic(\"returned past for\")"]
    defp loopv initp = pretty "var" <+> (goVar loopv) <+> pretty "=" <+> initp <> semi
    defsp = zipWith defp loopvs $ map fst initargs
    loopp = goWhile (pretty "true") bodyp
    (bodyp, bodyvs) = goEPTail tn who bt
    initargs = map goArg initas
    tfvs = Set.unions $ bodyvs : (map snd initargs)
goEPTail _tn _who (EP_Continue _ _which loopvs args) = (tp, argvs)
  where
    tp = vsep $ setsp ++ [pretty "continue"]
    setsp = zipWith setp loopvs $ map fst argargs
    setp loopv argp = goVar loopv <+> pretty "=" <+> argp <> semi
    argvs = Set.unions $ map snd argargs
    argargs = map goArg args
goEPTail tn who (EP_FromConsensus _ kt) = (tp, kfvs)
  where
    tp = vsep [pretty "// XXX FromConsensus", ktp]
    (ktp, kfvs) = goEPTail tn who kt

goPart :: (BLVar, EProgram b) -> Doc a
goPart (p, ep@(EP_Prog _ et)) =
  vsep_with_blank [ity_p, funp]
  where
    tn' = 0
    pn = gopart_name p
    ity_n = "Interact_" ++ pn
    bodyp' = vsep [pretty "var" <+> goTxn tn' <+> pretty "= stdlib.Txn0" <> semi, pretty "_ = txn0;", bodyp]
    (bodyp, _) = goEPTail tn' p et
    funp = goFunction pn ([pretty "ctc stdlib.Contract", pretty ("interact " ++ ity_n)]) bodyp'
    ity_p = pretty ("type " ++ ity_n ++ " interface") <+> (braces $ nest 2 $ hardline <> (hcat $ intersperse (semi <> hardline) ity_eps))
    ity_eps = map ity_mk $ M.toList ity
    ity = ep_interacts ep
    ity_mk (n, (arg_tys, rt)) =
      pretty n <> parens (hcat $ intersperse (comma <> space) $ map goType arg_tys) <+> (pretty "<-chan" <+> (goType rt))

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

goStringMap :: M.Map T.Text T.Text -> Doc a
goStringMap m = pretty "map[string]string" <> (goBraces $ vsep $ map (<> comma) $ map goMapField kvs)
  where
    goMapField (k, v) = goText k <> pretty ": " <> goBacktickText v
    kvs = M.toList m

goCnp :: (T.Text, M.Map T.Text T.Text) -> Doc a
goCnp (name, cnp) = pretty "const " <> pretty name <> pretty " = " <> goStringMap cnp <> semi

emit_go :: BLProgram b -> CNP_TMap -> Doc a
emit_go (BL_Prog _ rts pm _) cnps = modp
  where
    modp = vsep_with_blank $ preamble : pkgp : importp : retp : partsp ++ cnpsp ++ [mainp]
    preamble = pretty $ "// Automatically generated with Reach " ++ showVersion version
    pkgp = pretty $ "package main"
    importp = pretty $ "import ( \"reach-sh/stdlib\" )"
    retp = pretty "type Ret struct" <> (braces $ hcat $ intersperse (semi <> space) $ map goType rts)
    partsp = map goPart $ M.toList pm
    cnpsp = map goCnp $ M.toList cnps
    mainp = pretty "func main() { }"
