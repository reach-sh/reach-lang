module Reach.EmitJS where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.List (intersperse)
import Data.Text.Prettyprint.Doc
import Paths_reach (version)
import Data.Version (showVersion)

import Reach.AST
import Reach.EmitSol
  ( solMsg_evt
  , solMsg_fun
  , solType
  , CompiledSol )
import Reach.Util

jsString :: String -> Doc a
jsString s = squotes $ pretty s

jsVar :: BLVar -> Doc a
jsVar (n, _) = pretty $ "v" ++ show n

jsVar' :: BLVar -> Doc a
jsVar' (n, _) = pretty $ "p" ++ show n

jsLoopVar :: Int -> Doc a
jsLoopVar i = pretty $ "l" ++ show i

jsVarType :: BLVar -> Doc a
jsVarType (_, (_, bt)) = jsString $ solType bt

jsCon :: Constant -> Doc a
jsCon (Con_I i) = pretty i
jsCon (Con_B True) = pretty "true"
jsCon (Con_B False) = pretty "false"
jsCon (Con_BS s) = jsString $ B.unpack s

jsArg :: BLArg b -> (Doc a, Set.Set BLVar)
jsArg (BL_Var _ v) = (jsVar v, Set.singleton v)
jsArg (BL_Con _ c) = (jsCon c, Set.empty)

jsVarDecl :: BLVar -> Doc a
jsVarDecl bv = pretty "const" <+> jsVar bv

jsBraces :: Doc a -> Doc a
jsBraces body = braces (nest 2 $ hardline <> body <> space)

jsArray :: [Doc a] -> Doc a
jsArray elems = brackets $ hcat $ intersperse (comma <> space) elems

jsApply :: String -> [Doc a] -> Doc a
jsApply f args = pretty f <> parens (hcat $ intersperse (comma <> space) args)

jsFunction :: String -> [Doc a] -> Doc a -> Doc a
jsFunction name args body =
  pretty "async function" <+> jsApply name args <+> jsBraces body

jsLambda :: [Doc a] -> Doc a -> Doc a
jsLambda args body = jsApply "" args <+> pretty "=>" <+> jsBraces body

jsWhile :: Doc a -> Doc a -> Doc a
jsWhile cond body = pretty "while" <+> parens cond <+> jsBraces body

jsReturn :: Doc a -> Doc a
jsReturn a = pretty "return" <+> a <> semi

jsObject :: [(String, Doc a)] -> Doc a
jsObject kvs = jsBraces $ vsep $ (intersperse comma) $ map jsObjField kvs
  where jsObjField (k, v) = pretty (k ++ ":") <> hardline <> v

jsBinOp :: String -> Doc a -> Doc a -> Doc a
jsBinOp o l r = l <+> pretty o <+> r

jsIf :: Doc a -> Doc a -> Doc a -> Doc a
jsIf cap ttp ftp = pretty "if" <+> parens cap <+> jsBraces ttp <> hardline <> pretty "else" <+> jsBraces ftp

jsTxn :: Int -> Doc a
jsTxn n = pretty $ "txn" ++ show n

jsTimeoutFlag :: Int -> Doc a
jsTimeoutFlag n = jsTxn n <> pretty ".didTimeout"

jsPrimApply :: Int -> EP_Prim -> [Doc a] -> Doc a
jsPrimApply tn pr =
  case pr of
    CP ADD -> jsApply "stdlib.add"
    CP SUB -> jsApply "stdlib.sub"
    CP MUL -> jsApply "stdlib.mul"
    CP DIV -> jsApply "stdlib.div"
    CP MOD -> jsApply "stdlib.mod"
    CP PLT -> jsApply "stdlib.lt"
    CP PLE -> jsApply "stdlib.le"
    CP PEQ -> jsApply "stdlib.eq"
    CP PGE -> jsApply "stdlib.ge"
    CP PGT -> jsApply "stdlib.gt"
    CP IF_THEN_ELSE -> \args -> case args of
                      [ c, t, f ] -> c <+> pretty "?" <+> t <+> pretty ":" <+> f
                      _ -> impossible $ "emitJS: ITE called with wrong number of arguments"
    CP BYTES_EQ -> jsApply "stdlib.bytes_eq"
    CP BALANCE -> \_ -> jsTxn tn <> pretty ".balance"
    CP TXN_VALUE -> \_ -> jsTxn tn <> pretty ".value"
    RANDOM -> jsApply "stdlib.random_uint256"

jsEPExpr :: Int -> EPExpr b -> (Bool, (Doc a, Set.Set BLVar))
jsEPExpr _tn (EP_Arg _ a) = (False, jsArg a)
jsEPExpr tn (EP_PrimApp _ pr al) = (False, ((jsPrimApply tn pr $ map fst alp), (Set.unions $ map snd alp)))
  where alp = map jsArg al
jsEPExpr _ (EP_Interact _ m bt al) = (True, (ip, (Set.unions $ map snd alp)))
  where alp = map jsArg al
        ip = jsApply "stdlib.isType" [(jsString (solType bt)), pretty "await" <+> jsApply ("interact." ++ m) (map fst alp)]
jsEPExpr _ (EP_Digest _ al) = (False, ((jsApply "stdlib.keccak256" $ map fst alp), (Set.unions $ map snd alp)))
  where alp = map jsArg al
jsEPExpr _ (EP_ArrayRef _ ae ee) = (False, ((ae' <> pretty "[" <> ee' <> pretty "]"), (Set.unions $ map snd alp)))
  where alp = map jsArg [ae, ee]
        [ae', ee'] = map fst alp

jsAssert :: Doc a -> Doc a
jsAssert a = jsApply "stdlib.assert" [ a ] <> semi

jsTransfer :: BLVar -> Doc a -> Doc a
jsTransfer to a = jsApply "txn.transfer" [ jsVar to, a ] <> semi

jsEPStmt :: Bool -> EPStmt b -> Doc a -> (Doc a, Set.Set BLVar)
jsEPStmt stop_at_consensus s kp =
  case s of
    (EP_Claim _ CT_Possible _) -> (kp, Set.empty)
    (EP_Claim _ CT_Assert _) -> (kp, Set.empty)
    (EP_Claim _ _ a) -> (vsep [ jsAssert ap, kp ], afvs)
      where (ap, afvs) = jsArg a
    (EP_Transfer _ to a) ->
      if stop_at_consensus then
        (vsep [ jsTransfer to ap, kp ], fvs)
      else (kp, mempty)
      where (ap, afvs) = jsArg a
            fvs = Set.insert to afvs 

add_from :: Int -> FromSpec -> (Doc a, Set.Set BLVar) -> (Doc a, Set.Set BLVar)
add_from tn (FS_Join p) (x, s) =
  (vsep [ pretty "const" <+> jsVar p <+> pretty "=" <+> jsTxn tn <> pretty ".from" <> semi
        , x ]
  , s)
add_from _ (FS_From p) (x, s) = (x, Set.insert p s)
add_from _ FS_Any x = x

jsEPTail :: Bool -> Int -> BLPart -> EPTail b -> (Doc a, Set.Set BLVar)
jsEPTail stop_at_consensus tn who t =
  case t of
    (EP_Ret _ al) -> ((jsReturn $ jsArray $ map fst alp), Set.unions $ map snd alp)
      where alp = map jsArg al
    (EP_If _ ca tt ft) -> (tp, tfvs)
      where (ttp', ttfvs) = jsEPTail stop_at_consensus tn who tt
            (ftp', ftfvs) = jsEPTail stop_at_consensus tn who ft
            (cap, cafvs) = jsArg ca
            tp = jsIf cap ttp' ftp'
            tfvs = Set.unions [ cafvs, ttfvs, ftfvs ]
    (EP_Let _ bv ee kt) -> (tp, tfvs)
      where used = elem bv ktfvs
            tp = if keep then
                   vsep [ bvdeclp, ktp ]
              else
                   ktp
            keep = used || forcep
            tfvs' = Set.difference ktfvs (Set.singleton bv)
            tfvs = if keep then Set.union eefvs tfvs' else tfvs'
            bvdeclp = jsVarDecl bv <+> pretty "=" <+> eep <> semi
            (forcep, (eep, eefvs)) = jsEPExpr tn ee
            (ktp, ktfvs) = jsEPTail stop_at_consensus tn who kt
    (EP_SendRecv _ svs (fs_ok, i_ok, msg, amt, k_ok) (fs_to, i_to, delay, k_to)) -> (tp, tfvs)
      where tp = vsep [ dp, jsIf (jsTimeoutFlag tn') k_top k_okp ]
            dp = pretty "const" <+> jsTxn tn' <+> pretty "=" <+> pretty "await" <+> srp <> semi
            srp = jsApply "ctc.sendrecv"
              [ jsString $ blpart_name who
              , jsString (solMsg_fun i_ok)
              , vs
              , amtp
              , jsString (solMsg_evt i_ok)
              , delayp
              , jsString (solMsg_evt i_to)
              , jsLambda [ pretty "txn" ] ok_con_p ]
            (ok_con_p, ok_con_vs) = jsEPTail True tn' who k_ok
            tfvs = Set.unions [ kfvs, tofvs, amtfvs, delayfvs, Set.fromList svs, Set.fromList msg, ok_con_vs ]
            (delayp, delayfvs) = jsArg delay
            (amtp, amtfvs) = jsArg amt
            msg_vs = map jsVar msg
            vs = jsArray $ (map jsVar svs) ++ msg_vs
            (k_okp, kfvs) = add_from tn' fs_ok $ jsEPTail False tn' who k_ok
            (k_top, tofvs) = add_from tn' fs_to $ jsEPTail False tn' who k_to
            tn' = tn+1
    (EP_Do _ es kt) -> (tp, tfvs)
      where (tp, esfvs) = jsEPStmt stop_at_consensus es ktp
            tfvs = Set.union esfvs kfvs
            (ktp, kfvs) = jsEPTail stop_at_consensus tn who kt
    (EP_Recv _ svs (fs_ok, i_ok, msg, k_ok) (fs_to, i_to, delay, k_to)) -> (tp, tfvs)
      where tp = vsep [ rp, kp ]
            rp = pretty "const" <+> jsTxn tn' <+> pretty "=" <+>
                 pretty "await" <+>
                 (jsApply "ctc.recv"
                   [ jsString $ blpart_name who
                   , jsString (solMsg_evt i_ok)
                   , delayp, jsCon (Con_B to_me)
                   , (jsArray $ map jsVar svs)
                   , jsString (solMsg_fun i_to)
                   , jsString (solMsg_evt i_to)
                   , to_con_p ])
                 <> semi
            to_me = case fs_to of
                      FS_From x -> x == who
                      FS_Join x -> x == who
                      FS_Any -> True
            (to_con_p, to_con_vs) =
              if to_me then
                let (p, vs) = jsEPTail True tn' who k_to in
                  (jsLambda [ pretty "txn" ] p, vs)
              else
                (pretty "null", mempty)
            tfvs = Set.unions [Set.fromList svs, Set.fromList msg, kfvs, tofvs, delayfvs, to_con_vs]
            kp = jsIf (jsTimeoutFlag tn') k_top k_okp'
            k_okp' = vsep [ pretty "const" <+> jsArray msg_vs <+> pretty "=" <+> (jsTxn tn') <> pretty ".data" <> semi
                          , k_okp ]
            (delayp, delayfvs) = jsArg delay
            msg_vs = map jsVar msg
            (k_okp, kfvs) = add_from tn' fs_ok $ jsEPTail False tn' who k_ok
            (k_top, tofvs) = add_from tn' fs_to $ jsEPTail False tn' who k_to
            tn' = tn+1
    (EP_Loop _ _which loopvs initas bt) -> (tp, tfvs)
      where tp = vsep $ defsp ++ [ loopp ]
            defp loopv initp = pretty "let" <+> (jsVar loopv) <+> pretty "=" <+> initp <> semi
            defsp = zipWith defp loopvs $ map fst initargs
            loopp = jsWhile (pretty "true") bodyp
            (bodyp, bodyvs) = jsEPTail stop_at_consensus tn who bt
            initargs = map jsArg initas
            tfvs = Set.unions $ bodyvs : (map snd initargs)
    (EP_Continue _ _which loopvs args) -> do_stop $ (tp, argvs)
      where tp = vsep $ setsp ++ [ pretty "continue;" ]
            setsp = zipWith setp loopvs $ map fst argargs
            setp loopv argp = jsVar loopv <+> pretty "=" <+> argp <> semi
            argvs = Set.unions $ map snd argargs
            argargs = map jsArg args
    (EP_FromConsensus _ kt) -> do_stop $ (ktp, kfvs)
      where (ktp, kfvs) = jsEPTail False tn who kt
  where do_stop x = if stop_at_consensus then (jsReturn (jsCon (Con_B True)), mempty) else x

jsPart :: (BLPart, EProgram b) -> Doc a
jsPart (p, (EP_Prog _ pargs et)) =
  pretty "export" <+> jsFunction pn ([ pretty "ctc", pretty "interact" ] ++ pargs_vs) bodyp'
  where tn' = 0
        pn = blpart_name p
        pargs_vs = map jsVar pargs
        bodyp' = vsep [ pretty "const" <+> jsTxn tn' <+> pretty "= { balance: 0, value: 0 }" <> semi
                      , bodyp ]
        (bodyp, _) = jsEPTail False tn' p et

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

emit_js :: BLProgram b -> CompiledSol -> String -> Doc a
emit_js (BL_Prog _ _ pm _) (abi, code) code2 = modp
  where modp = vsep_with_blank $ preamble : importp : partsp ++ [ abip, codep, code2p ]
        preamble = pretty $ "// Automatically generated with Reach " ++ showVersion version
        importp = pretty $ "import * as stdlib from '@reach-sh/stdlib';"
        partsp = map jsPart $ M.toList pm
        abip = pretty $ "export const ABI = " ++ abi ++ ";"
        codep = pretty $ "export const Bytecode = \"0x" ++ code ++ "\";"
        code2p = pretty $ "export const Bytecode2 = \"0x" ++ code2 ++ "\";"
