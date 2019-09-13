module Reach.EmitJS where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.List (intersperse)
import Data.Text.Prettyprint.Doc

import Reach.AST
import Reach.EmitSol
  ( solMsg_evt
  , solMsg_fun
  , solType
  , CompiledSol )

{- Compilation to Javascript

   I'm imagining the type of the JS export is:

   Network -> Participant -> NetworkArgs x Args x (Result -> A) -> A or doesn't

   We have some standard way of interacting with the network (so we
   don't depend in the compiler on whether we're using rinkydink or
   whatever or what the name is.) Then, you can look up the code for
   one of the participants (by name). Then you provide extra arguments
   for the network (such as the contract / game id) and the
   participant's initial knowledge, then a continuation for what to do
   with the result.
  -}

jsString :: String -> Doc a
jsString s = dquotes $ pretty s

jsVar :: BLVar -> Doc a
jsVar (n, _, _) = pretty $ "v" ++ show n

jsVar' :: BLVar -> Doc a
jsVar' (n, _, _) = pretty $ "p" ++ show n

jsLoopVar :: Int -> Doc a
jsLoopVar i = pretty $ "l" ++ show i

jsVarType :: BLVar -> Doc a
jsVarType (_, _, bt) = jsString $ solType bt

jsCon :: Constant -> Doc a
jsCon (Con_I i) = pretty i
jsCon (Con_B True) = pretty "true"
jsCon (Con_B False) = pretty "false"
jsCon (Con_BS s) = jsString $ B.unpack s

jsArg :: BLArg b -> (Doc a, Set.Set BLVar)
jsArg (BL_Var _ v) = (jsVar v, Set.singleton v)
jsArg (BL_Con _ c) = (jsCon c, Set.empty)

jsPartVar :: Participant -> Doc a
jsPartVar p = pretty $ "p" ++ p

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

jsTxn :: Int -> Doc a
jsTxn n = pretty $ "txn" ++ show n

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
                      _ -> spa_error ()
    CP UINT256_TO_BYTES -> jsApply "stdlib.uint256_to_bytes"
    CP DIGEST -> jsApply "stdlib.keccak256"
    CP BYTES_EQ -> jsApply "stdlib.bytes_eq"
    CP BYTES_LEN -> jsApply "stdlib.bytes_len"
    CP BCAT -> jsApply "stdlib.bytes_cat"
    CP BCAT_LEFT -> jsApply "stdlib.bytes_left"
    CP BCAT_RIGHT -> jsApply "stdlib.bytes_right"
    CP BALANCE -> \_ -> jsTxn tn <> pretty ".balance"
    CP TXN_VALUE -> \_ -> jsTxn tn <> pretty ".value"
    RANDOM -> jsApply "stdlib.random_uint256"
    INTERACT -> error "interact doesn't use jsPrimApply"
  where spa_error () = error "jsPrimApply"

jsEPExpr :: Int -> EPExpr b -> (Doc a, Set.Set BLVar)
jsEPExpr _tn (EP_Arg _ a) = jsArg a
jsEPExpr tn (EP_PrimApp _ pr al) = ((jsPrimApply tn pr $ map fst alp), (Set.unions $ map snd alp))
  where alp = map jsArg al

jsAssert :: Doc a -> Doc a
jsAssert a = jsApply "stdlib.assert" [ a ] <> semi

jsEPStmt :: EPStmt b -> Doc a -> (Doc a, Set.Set BLVar)
jsEPStmt (EP_Claim _ CT_Possible _) kp = (kp, Set.empty)
jsEPStmt (EP_Claim _ CT_Assert _) kp = (kp, Set.empty)
jsEPStmt (EP_Claim _ _ a) kp = (vsep [ jsAssert ap, kp ], afvs)
  where (ap, afvs) = jsArg a
jsEPStmt (EP_Send _ _ _ _ _) _ = error "Impossible"

jsEPTail :: Int -> String -> EPTail b -> (Doc a, Set.Set BLVar)
jsEPTail _tn _who (EP_Ret _ al) = ((jsReturn $ jsArray $ map fst alp), Set.unions $ map snd alp)
  where alp = map jsArg al
jsEPTail tn who (EP_If _ ca tt ft) = (tp, tfvs)
  where (ttp', ttfvs) = jsEPTail tn who tt
        ttp = jsBraces ttp'
        (ftp', ftfvs) = jsEPTail tn who ft
        ftp = jsBraces ftp'
        (cap, cafvs) = jsArg ca
        tp = pretty "if" <+> parens cap <+> ttp <> hardline <> pretty "else" <+> ftp
        tfvs = Set.unions [ cafvs, ttfvs, ftfvs ]
jsEPTail tn who (EP_Let _ v (EP_PrimApp _ INTERACT al) kt) = (tp, tfvs)
  where (ktp, ktfvs) = jsEPTail tn who kt
        alp = map jsArg al
        ip = pretty "await" <+> jsApply "interact" (map fst alp)
        tfvs = Set.union ktfvs $ Set.unions $ map snd alp
        dp = jsVarDecl v <+> pretty "=" <+> ip <> semi
        tp = vsep [ dp, ktp ]
jsEPTail tn who (EP_Let _ bv ee kt) = (tp, tfvs)
  where used = elem bv ktfvs
        tp = if used then
               vsep [ bvdeclp, ktp ]
             else
               ktp
        tfvs' = Set.difference ktfvs (Set.singleton bv)
        tfvs = if used then Set.union eefvs tfvs' else tfvs'
        bvdeclp = jsVarDecl bv <+> pretty "=" <+> eep <> semi
        (eep, eefvs) = jsEPExpr tn ee
        (ktp, ktfvs) = jsEPTail tn who kt
jsEPTail tn who (EP_Do _ (EP_Send _ i svs msg amt) (EP_Recv _ True _ _ _ kt)) = (tp, tfvs)
  where srp = jsApply "ctc.sendrecv" [ jsString who
                                    , jsString (solMsg_fun i), vs, amtp
                                    , jsString (solMsg_evt i) ]
        dp = pretty "const" <+> jsArray [jsTxn tn'] <+> pretty "=" <+> pretty "await" <+> srp <> semi
        tp = vsep [ dp, skp ]
        skp = debugp <> ktp
        debugp = if global_debug then jsApply "console.log" [ jsString $ who ++ " sent/recv " ++ show i ] <> semi <> hardline else emptyDoc
        tfvs = Set.unions [ kfvs, amtfvs, Set.fromList svs, Set.fromList msg ]
        (amtp, amtfvs) = jsArg amt
        msg_vs = map jsVar msg
        vs = jsArray $ (map jsVar svs) ++ msg_vs
        (ktp, kfvs) = jsEPTail tn' who kt
        tn' = tn+1
jsEPTail tn who (EP_Do _ es kt) = (tp, tfvs)
  where (tp, esfvs) = jsEPStmt es ktp
        tfvs = Set.union esfvs kfvs
        (ktp, kfvs) = jsEPTail tn who kt
jsEPTail _ _ (EP_Recv _ True _ _ _ _) =
  error "Impossible"
jsEPTail tn who (EP_Recv _ False i _ msg kt) = (tp, tfvs)
  where tp = vsep [ rp, kp ]
        rp = pretty "const" <+> jsArray (msg_vs ++ [jsTxn tn']) <+> pretty "=" <+> pretty "await" <+> (jsApply "ctc.recv" [ jsString who, jsString (solMsg_evt i) ]) <> semi
        tfvs = Set.unions [Set.fromList msg, ktfvs]
        kp = (debugp <> ktp)
        msg_vs = map jsVar msg
        (ktp, ktfvs) = jsEPTail tn' who kt
        tn' = tn+1
        debugp = if global_debug then jsApply "console.log" [ jsString $ who ++ " received " ++ show i ] <> semi <> hardline else emptyDoc
jsEPTail tn who (EP_Loop _ which loopv inita bt) = (tp, tfvs)
  where tp = vsep [ defp, loopp ]
        defp = pretty "let" <+> (jsLoopVar which) <+> pretty "=" <+> initp <> semi
        loopp = jsWhile (pretty "true") bodyp'
        bodyp' = vsep [ jsVarDecl loopv <+> pretty "=" <+> (jsLoopVar which) <> semi
                      , bodyp ]
        (bodyp, bodyvs) = jsEPTail tn who bt
        (initp, initvs) = jsArg inita
        tfvs = Set.union initvs bodyvs
jsEPTail _tn _who (EP_Continue _ which arg) = (tp, argvs)
  where tp = vsep [ (jsLoopVar which) <+> pretty "=" <+> argp <> semi
                  , pretty "continue;" ]
        (argp, argvs) = jsArg arg

jsPart :: (Participant, EProgram b) -> Doc a
jsPart (p, (EP_Prog _ pargs et)) =
  pretty "export" <+> jsFunction p ([ pretty "stdlib", pretty "ctc", pretty "interact" ] ++ pargs_vs) bodyp'
  where tn' = 0
        pargs_vs = map jsVar pargs
        bodyp' = vsep [ pretty "const" <+> jsTxn tn' <+> pretty "= { balance: 0, value: 0 }" <> semi
                      , bodyp ]
        (bodyp, _) = jsEPTail tn' p et

global_debug :: Bool
global_debug = False

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

emit_js :: BLProgram b -> CompiledSol -> Doc a
emit_js (BL_Prog _ pm _) (abi, code) = modp
  where modp = vsep_with_blank $ partsp ++ [ abip, codep ]
        partsp = map jsPart $ M.toList pm
        abip = pretty $ "export const ABI = " ++ abi ++ ";"
        codep = pretty $ "export const Bytecode = " ++ code ++ ";"
