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

jsLoopName :: Int -> String
jsLoopName i = "l" ++ show i

jsVarType :: BLVar -> Doc a
jsVarType (_, _, bt) = jsString $ solType bt

jsCon :: Constant -> Doc a
jsCon (Con_I i) = pretty i
jsCon (Con_B True) = pretty "true"
jsCon (Con_B False) = pretty "false"
jsCon (Con_BS s) = jsString $ B.unpack s

jsArg :: BLArg -> (Doc a, Set.Set BLVar)
jsArg (BL_Var v) = (jsVar v, Set.singleton v)
jsArg (BL_Con c) = (jsCon c, Set.empty)

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
  pretty "function" <+> jsApply name args <+> jsBraces body

jsLambda :: [Doc a] -> Doc a -> Doc a
jsLambda args body = jsApply "" args <+> pretty "=>" <+> jsBraces body

jsReturn :: Doc a -> Doc a
jsReturn a = pretty "return" <+> a <> semi

jsObject :: [(String, Doc a)] -> Doc a
jsObject kvs = jsBraces $ vsep $ (intersperse comma) $ map jsObjField kvs
  where jsObjField (k, v) = pretty (k ++ ":") <> hardline <> v

jsBinOp :: String -> Doc a -> Doc a -> Doc a
jsBinOp o l r = l <+> pretty o <+> r

jsPrimApply :: EP_Prim -> [Doc a] -> Doc a
jsPrimApply pr =
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
    CP BALANCE -> \_ -> pretty $ "txn.balance"
    CP TXN_VALUE -> \_ -> pretty $ "txn.value"
    RANDOM -> jsApply "stdlib.random_uint256"
    INTERACT -> error "interact doesn't use jsPrimApply"
  where spa_error () = error "jsPrimApply"

jsEPExpr :: EPExpr -> (Doc a, Set.Set BLVar)
jsEPExpr (EP_Arg a) = jsArg a
jsEPExpr (EP_PrimApp pr al) = ((jsPrimApply pr $ map fst alp), (Set.unions $ map snd alp))
  where alp = map jsArg al

jsAssert :: Doc a -> Doc a
jsAssert a = jsApply "stdlib.assert" [ a ] <> semi

jsEPStmt :: EPStmt -> Doc a -> (Doc a, Set.Set BLVar)
jsEPStmt (EP_Claim CT_Possible _) kp = (kp, Set.empty)
jsEPStmt (EP_Claim CT_Assert _) kp = (kp, Set.empty)
jsEPStmt (EP_Claim _ a) kp = (vsep [ jsAssert ap, kp ], afvs)
  where (ap, afvs) = jsArg a
jsEPStmt (EP_Send _ _ _ _) _ = error "Impossible"

jsEPTail :: String -> EPTail -> (Doc a, Set.Set BLVar)
jsEPTail _who (EP_Ret al) = ((jsApply "kTop" $ map fst alp) <> semi, Set.unions $ map snd alp)
  where alp = map jsArg al
jsEPTail who (EP_If ca tt ft) = (tp, tfvs)
  where (ttp', ttfvs) = jsEPTail who tt
        ttp = jsBraces ttp'
        (ftp', ftfvs) = jsEPTail who ft
        ftp = jsBraces ftp'
        (cap, cafvs) = jsArg ca
        tp = pretty "if" <+> parens cap <+> ttp <> hardline <> pretty "else" <+> ftp
        tfvs = Set.unions [ cafvs, ttfvs, ftfvs ]
jsEPTail who (EP_Let v (EP_PrimApp INTERACT al) kt) = (tp, tfvs)
  where kp = jsLambda [ jsVar v ] ktp
        (ktp, ktfvs) = jsEPTail who kt
        alp = map jsArg al
        tp = jsApply "interact" ((map fst alp) ++ [ kp ]) <> semi
        tfvs = Set.union ktfvs $ Set.unions $ map snd alp
jsEPTail who (EP_Let bv ee kt) = (tp, tfvs)
  where used = elem bv ktfvs
        tp = if used then
               vsep [ bvdeclp, ktp ]
             else
               ktp
        tfvs' = Set.difference ktfvs (Set.singleton bv)
        tfvs = if used then Set.union eefvs tfvs' else tfvs'
        bvdeclp = jsVarDecl bv <+> pretty "=" <+> eep <> semi
        (eep, eefvs) = jsEPExpr ee
        (ktp, ktfvs) = jsEPTail who kt
jsEPTail who (EP_Do (EP_Send i svs msg amt) (EP_Recv True _ _ _ kt)) = (tp, tfvs)
  where tp = jsApply "ctc.sendrecv" [ jsString who
                                    , jsString (solMsg_fun i), vs, amtp
                                    , jsString (solMsg_evt i)
                                    , jsLambda ([ pretty "txn" ]) skp ]
             <> semi
        --- XXX This commented out stuff might be useful, but requires a JS stdlib change
        skp = debugp <> {- require_and <> hardline <> -} ktp
        debugp = if global_debug then jsApply "console.log" [ jsString $ who ++ " sent/recv " ++ show i ] <> semi <> hardline else emptyDoc
        tfvs = Set.unions [ kfvs, amtfvs, Set.fromList svs, Set.fromList msg ]
        (amtp, amtfvs) = jsArg amt
        msg_vs = map jsVar msg
        --- msg'_vs = map jsVar' msg
        --- require_and = vsep $ zipWith require_match msg_vs msg'_vs
        --- require_match mvp mv'p = jsAssert $ jsApply "stdlib.equal" [ mvp, mv'p ]
        vs = jsArray $ (map jsVar svs) ++ msg_vs
        (ktp, kfvs) = jsEPTail who kt
jsEPTail who (EP_Do es kt) = (tp, tfvs)
  where (tp, esfvs) = jsEPStmt es ktp
        tfvs = Set.union esfvs kfvs
        (ktp, kfvs) = jsEPTail who kt
jsEPTail _ (EP_Recv True _ _ _ _) =
  error "Impossible"
jsEPTail who (EP_Recv False i _ msg kt) = (tp, tfvs)
  where tp = jsApply "ctc.recv" [ jsString who, jsString (solMsg_evt i), kp ] <> semi
        tfvs = Set.unions [Set.fromList msg, ktfvs]
        kp = jsLambda (msg_vs ++ [pretty "txn"]) (debugp <> ktp)
        msg_vs = map jsVar msg
        (ktp, ktfvs) = jsEPTail who kt
        debugp = if global_debug then jsApply "console.log" [ jsString $ who ++ " received " ++ show i ] <> semi <> hardline else emptyDoc
jsEPTail who (EP_Loop which loopv inita bt) = (tp, tfvs)
  where tp = vsep [ defp, callp ]
        (callp, callvs) = jsEPTail who (EP_Continue which inita)
        defp = jsFunction (jsLoopName which) [ jsVar loopv ] bodyp <> semi
        (bodyp, bodyvs) = jsEPTail who bt
        tfvs = Set.union callvs bodyvs
jsEPTail _who (EP_Continue which arg) = (tp, argvs)
  where tp = jsApply (jsLoopName which) [ argp ] <> semi
        (argp, argvs) = jsArg arg

jsPart :: (Participant, EProgram) -> Doc a
jsPart (p, (EP_Prog pargs et)) =
  pretty "export" <+> jsFunction p ([ pretty "stdlib", pretty "ctc", pretty "txn", pretty "interact" ] ++ pargs_vs ++ [ pretty "kTop" ]) bodyp
  where pargs_vs = map jsVar pargs
        (bodyp, _) = jsEPTail p et

global_debug :: Bool
global_debug = False

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

emit_js :: BLProgram -> CompiledSol -> Doc a
emit_js (BL_Prog pm _) (abi, code) = modp
  where modp = vsep_with_blank $ partsp ++ [ abip, codep ]
        partsp = map jsPart $ M.toList pm
        abip = pretty $ "export const ABI = " ++ abi ++ ";"
        codep = pretty $ "export const Bytecode = " ++ code ++ ";"
