module Reach.EmitGo where

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
---import Reach.Util

goString :: String -> Doc a
goString s = dquotes $ pretty s

goVar :: BLVar -> Doc a
goVar (n, _) = pretty $ "v" ++ show n

goVar' :: BLVar -> Doc a
goVar' (n, _) = pretty $ "p" ++ show n

goLoopVar :: Int -> Doc a
goLoopVar i = pretty $ "l" ++ show i

goVarType' :: BLVar -> String
goVarType' (_, (_, bt)) = solType bt

goVarType :: BLVar -> Doc a
goVarType v = goString $ goVarType' v

goVarAndType :: BLVar -> Doc a
goVarAndType v = goVar v <+> pretty (goVarType' v)

goCon :: Constant -> Doc a
goCon (Con_I i) = pretty i
goCon (Con_B True) = pretty "true"
goCon (Con_B False) = pretty "false"
goCon (Con_BS s) = goString $ B.unpack s

goArg :: BLArg b -> (Doc a, Set.Set BLVar)
goArg (BL_Var _ v) = (goVar v, Set.singleton v)
goArg (BL_Con _ c) = (goCon c, Set.empty)

goVarDecl :: BLVar -> Doc a
goVarDecl bv = pretty "const" <+> goVar bv

goBraces :: Doc a -> Doc a
goBraces body = braces (nest 2 $ hardline <> body <> space)

goStruct :: String -> [Doc a] -> Doc a
goStruct ty elems = (pretty ty) <> (braces $ hcat $ intersperse (comma <> space) elems)

goApply :: String -> [Doc a] -> Doc a
goApply f args = pretty f <> parens (hcat $ intersperse (comma <> space) args)

goFunction :: String -> [Doc a] -> Doc a -> Doc a
goFunction name args body =
  pretty "func" <+> goApply name args <+> pretty "ret" <+> goBraces body

goLambda :: [Doc a] -> Doc a -> Doc a
goLambda args body = goApply "" args <+> pretty "=>" <+> goBraces body

goWhile :: Doc a -> Doc a -> Doc a
goWhile cond body = pretty "for" <+> parens cond <+> goBraces body

goReturn :: Doc a -> Doc a
goReturn a = pretty "return" <+> a <> semi

goObject :: [(String, Doc a)] -> Doc a
goObject kvs = goBraces $ vsep $ (intersperse comma) $ map goObjField kvs
  where goObjField (k, v) = pretty (k ++ ":") <> hardline <> v

goBinOp :: String -> Doc a -> Doc a -> Doc a
goBinOp o l r = l <+> pretty o <+> r

goIf :: Doc a -> Doc a -> Doc a -> Doc a
goIf cap ttp ftp = pretty "if" <+> parens cap <+> pretty "{" <> (nest 2 $ hardline <> ttp) <> hardline <> pretty "} else {" <> (nest 2 $ hardline <> ftp) <+> pretty "}"

goTxn :: Int -> Doc a
goTxn n = pretty $ "txn" ++ show n

goTimeoutFlag :: Int -> Doc a
goTimeoutFlag n = goTxn n <> pretty ".didTimeout"

goPrimApply :: Int -> EP_Prim -> [Doc a] -> Doc a
goPrimApply tn pr =
  case pr of
    CP ADD -> goApply "stdlib.add"
    CP SUB -> goApply "stdlib.sub"
    CP MUL -> goApply "stdlib.mul"
    CP DIV -> goApply "stdlib.div"
    CP MOD -> goApply "stdlib.mod"
    CP PLT -> goApply "stdlib.lt"
    CP PLE -> goApply "stdlib.le"
    CP PEQ -> goApply "stdlib.eq"
    CP PGE -> goApply "stdlib.ge"
    CP PGT -> goApply "stdlib.gt"
    CP IF_THEN_ELSE -> goApply "stdlib.ite"
    CP BYTES_EQ -> goApply "stdlib.bytes_eq"
    CP BALANCE -> \_ -> goTxn tn <> pretty ".balance"
    CP TXN_VALUE -> \_ -> goTxn tn <> pretty ".value"
    RANDOM -> goApply "stdlib.random_uint256"

goEPExpr :: Int -> EPExpr b -> (Bool, (Doc a, Set.Set BLVar))
goEPExpr _tn (EP_Arg _ a) = (False, goArg a)
goEPExpr tn (EP_PrimApp _ pr al) = (False, ((goPrimApply tn pr $ map fst alp), (Set.unions $ map snd alp)))
  where alp = map goArg al
goEPExpr _ (EP_Interact _ m bt al) = (True, (ip, (Set.unions $ map snd alp)))
  where alp = map goArg al
        ip = goApply "stdlib.isType" [(goString (solType bt)), pretty "<-" <+> goApply ("interact." ++ m) (map fst alp)]
goEPExpr _ (EP_Digest _ al) = (False, ((goApply "stdlib.keccak256" $ map fst alp), (Set.unions $ map snd alp)))
  where alp = map goArg al
goEPExpr _ (EP_ArrayRef _ ae ee) = (False, ((ae' <> pretty "[" <> ee' <> pretty "]"), (Set.unions $ map snd alp)))
  where alp = map goArg [ae, ee]
        [ae', ee'] = map fst alp

goAssert :: Doc a -> Doc a
goAssert a = goApply "stdlib.assert" [ a ] <> semi

goTransfer :: BLVar -> Doc a -> Doc a
goTransfer to a = pretty "// " <> goApply "txn.transfer" [ goVar to, a ] <> semi

goEPStmt :: EPStmt b -> Doc a -> (Doc a, Set.Set BLVar)
goEPStmt (EP_Claim _ CT_Possible _) kp = (kp, Set.empty)
goEPStmt (EP_Claim _ CT_Assert _) kp = (kp, Set.empty)
goEPStmt (EP_Claim _ _ a) kp = (vsep [ goAssert ap, kp ], afvs)
  where (ap, afvs) = goArg a
goEPStmt (EP_Transfer _ to a) kp = (vsep [ goTransfer to ap, kp ], fvs)
  where (ap, afvs) = goArg a
        fvs = Set.insert to afvs 

add_from :: Int -> FromSpec -> (Doc a, Set.Set BLVar) -> (Doc a, Set.Set BLVar)
add_from tn (FS_Join p) (x, s) =
  (vsep [ pretty "const" <+> goVar p <+> pretty "=" <+> goTxn tn <> pretty ".from" <> semi
        , x ]
  , s)
add_from _ (FS_From p) (x, s) = (x, Set.insert p s)
add_from _ FS_Any x = x

goEPTail :: Int -> BLPart -> EPTail b -> (Doc a, Set.Set BLVar)
goEPTail _tn _who (EP_Ret _ al) = ((goReturn $ goStruct "ret" $ map fst alp), Set.unions $ map snd alp)
  where alp = map goArg al
goEPTail tn who (EP_If _ ca tt ft) = (tp, tfvs)
  where (ttp', ttfvs) = goEPTail tn who tt
        (ftp', ftfvs) = goEPTail tn who ft
        (cap, cafvs) = goArg ca
        tp = goIf cap ttp' ftp'
        tfvs = Set.unions [ cafvs, ttfvs, ftfvs ]
goEPTail tn who (EP_Let _ bv ee kt) = (tp, tfvs)
  where used = elem bv ktfvs
        tp = if keep then
               vsep [ bvdeclp, ktp ]
             else
               ktp
        keep = used || forcep
        tfvs' = Set.difference ktfvs (Set.singleton bv)
        tfvs = if keep then Set.union eefvs tfvs' else tfvs'
        bvdeclp = goVarDecl bv <+> pretty "=" <+> eep <> semi
        (forcep, (eep, eefvs)) = goEPExpr tn ee
        (ktp, ktfvs) = goEPTail tn who kt
goEPTail tn who (EP_SendRecv _ svs (fs_ok, i_ok, msg, amt, k_ok) (fs_to, i_to, delay, k_to)) = (tp, tfvs)
  where srp = goApply "ctc.sendrecv" [ goString $ blpart_name who
                                     , goString (solMsg_fun i_ok), vs, amtp
                                     , goString (solMsg_evt i_ok)
                                     , delayp, goString (solMsg_evt i_to) ]
        dp = pretty "const" <+> goTxn tn' <+> pretty "=" <+> pretty "<-" <+> srp <> semi
        tp = vsep [ dp, goIf (goTimeoutFlag tn') k_top k_okp ]
        tfvs = Set.unions [ kfvs, tofvs, amtfvs, delayfvs, Set.fromList svs, Set.fromList msg ]
        (delayp, delayfvs) = goArg delay
        (amtp, amtfvs) = goArg amt
        msg_vs = map goVar msg
        vs = goStruct (solMsg_fun i_ok) $ (map goVar svs) ++ msg_vs
        (k_okp, kfvs) = add_from tn' fs_ok $ goEPTail tn' who k_ok
        (k_top, tofvs) = add_from tn' fs_to $ goEPTail tn' who k_to
        tn' = tn+1
goEPTail tn who (EP_Do _ es kt) = (tp, tfvs)
  where (tp, esfvs) = goEPStmt es ktp
        tfvs = Set.union esfvs kfvs
        (ktp, kfvs) = goEPTail tn who kt
goEPTail tn who (EP_Recv _ svs (fs_ok, i_ok, msg, k_ok) (fs_to, i_to, delay, k_to)) = (tp, tfvs)
  where tp = vsep [ rp, kp ]
        rp = pretty "const" <+> goTxn tn' <+> pretty "=" <+>
             pretty "<-" <+> (goApply "ctc.recv" [ goString $ blpart_name who
                                                    , goString (solMsg_evt i_ok)
                                                    , delayp, goCon (Con_B to_me)
                                                    , (goStruct (solMsg_fun i_to) $ map goVar svs)
                                                    , goString (solMsg_fun i_to), goString (solMsg_evt i_to)]) <> semi
        to_me = case fs_to of
                  FS_From x -> x == who
                  FS_Join x -> x == who
                  FS_Any -> True
        tfvs = Set.unions [Set.fromList svs, Set.fromList msg, kfvs, tofvs, delayfvs]
        kp = goIf (goTimeoutFlag tn') k_top k_okp'
        k_okp' = vsep $ msg_vsps ++ [ k_okp ]
        (delayp, delayfvs) = goArg delay
        msg_vsps = map (\v-> pretty "const" <+> v <+> pretty "=" <+> (goTxn tn') <> pretty ".data." <> v <> semi) msg_vs
        msg_vs = map goVar msg
        (k_okp, kfvs) = add_from tn' fs_ok $ goEPTail tn' who k_ok
        (k_top, tofvs) = add_from tn' fs_to $ goEPTail tn' who k_to
        tn' = tn+1
goEPTail tn who (EP_Loop _ _which loopvs initas bt) = (tp, tfvs)
  where tp = vsep $ defsp ++ [ loopp ]
        defp loopv initp = pretty "var" <+> (goVar loopv) <+> pretty "=" <+> initp <> semi
        defsp = zipWith defp loopvs $ map fst initargs
        loopp = goWhile (pretty "true") bodyp
        (bodyp, bodyvs) = goEPTail tn who bt
        initargs = map goArg initas
        tfvs = Set.unions $ bodyvs : (map snd initargs)
goEPTail _tn _who (EP_Continue _ _which loopvs args) = (tp, argvs)
  where tp = vsep $ setsp ++ [ pretty "continue" <> semi ]
        setsp = zipWith setp loopvs $ map fst argargs
        setp loopv argp = goVar loopv <+> pretty "=" <+> argp <> semi
        argvs = Set.unions $ map snd argargs
        argargs = map goArg args

goPart :: (BLPart, EProgram b) -> Doc a
goPart (p, (EP_Prog _ pargs et)) =
  goFunction pn ([ pretty "ctc Contract", pretty "interact Interact" ] ++ pargs_vs) bodyp'
  where tn' = 0
        pn = blpart_name p
        pargs_vs = map goVarAndType pargs
        bodyp' = vsep [ pretty "const" <+> goTxn tn' <+> pretty "= Txn{ balance: 0, value: 0 }" <> semi
                      , bodyp ]
        (bodyp, _) = goEPTail tn' p et

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

emit_go :: BLProgram b -> CompiledSol -> String -> Doc a
emit_go (BL_Prog _ pm _) (abi, code) code2 = modp
  where modp = vsep_with_blank $ preamble : pkgp : importp : partsp ++ [ abip, codep, code2p ]
        preamble = pretty $ "// Automatically generated with Reach " ++ showVersion version
        pkgp = pretty $ "package main"
        importp = pretty $ "// XXX import ( \"reach-sh/stdlib\" )"
        partsp = map goPart $ M.toList pm
        abip = pretty "const ABI = `" <> pretty abi <> pretty "`" <> semi
        codep = pretty $ "const Bytecode = \"0x" ++ code ++ "\";"
        code2p = pretty $ "const Bytecode2 = \"0x" ++ code2 ++ "\";"

--- XXX Since go is statically typed, I need to add definitions for
--- the various structures for the message data and return values.
