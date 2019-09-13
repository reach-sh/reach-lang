{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Reach.Parser
  ( readReachFile
  ) where

import System.Directory
import System.FilePath
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Text.ParserCombinators.Parsec.Number (numberValue)
import Data.Data
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Control.Monad
import Text.Pretty.Simple
import qualified Data.Text.Lazy as L
import Data.FileEmbed
import GHC.IO.Encoding

import Reach.AST

type TP = (FilePath, Maybe TokenPosn)

type DecodeStmtsState = (FilePath, Maybe XLVar, Maybe Participant)
make_dss :: FilePath -> DecodeStmtsState
make_dss fp = (fp, Nothing, Nothing)
sub_dss :: DecodeStmtsState -> DecodeStmtsState
sub_dss (fp, lv, _) = (fp, lv, Nothing)

who_dss :: DecodeStmtsState -> Maybe Participant -> DecodeStmtsState
who_dss (fp, lv, _) who = (fp, lv, who)
dss_who :: DecodeStmtsState -> Maybe Participant
dss_who (_, _, w) = w

loopv_dss :: DecodeStmtsState -> XLVar -> DecodeStmtsState
loopv_dss (fp, _, w) lv = (fp, Just lv, w)
dss_loopv :: DecodeStmtsState -> Maybe XLVar
dss_loopv (_, lv, _) = lv

dss_tp :: DecodeStmtsState -> Maybe TokenPosn -> TP
dss_tp (fp, _, _) mt = (fp, mt)

tpa :: JSAnnot -> Maybe TokenPosn
tpa (JSAnnot t _) = Just t
tpa _ = Nothing

spa :: JSSemi -> Maybe TokenPosn
spa (JSSemi a) = tpa a
spa _ = Nothing

shortShow :: Show a => a -> String
shortShow j = take 1024 (L.unpack (pShow j))

expect_error :: Show a => Data a => String -> a -> b
expect_error label j = error $ "error: expected " ++ label ++ " but found " ++ (show $ toConstr j) ++ ":\n" ++ (shortShow j)

doXLEnum :: a -> XLVar -> [XLVar] -> [XLDef a]
doXLEnum ann predv vs = [ dvs, predd ]
  where dvs = XL_DefineValues ann vs ve
        ve = XL_Values ann $ zipWith (\_ i -> XL_Con ann (Con_I i)) vs [0..]
        predd = XL_DefineFun ann predv [ "x" ] checke
        checke = XL_FunApp ann (XL_Var ann "and") [ lee, lte ]
        lee = XL_FunApp ann (XL_Prim ann (CP PLE)) [ XL_Con ann (Con_I 0), XL_Var ann "x" ]
        lte = XL_FunApp ann (XL_Prim ann (CP PLT)) [ XL_Var ann "x", XL_Con ann (Con_I (toInteger (length vs))) ]

flattenJSArray :: [JSArrayElement] -> [JSExpression]
flattenJSArray a = concatMap f a
  where f (JSArrayComma _) = []
        f (JSArrayElement e) = [e]

flattenJSCL :: JSCommaList a -> [a]
flattenJSCL (JSLCons a _ b) = (flattenJSCL a) ++ [b]
flattenJSCL (JSLOne a) = [a]
flattenJSCL (JSLNil) = []

flattenJSCTL :: JSCommaTrailingList a -> [a]
flattenJSCTL (JSCTLComma a _) = flattenJSCL a
flattenJSCTL (JSCTLNone a) = flattenJSCL a

expectId :: JSExpression -> XLVar
expectId (JSIdentifier _ x) = x
expectId j = expect_error "identifier" j

expectIdent :: JSIdent -> XLVar
expectIdent (JSIdentName _ x) = x
expectIdent j = expect_error "identifier" j

decodeLetLHS :: JSExpression -> Maybe [XLVar]
decodeLetLHS (JSIdentifier _ x) = Just [x]
decodeLetLHS (JSArrayLiteral _ xs _) = Just $ map expectId $ flattenJSArray xs
decodeLetLHS j = expect_error "let lhs" j

decodeParams :: JSArrowParameterList -> [XLVar]
decodeParams j@(JSUnparenthesizedArrowParameter _) = expect_error "list of params" j
decodeParams (JSParenthesizedArrowParameterList _ j _) = map expectIdent $ flattenJSCL j

decodeBinOp :: (JSAnnot -> TP) -> JSBinOp -> XLExpr TP -> XLExpr TP -> XLExpr TP
decodeBinOp tp o arg1 arg2 =
  case o of
    JSBinOpAnd a -> fun a "and"
    JSBinOpDivide a -> prim a (CP DIV)
    JSBinOpEq a -> prim a (CP PEQ)
    JSBinOpGe a -> prim a (CP PGE)
    JSBinOpGt a -> prim a (CP PGT)
    JSBinOpLe a -> prim a (CP PLE)
    JSBinOpLt a -> prim a (CP PLT)
    JSBinOpMinus a -> prim a (CP SUB)
    JSBinOpMod a -> prim a (CP MOD)
    JSBinOpNeq a -> fun a "neq"
    JSBinOpOr a -> fun a "or"
    JSBinOpPlus a -> prim a (CP ADD)
    JSBinOpStrictEq a -> prim a (CP BYTES_EQ)
    JSBinOpStrictNeq a -> fun a "bytes_neq"
    JSBinOpTimes a -> prim a (CP MUL)
    j -> expect_error "binary operator" j
  where fun a f = XL_FunApp (tp a) (XL_Var (tp a) f) args
        prim a p = XL_FunApp (tp a) (XL_Prim (tp a) p) args
        args = [ arg1, arg2 ]

decodeUnaOp :: (JSAnnot -> TP) -> JSUnaryOp -> XLExpr TP -> XLExpr TP
decodeUnaOp tp j arg =
  case j of
    (JSUnaryOpMinus a) -> XL_FunApp (tp a) (XL_Prim (tp a) (CP SUB)) [ (XL_Con (tp a) (Con_I 0)), arg ]
    (JSUnaryOpNot a) -> XL_FunApp (tp a) (XL_Var (tp a) "not") [ arg ]
    _ -> expect_error "unary operator" j

string_trim_quotes :: [a] -> [a]
string_trim_quotes x = reverse $ tail $ reverse $ tail x

decodeExpr :: DecodeStmtsState -> JSExpression -> XLExpr TP
decodeExpr dss je =
  case je of
    (JSIdentifier a x) -> XL_Var (tp a) x
    (JSDecimal a n) -> XL_Con (tp a) (Con_I (numberValue 10 n))
    (JSLiteral a "true") -> XL_Con (tp a) (Con_B True)
    (JSLiteral a "false") -> XL_Con (tp a) (Con_B False)
    --- Other kinds of literals disallowed
    (JSHexInteger a n) -> XL_Con (tp a) (Con_I (numberValue 16 n))
    (JSOctal a n) -> XL_Con (tp a) (Con_I (numberValue 8 n))
    (JSStringLiteral a s) -> XL_Con (tp a) (Con_BS (B.pack (string_trim_quotes s)))
    --- No regex
    (JSArrayLiteral a es _) -> XL_Values (tp a) $ map (decodeExpr dss) $ flattenJSArray es
    --- No assign
    --- No call dot or call square ?
    (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier a "transfer") _ (JSLOne amt) _) _ (JSIdentifier _ "to")) _ (JSLOne (JSIdentifier _ p)) _) ->
      XL_Transfer (tp a) p (decodeExpr dss amt)
    --- No comma
    (JSExpressionBinary lhs op rhs) -> (decodeBinOp tp op) (decodeExpr dss lhs) (decodeExpr dss rhs)
    (JSExpressionParen _ j _) -> decodeExpr dss j
    --- No postfix
    (JSExpressionTernary c a t _ f) -> XL_If (tp a) (decodeExpr dss c) (decodeExpr dss t) (decodeExpr dss f)
    (JSArrowExpression params a s) -> XL_Lambda (tp a) (decodeParams params) (decodeStmts dss [s])
    --- No function w/ name
    --- No JSMemberDot
    (JSMemberDot (JSIdentifier a "txn") _ (JSIdentifier _ "value")) ->
      XL_FunApp (tp a) (XL_Prim (tp a) (CP TXN_VALUE)) []
    --- No JSMemberNew
    --- No JSMemberSquare
    --- No NewExpression
    --- No ObjectLiteral
    --- No SpreadExpression
    (JSUnaryExpression op e) -> (decodeUnaOp tp op) (decodeExpr dss e)
    --- No VarInitExpression
    (JSMemberExpression (JSMemberDot (JSIdentifier a "interact") _ (JSIdentifier ma method)) _ args _) ->
      XL_FunApp (tp a) (XL_Prim (tp a) INTERACT) ((XL_Con (tp ma) (Con_BS (B.pack method))):(map (decodeExpr dss) $ flattenJSCL args))
    (JSMemberExpression f a eargs _) ->
      case f of
        JSIdentifier _ "assert" -> claim CT_Assert
        JSIdentifier _ "assume" -> claim CT_Assume
        JSIdentifier _ "require" -> claim CT_Require
        JSIdentifier _ "possible" -> claim CT_Possible
        JSIdentifier _ "uint256_bytes" -> prim (CP UINT256_TO_BYTES)
        JSIdentifier _ "digest" -> prim (CP DIGEST)
        JSIdentifier _ "length" -> prim (CP BYTES_LEN)
        JSIdentifier _ "msgcons" -> prim (CP BCAT)
        JSIdentifier _ "msgleft" -> prim (CP BCAT_LEFT)
        JSIdentifier _ "msgright" -> prim (CP BCAT_RIGHT)
        JSIdentifier _ "balance" -> prim (CP BALANCE)
        JSIdentifier _ "random" -> prim RANDOM
        JSIdentifier _ "declassify" -> XL_Declassify h (arg1 ())
        o -> fun (decodeExpr dss o)
      where fun o = XL_FunApp h o args
            prim p = XL_FunApp h (XL_Prim h p) args
            claim ct = XL_Claim h ct (arg1 ())
            arg1 () = case args of
                        [x] -> x
                        _ -> expect_error "single argument to claim" je
            h = tp a
            args = (map (decodeExpr dss) $ flattenJSCL eargs)
    j -> expect_error "expression" j
  where tp = dss_tp dss . tpa

data LetNothing
  = LN_Flatten
  | LN_Null
  | LN_Error
letnothing :: LetNothing -> TP -> DecodeStmtsState -> XLExpr TP -> [JSStatement] -> XLExpr TP
letnothing flatok t dss e ks =
  case ks of
    [] -> case flatok of
      LN_Flatten -> e
      LN_Null -> XL_Let t (dss_who dss) Nothing e (XL_Values t [])
      LN_Error -> expect_error "non-empty statement list" (t,ks)
    _ -> XL_Let t (dss_who dss) Nothing e (decodeStmts dss ks)

mergeStmts :: JSStatement -> [JSStatement] -> [JSStatement]
mergeStmts (JSStatementBlock _ ss1 _ _) ss2 = ss1 ++ ss2
mergeStmts s ss = s:ss

doToConsensus :: TP -> Participant -> [XLVar] -> XLExpr TP -> XLExpr TP -> XLExpr TP
doToConsensus h p vs amt conk = XL_ToConsensus h p vs amt k'
  where k' = XL_Let h Nothing Nothing (XL_Claim h CT_Require (XL_FunApp h (XL_Prim h (CP PEQ)) [ (XL_FunApp h (XL_Prim h (CP TXN_VALUE)) []), amt ])) conk

decodeStmts :: DecodeStmtsState -> [JSStatement] -> XLExpr TP
decodeStmts dss js =
  let ds = decodeStmts in
  case js of
    j@[] -> expect_error "non-empty statement list" j
    ((JSStatementBlock a ss _ _):k) ->
      letnothing LN_Flatten (tp a) dss (ds dss ss) k
    --- No Break
    --- No Let
    ((JSConstant a (JSLOne (JSVarInitExpression v (JSVarInit _ e))) _):k) ->
      XL_Let (tp a) (dss_who dss) (decodeLetLHS v) (decodeExpr dss e) (ds dss k)
    --- No DoWhile
    --- No For, ForIn, ForVar, ForVarIn, ForLet, ForLetIn, ForLetOf, ForOf, ForVarOf
    --- No Function
    (j@(JSIf _ _ _ _ _):_k) -> expect_error "if must have else" j
    ((JSIfElse a _ cond _ true _ false):k) ->
      letnothing LN_Flatten (tp a) dss theif k
      where theif = (XL_If (tp a) (decodeExpr dss cond) (ds dss [true]) (ds dss [false]))
    --- No Labelled
    --- No EmptyStatement
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _) _):ek) ->
      doToConsensus (tp a) p vs amt conk
      where vs = map expectId $ flattenJSCL evs
            amt = decodeExpr dss eamt
            conk = ds (sub_dss dss) ek
    ((JSExpressionStatement e semi):k) ->
      letnothing LN_Null (sp semi) dss (decodeExpr dss e) k
    --- No AssignStatement
    ((JSMethodCall (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _ _):ek) ->
      doToConsensus (tp a) p [] amt conk
      where amt = decodeExpr dss eamt
            conk = ds (sub_dss dss) ek
    ((JSMethodCall (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _ _):ek) ->
      doToConsensus h p vs (XL_Con h (Con_I 0)) conk
      where vs = map expectId $ flattenJSCL evs        
            conk = ds (sub_dss dss) ek
            h = tp a
    ((JSMethodCall (JSMemberDot (JSIdentifier _a p) _ (JSIdentifier _ "only"))
       _ (JSLOne (JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ s)) _ _):k) ->
      ds (who_dss dss (Just p)) (mergeStmts s k)
    ((JSMethodCall (JSIdentifier a "commit") _ JSLNil _ _):k) ->
      XL_FromConsensus (tp a) (ds (sub_dss dss) k)
    ((JSMethodCall f ann1 args ann2 _semi):k) ->
      letnothing LN_Null (tp ann1) dss (decodeExpr dss (JSMemberExpression f ann1 args ann2)) k
    (j@(JSReturn a me _):k) ->
      case k of
        [] -> case me of
          Just e -> decodeExpr dss e
          Nothing -> XL_Values (tp a) []
        _ -> expect_error "return with nothing after it" j
    --- No Switch
    --- No Throw
    --- No Try (yet)
    --- No Variable
    --- No With
    ((JSVariable a (JSLOne (JSVarInitExpression (JSIdentifier _ loop_v) (JSVarInit _ einit_e))) _):(JSMethodCall (JSIdentifier _ "invariant") _ (JSLOne einvariant_e) _ _):(JSWhile _ _ econd_e _ ebody_e):k) ->
      XL_While h loop_v (decodeExpr dss einit_e) stop_e (decodeExpr dss einvariant_e) (ds dss' [ebody_e]) (ds dss k)
      where dss' = loopv_dss dss loop_v
            cond_e = (decodeExpr dss econd_e)
            stop_e = XL_FunApp h (XL_Var h "not") [ cond_e ]
            h = tp a
    ((JSAssignStatement (JSIdentifier a loopv) (JSAssign _) rhs _):(JSContinue _ JSIdentNone _):[]) ->
      if Just loopv == (dss_loopv dss) then
        XL_Continue (tp a) (decodeExpr dss rhs)
      else
        expect_error "continue matching nearest while loop" js
    (j:_) -> expect_error "statement" j
  where tp = dss_tp dss . tpa
        sp = dss_tp dss . spa

decodeBlock :: FilePath -> JSBlock -> XLExpr TP
decodeBlock fp (JSBlock _ ss _) = decodeStmts (make_dss fp) ss
             
decodeDef :: FilePath -> JSStatement -> [XLDef TP]
decodeDef fp j =
  case j of
    (JSConstant a (JSLOne (JSVarInitExpression (JSIdentifier _ x) (JSVarInit _ (JSMemberExpression (JSIdentifier _ "Enum") _ (JSLOne (JSArrayLiteral _ vs _)) _)))) _) ->
      doXLEnum (tp a) x (map expectId (flattenJSArray vs))
    (JSFunction a (JSIdentName _ f) _ eargs _ ee _) ->
      [XL_DefineFun (tp a) f args e]
      where args = map expectIdent (flattenJSCL eargs)
            e = decodeBlock fp ee
    _ -> expect_error "definition" j
  where tp a = (fp, tpa a)

decodeType :: JSExpression -> BaseType
decodeType (JSIdentifier _ "uint256") = AT_UInt256
decodeType (JSIdentifier _ "bool") = AT_Bool
decodeType (JSIdentifier _ "bytes") = AT_Bytes
decodeType j = expect_error "type" j

decodeVarDecl :: (JSAnnot -> TP) -> JSObjectProperty -> (TP, XLVar, BaseType)
decodeVarDecl tp (JSPropertyNameandValue (JSPropertyIdent _ v) a [ e ]) = ((tp a), v, bt)
  where bt = decodeType e
decodeVarDecl _ j = expect_error "variable declaration" j

decodeBody :: FilePath -> ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP)) -> JSModuleItem -> IO ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP))
decodeBody fp (d, p, me) msis =
  case msis of
    (JSModuleStatementListItem s@(JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ _x) (JSVarInit _ (JSMemberExpression (JSIdentifier _ "Enum") _ _ _)))) _)) ->
      return $ (d ++ decodeDef fp s, p, me)
    (JSModuleStatementListItem j@(JSFunction _ (JSIdentName _ "main") _ (JSLNil) _ body _)) ->
      case me of
        Nothing -> return $ (d, p, Just (decodeBlock fp body))
        Just _ -> expect_error "only one main function" j 
    (JSModuleStatementListItem s@(JSFunction _ _ _ _ _ _ _)) ->
      return $ (d ++ decodeDef fp s, p, me)
    (JSModuleStatementListItem (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ who) (JSVarInit _ (JSMemberExpression (JSIdentifier a "participant") _ (JSLOne (JSObjectLiteral _ vs _)) _)))) _)) ->
      return $ (d, p', me)
      where p' = M.insert who ((tp a), ds) p
            ds = map (decodeVarDecl tp) $ flattenJSCTL vs
    (JSModuleImportDeclaration _ (JSImportDeclarationBare _ m _)) ->
      let mp = string_trim_quotes m in
        do ma <- parseJsModule mp
           mfp <- makeAbsolute mp
           defs <- decodeXLLibrary mfp ma
           return $ (d ++ defs, p, me)
    _ -> expect_error "body element" msis
  where tp a = (fp, tpa a)

decodeXLProgram :: FilePath -> JSAST -> IO (XLProgram TP)
decodeXLProgram fp (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach/exe\'") _)):j) a) = do
  init_defs <- stdlib_defs
  (d, p, Just b) <- foldM (decodeBody fp) (init_defs, M.empty, Nothing) j
  return $ XL_Prog (fp, (tpa a)) d p b
decodeXLProgram _ j = expect_error "program" j

decodeXLLibrary :: FilePath -> JSAST -> IO [XLDef TP]
decodeXLLibrary fp (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach/lib\'") _)):j) a) = do
  (d, p, mm) <- foldM (decodeBody fp) ([], M.empty, Nothing) j
  (case mm of
     Nothing -> return ()
     _ -> expect_error "library has no main" a)
  (if M.null p then
     return ()
    else
     expect_error "library has no participants" a)
  return $ d
decodeXLLibrary _ j = expect_error "library" j

stdlib_defs :: IO [XLDef TP]
stdlib_defs = decodeXLLibrary "STDLIB" $ readJsModule $ (B.unpack $(embedFile "../reach/stdlib.reach"))

parseJsModule :: FilePath -> IO JSAST
parseJsModule f = do
  setLocaleEncoding utf8
  s <- readFile f
  return $ readJsModule s

readReachFile :: FilePath -> IO (XLProgram TP)
readReachFile srcp =
  withCurrentDirectory (takeDirectory srcp)
  (do js <- parseJsModule (takeFileName srcp)
      decodeXLProgram srcp js)
