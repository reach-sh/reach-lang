{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Reach.ParserInternal where

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

import Test.SmallCheck.Series
import GHC.Generics

import Reach.AST

type TP = (FilePath, Maybe TokenPosn)

show_tp :: TP -> String
show_tp (fp, mtp) = fp ++
  case mtp of
    Nothing -> ""
    Just (TokenPn _ l c) -> ":" ++ show l ++ ":" ++ show c

class ExtractTP a where
  extract_tp :: a -> Maybe TokenPosn

instance (ExtractTP JSAST) where
  extract_tp (JSAstProgram _ a) = tpa a
  extract_tp (JSAstModule _ a) = tpa a
  extract_tp (JSAstStatement _ a) = tpa a
  extract_tp (JSAstExpression _ a) = tpa a
  extract_tp (JSAstLiteral _ a) = tpa a

data ParseError
  = PE_HeaderProgram
  | PE_HeaderLibrary
  deriving (Generic, Show)

instance Monad m => Serial m ParseError

expect_throw :: ExtractTP a => ParseError -> FilePath -> a -> b
expect_throw pe fp j = error $ show_tp (fp, (extract_tp j)) ++ ": " ++ msg
  where msg = case pe of
          PE_HeaderProgram -> "expected: 'reach 0.1 exe';"
          PE_HeaderLibrary -> "expected: 'reach 0.1 lib';"

shortShow :: Show a => a -> String
shortShow j = take 1024 (L.unpack (pShow j))

expect_error :: Show a => Data a => String -> a -> b
expect_error label j = error $ "error: expected " ++ label ++ " but found " ++ (show $ toConstr j) ++ ":\n" ++ (shortShow j)

type DecodeStmtsState = (FilePath, Maybe [XLVar], Maybe XLPart)
make_dss :: FilePath -> DecodeStmtsState
make_dss fp = (fp, Nothing, Nothing)
sub_dss :: DecodeStmtsState -> DecodeStmtsState
sub_dss (fp, lv, _) = (fp, lv, Nothing)

who_dss :: DecodeStmtsState -> Maybe XLPart -> DecodeStmtsState
who_dss (fp, lv, _) who = (fp, lv, who)
dss_who :: DecodeStmtsState -> Maybe XLPart
dss_who (_, _, w) = w

loopvs_dss :: DecodeStmtsState -> [XLVar] -> DecodeStmtsState
loopvs_dss (fp, _, w) lv = (fp, Just lv, w)
dss_loopvs :: DecodeStmtsState -> Maybe [XLVar]
dss_loopvs (_, lv, _) = lv

dss_tp :: DecodeStmtsState -> Maybe TokenPosn -> TP
dss_tp (fp, _, _) mt = (fp, mt)

tpa :: JSAnnot -> Maybe TokenPosn
tpa (JSAnnot t _) = Just t
tpa _ = Nothing

spa :: JSSemi -> Maybe TokenPosn
spa (JSSemi a) = tpa a
spa _ = Nothing

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

decodeLetLHS :: JSExpression -> [XLVar]
decodeLetLHS (JSIdentifier _ x) = [x]
decodeLetLHS (JSArrayLiteral _ xs _) = map expectId $ flattenJSArray xs
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
    (JSMemberExpression (JSMemberDot (JSIdentifier a "interact") _ (JSIdentifier _ method)) _ args _) ->
      XL_Interact (tp a) method BT_Bool (map (decodeExpr dss) $ flattenJSCL args)
    (JSMemberExpression (JSIdentifier a "is") _ (JSLCons (JSLOne te) _ (JSMemberExpression (JSMemberDot (JSIdentifier _ "interact") _ (JSIdentifier _ method)) _ args _)) _) ->
      XL_Interact (tp a) method (decodeType te) (map (decodeExpr dss) $ flattenJSCL args)
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
      XL_Let (tp a) (dss_who dss) (Just (decodeLetLHS v)) (decodeExpr dss e) (ds dss k)
    --- No DoWhile
    --- No For, ForIn, ForVar, ForVarIn, ForLet, ForLetIn, ForLetOf, ForOf, ForVarOf
    --- No Function
    (j@(JSIf _ _ _ _ _):_k) -> expect_error "if must have else" j
    ((JSIfElse a _ cond _ true _ false):k) ->
      letnothing LN_Flatten (tp a) dss theif k
      where theif = (XL_If (tp a) (decodeExpr dss cond) (ds dss [true]) (ds dss [false]))
    --- No Labelled
    --- No EmptyStatement
    --- No AssignStatement
    --- Publish + Pay + Timeout
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _) _ (JSIdentifier _ "timeout")) _ targs _) _):ek) ->
      decodeToConsensus a p (Just evs) (Just eamt) targs ek
    --- Pay + Timeout
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _) _ (JSIdentifier _ "timeout")) _ targs _) _):ek) ->
      decodeToConsensus a p Nothing (Just eamt) targs ek
    --- Publish + Timeout
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _) _ (JSIdentifier _ "timeout")) _ targs _) _):ek) ->
      decodeToConsensus a p (Just evs) Nothing targs ek
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
    ((JSVariable a (JSLOne (JSVarInitExpression loop_var_je (JSVarInit _ einit_e))) _):(JSMethodCall (JSIdentifier _ "invariant") _ (JSLOne einvariant_e) _ _):(JSWhile _ _ econd_e _ ebody_e):k) ->
      XL_While h loop_vs (decodeExpr dss einit_e) stop_e (decodeExpr dss einvariant_e) (ds dss' [ebody_e]) (ds dss k)
      where dss' = loopvs_dss dss loop_vs
            loop_vs = decodeLetLHS loop_var_je
            cond_e = (decodeExpr dss econd_e)
            stop_e = XL_FunApp h (XL_Var h "not") [ cond_e ]
            h = tp a
    ((JSAssignStatement loopvs_je (JSAssign _) rhs _):(JSContinue a JSIdentNone _):[]) ->
      let loopvs = decodeLetLHS loopvs_je in
      if Just loopvs == (dss_loopvs dss) then
        XL_Continue (tp a) (decodeExpr dss rhs)
      else
        expect_error "continue matching nearest while loop" js
    ((JSExpressionStatement e semi):k) ->
      letnothing LN_Null (sp semi) dss (decodeExpr dss e) k
    (j:_) -> expect_error "statement" j
  where tp = dss_tp dss . tpa
        sp = dss_tp dss . spa
        decodeToConsensus a p mevs meamt etargs ek =
          XL_ToConsensus h (p, vs, amt) (mwho_to, de, te) k'
          where k' = XL_Let h Nothing Nothing amt_claim conk
                amt_claim = XL_Claim h CT_Require (XL_FunApp h (XL_Prim h (CP PEQ)) [ (XL_FunApp h (XL_Prim h (CP TXN_VALUE)) []), amt ])
                h = tp a
                vs = case mevs of
                  Nothing -> []
                  Just evs -> map expectId $ flattenJSCL evs
                amt = case meamt of
                  Nothing -> XL_Con h (Con_I 0)
                  Just eamt -> decodeExpr dss eamt
                conk = decodeStmts (sub_dss dss) ek
                mwho_to = case who_je of
                            JSIdentifier _ "_" -> Nothing
                            JSIdentifier _ who -> Just who
                            _ -> expect_error "active participant or no-one" who_je
                [ ede, who_je, ete ] = flattenJSCL etargs
                de = decodeExpr (sub_dss dss) ede
                te = XL_FunApp h (decodeExpr (sub_dss dss) ete) []

decodeBlock :: FilePath -> JSBlock -> XLExpr TP
decodeBlock fp (JSBlock _ ss _) = decodeStmts (make_dss fp) ss
             
decodeDef :: FilePath -> JSStatement -> [XLDef TP]
decodeDef fp j =
  case j of
    (JSConstant a (JSLOne (JSVarInitExpression (JSIdentifier _ x) (JSVarInit _ (JSMemberExpression (JSIdentifier _ "Enum") _ (JSLOne (JSArrayLiteral _ vs _)) _)))) _) ->
      doXLEnum (tp a) x (map expectId (flattenJSArray vs))
    (JSConstant a (JSLOne (JSVarInitExpression lhs (JSVarInit _ e))) _) ->
      [XL_DefineValues (tp a) (decodeLetLHS lhs) $ decodeExpr (make_dss fp) e]
    (JSFunction a (JSIdentName _ f) _ eargs _ ee _) ->
      [XL_DefineFun (tp a) f args e]
      where args = map expectIdent (flattenJSCL eargs)
            e = decodeBlock fp ee
    _ -> expect_error "definition" j
  where tp a = (fp, tpa a)

decodeType :: JSExpression -> BaseType
decodeType (JSIdentifier _ "uint256") = BT_UInt256
decodeType (JSIdentifier _ "bool") = BT_Bool
decodeType (JSIdentifier _ "bytes") = BT_Bytes
decodeType j = expect_error "type" j

decodeVarDecl :: (JSAnnot -> TP) -> JSObjectProperty -> (TP, XLVar, BaseType)
decodeVarDecl tp (JSPropertyNameandValue (JSPropertyIdent _ v) a [ e ]) = ((tp a), v, bt)
  where bt = decodeType e
decodeVarDecl _ j = expect_error "variable declaration" j

decodeBody :: FilePath -> ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP)) -> JSModuleItem -> IO ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP))
decodeBody fp (d, p, me) msis =
  case msis of
    (JSModuleStatementListItem (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ who) (JSVarInit _ (JSMemberExpression (JSIdentifier a "participant") _ (JSLOne (JSObjectLiteral _ vs _)) _)))) _)) ->
      return $ (d, p', me)
      where p' = M.insert who ((tp a), ds) p
            ds = map (decodeVarDecl tp) $ flattenJSCTL vs
    (JSModuleStatementListItem j@(JSFunction _ (JSIdentName _ "main") _ (JSLNil) _ body _)) ->
      case me of
        Nothing -> return $ (d, p, Just (decodeBlock fp body))
        Just _ -> expect_error "only one main function" j 
    (JSModuleStatementListItem s@(JSConstant _ _ _)) ->
      return $ (d ++ decodeDef fp s, p, me)
    (JSModuleStatementListItem s@(JSFunction _ _ _ _ _ _ _)) ->
      return $ (d ++ decodeDef fp s, p, me)
    (JSModuleImportDeclaration _ (JSImportDeclarationBare _ m _)) -> do
      defs <- readReachLibrary (string_trim_quotes m)
      return $ (d ++ defs, p, me)
    _ -> expect_error "body element" msis
  where tp a = (fp, tpa a)

decodeXLProgram :: FilePath -> JSAST -> IO (XLProgram TP)
decodeXLProgram fp (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach 0.1 exe\'") _)):j) a) = do
  init_defs <- stdlib_defs
  (d, p, Just b) <- foldM (decodeBody fp) (init_defs, M.empty, Nothing) j
  return $ XL_Prog (fp, (tpa a)) d p b
decodeXLProgram fp j = expect_throw PE_HeaderProgram fp j

decodeXLLibrary :: FilePath -> JSAST -> IO [XLDef TP]
decodeXLLibrary fp (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach 0.1 lib\'") _)):j) a) = do
  (d, p, mm) <- foldM (decodeBody fp) ([], M.empty, Nothing) j
  (case mm of
     Nothing -> return ()
     _ -> expect_error "library has no main" a)
  (if M.null p then
     return ()
    else
     expect_error "library has no participants" a)
  return $ d
decodeXLLibrary fp j = expect_throw PE_HeaderLibrary fp j

stdlib_defs :: IO [XLDef TP]
stdlib_defs = decodeXLLibrary "STDLIB" $ readJsModule $ (B.unpack $(embedFile "./rsh/stdlib.rsh"))

readReachX :: FilePath -> (FilePath -> JSAST -> IO a) -> IO a
readReachX srcp decodeIt = do
  srcp_abs <- makeAbsolute srcp
  setLocaleEncoding utf8
  s <- readFile srcp_abs
  let js = readJsModule s
  withCurrentDirectory (takeDirectory srcp_abs)
    (decodeIt srcp js)

readReachLibrary :: FilePath -> IO [XLDef TP]
readReachLibrary srcp = readReachX srcp decodeXLLibrary

readReachFile :: FilePath -> IO (XLProgram TP)
readReachFile srcp = readReachX srcp decodeXLProgram
