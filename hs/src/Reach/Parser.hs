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

--- XXX It stinks that this doesn't have the filename in it
type TP = Maybe TokenPosn

tp :: JSAnnot -> TP
tp (JSAnnot t _) = Just t
tp _ = Nothing

sp :: JSSemi -> TP
sp (JSSemi a) = tp a
sp _ = Nothing

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
        lee = XL_PrimApp ann (CP PLE) [ XL_Con ann (Con_I 0), XL_Var ann "x" ]
        lte = XL_PrimApp ann (CP PLT) [ XL_Var ann "x", XL_Con ann (Con_I (toInteger (length vs))) ]

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

decodeBinOp :: JSBinOp -> XLExpr TP -> XLExpr TP -> XLExpr TP
decodeBinOp o arg1 arg2 =
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
        prim a p = XL_PrimApp (tp a) p args
        args = [ arg1, arg2 ]

decodeUnaOp :: JSUnaryOp -> XLExpr TP -> XLExpr TP
decodeUnaOp (JSUnaryOpMinus a) arg = XL_PrimApp (tp a) (CP SUB) [ (XL_Con (tp a) (Con_I 0)), arg ]
decodeUnaOp (JSUnaryOpNot a) arg = XL_FunApp (tp a) (XL_Var (tp a) "not") [ arg ]
decodeUnaOp j _ = expect_error "unary operator" j

decodeExpr :: JSExpression -> XLExpr TP
decodeExpr (JSIdentifier a x) = XL_Var (tp a) x
decodeExpr (JSDecimal a n) = XL_Con (tp a) (Con_I (numberValue 10 n))
decodeExpr (JSLiteral a "true") = XL_Con (tp a) (Con_B True)
decodeExpr (JSLiteral a "false") = XL_Con (tp a) (Con_B False)
--- Other kinds of literals disallowed
decodeExpr (JSHexInteger a n) = XL_Con (tp a) (Con_I (numberValue 16 n))
decodeExpr (JSOctal a n) = XL_Con (tp a) (Con_I (numberValue 8 n))
decodeExpr (JSStringLiteral a s) = XL_Con (tp a) (Con_BS (B.pack (trim s)))
  where trim x = reverse $ tail $ reverse $ tail x
--- No regex
decodeExpr (JSArrayLiteral a es _) = XL_Values (tp a) $ map decodeExpr $ flattenJSArray es
--- No assign
--- No call dot or call square ?
decodeExpr (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier a "transfer") _ (JSLOne amt) _) _ (JSIdentifier _ "to")) _ (JSLOne (JSIdentifier _ p)) _) =
  XL_Transfer (tp a) p (decodeExpr amt)
--- No comma
decodeExpr (JSExpressionBinary lhs op rhs) = (decodeBinOp op) (decodeExpr lhs) (decodeExpr rhs)
decodeExpr (JSExpressionParen _ j _) = decodeExpr j
--- No postfix
decodeExpr (JSExpressionTernary c a t _ f) = XL_If (tp a) (decodeExpr c) (decodeExpr t) (decodeExpr f)
decodeExpr (JSArrowExpression params a s) = XL_Lambda (tp a) (decodeParams params) (decodeStmts Nothing [s])
--- No function w/ name
--- No JSMemberDot
--- No JSMemberNew
--- No JSMemberSquare
--- No NewExpression
--- No ObjectLiteral
--- No SpreadExpression
decodeExpr (JSUnaryExpression op e) = (decodeUnaOp op) (decodeExpr e)
--- No VarInitExpression
decodeExpr (JSMemberExpression (JSMemberDot (JSIdentifier a "interact") _ (JSIdentifier ma method)) _ args _) =
  XL_PrimApp (tp a) INTERACT ((XL_Con (tp ma) (Con_BS (B.pack method))):(map decodeExpr $ flattenJSCL args))
decodeExpr j@(JSMemberExpression f a eargs _) =
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
    o -> fun (decodeExpr o)
  where fun o = XL_FunApp h o args
        prim p = XL_PrimApp h p args
        claim ct = XL_Claim h ct (arg1 ())
        arg1 () = case args of
                    [x] -> x
                    _ -> expect_error "single argument to claim" j
        h = tp a
        args = (map decodeExpr $ flattenJSCL eargs)
--- XXX TXN_VALUE
decodeExpr j = expect_error "expression" j

data LetNothing
  = LN_Flatten
  | LN_Null
  | LN_Error
letnothing :: LetNothing -> TP -> Maybe Participant -> XLExpr TP -> [JSStatement] -> XLExpr TP
letnothing flatok t who e ks =
  case ks of
    [] -> case flatok of
      LN_Flatten -> e
      LN_Null -> XL_Let t who Nothing e (XL_Values t [])
      LN_Error -> expect_error "non-empty statement list" (t,ks)
    _ -> XL_Let t who Nothing e (decodeStmts who ks)

mergeStmts :: JSStatement -> [JSStatement] -> [JSStatement]
mergeStmts (JSStatementBlock _ ss1 _ _) ss2 = ss1 ++ ss2
mergeStmts s ss = s:ss

doToConsensus :: TP -> Participant -> [XLVar] -> XLExpr TP -> XLExpr TP -> XLExpr TP
doToConsensus h p vs amt conk = XL_ToConsensus h p vs amt k'
  where k' = XL_Let h Nothing Nothing (XL_Claim h CT_Require (XL_PrimApp h (CP PEQ) [ (XL_PrimApp h (CP TXN_VALUE) []), amt ])) conk

decodeStmts :: Maybe Participant -> [JSStatement] -> XLExpr TP
decodeStmts _who j@[] = expect_error "non-empty statement list" j
decodeStmts who ((JSStatementBlock a ss _ _):k) =
  letnothing LN_Flatten (tp a) who (decodeStmts who ss) k
--- No Break
--- No Let
decodeStmts who ((JSConstant a (JSLOne (JSVarInitExpression v (JSVarInit _ e))) _):k) =
  XL_Let (tp a) who (decodeLetLHS v) (decodeExpr e) (decodeStmts who k)
--- No DoWhile
--- No For, ForIn, ForVar, ForVarIn, ForLet, ForLetIn, ForLetOf, ForOf, ForVarOf
--- No Function
decodeStmts _who (j@(JSIf _ _ _ _ _):_k) = expect_error "if must have else" j
decodeStmts who ((JSIfElse a _ cond _ true _ false):k) =
  letnothing LN_Flatten (tp a) who theif k
  where theif = (XL_If (tp a) (decodeExpr cond) (decodeStmts who [true]) (decodeStmts who [false]))
--- No Labelled
--- No EmptyStatement
decodeStmts _who ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _) _):ek) =
  doToConsensus (tp a) p vs amt conk
  where vs = map expectId $ flattenJSCL evs
        amt = decodeExpr eamt
        conk = decodeStmts Nothing ek
decodeStmts who ((JSExpressionStatement e semi):k) =
  letnothing LN_Null (sp semi) who (decodeExpr e) k
--- No AssignStatement
decodeStmts _who ((JSMethodCall (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _ _):ek) =
  doToConsensus (tp a) p [] amt conk
  where amt = decodeExpr eamt
        conk = decodeStmts Nothing ek
decodeStmts _who ((JSMethodCall (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _ _):ek) =
  doToConsensus h p vs (XL_Con h (Con_I 0)) conk
  where vs = map expectId $ flattenJSCL evs        
        conk = decodeStmts Nothing ek
        h = tp a
decodeStmts _who ((JSMethodCall (JSMemberDot (JSIdentifier _a p) _ (JSIdentifier _ "only"))
                   _ (JSLOne (JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ s)) _ _):k) =
  decodeStmts (Just p) (mergeStmts s k)
decodeStmts _who ((JSMethodCall (JSIdentifier a "commit") _ JSLNil _ _):k) =
  XL_FromConsensus (tp a) (decodeStmts Nothing k)
decodeStmts who ((JSMethodCall f ann1 args ann2 _semi):k) =
  letnothing LN_Null (tp ann1) who (decodeExpr (JSMemberExpression f ann1 args ann2)) k
decodeStmts _who (j@(JSReturn a me _):k) =
  case k of
    [] -> case me of
      Just e -> decodeExpr e
      Nothing -> XL_Values (tp a) []
    _ -> expect_error "return with nothing after it" j
--- No Switch
--- No Throw
--- No Try (yet)
--- No Variable
--- No With
decodeStmts who ((JSVariable a (JSLOne (JSVarInitExpression (JSIdentifier _ loop_v) (JSVarInit _ einit_e))) _):(JSMethodCall (JSIdentifier _ "invariant") _ (JSLOne einvariant_e) _ _):(JSWhile _ _ econd_e _ ebody_e):k) =
  XL_While h loop_v (decodeExpr einit_e) stop_e (decodeExpr einvariant_e) (decodeStmts who [ebody_e]) (decodeStmts who k)
  where cond_e = (decodeExpr econd_e)
        stop_e = XL_FunApp h (XL_Var h "not") [ cond_e ]
        h = tp a
decodeStmts _who ((JSAssignStatement (JSIdentifier a _loopv) (JSAssign _) rhs _):(JSContinue _ JSIdentNone _):[]) =
  --- XXX Check loopv is correct
  XL_Continue (tp a) (decodeExpr rhs)
decodeStmts _who (j:_) = expect_error "statement" j

decodeBlock :: JSBlock -> XLExpr TP
decodeBlock (JSBlock _ ss _) = decodeStmts Nothing ss
             
decodeDef :: JSStatement -> [XLDef TP]
decodeDef (JSConstant a (JSLOne (JSVarInitExpression (JSIdentifier _ x) (JSVarInit _ (JSMemberExpression (JSIdentifier _ "Enum") _ (JSLOne (JSArrayLiteral _ vs _)) _)))) _) = doXLEnum (tp a) x (map expectId (flattenJSArray vs))
decodeDef (JSFunction a (JSIdentName _ f) _ eargs _ ee _) = [XL_DefineFun (tp a) f args e]
  where args = map expectIdent (flattenJSCL eargs)
        e = decodeBlock ee
decodeDef j = expect_error "definition" j

decodeType :: JSExpression -> BaseType
decodeType (JSIdentifier _ "uint256") = AT_UInt256
decodeType (JSIdentifier _ "bool") = AT_Bool
decodeType (JSIdentifier _ "bytes") = AT_Bytes
decodeType j = expect_error "type" j

decodeVarDecl :: JSObjectProperty -> (TP, XLVar, BaseType)
decodeVarDecl (JSPropertyNameandValue (JSPropertyIdent _ v) a [ e ]) = ((tp a), v, bt)
  where bt = decodeType e
decodeVarDecl j = expect_error "variable declaration" j

decodeBody :: ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP)) -> JSModuleItem -> IO ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP))
decodeBody (d, p, me) (JSModuleStatementListItem s@(JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ _x) (JSVarInit _ (JSMemberExpression (JSIdentifier _ "Enum") _ _ _)))) _)) =
  return $ (d ++ decodeDef s, p, me)
decodeBody (d, p, me) (JSModuleStatementListItem j@(JSFunction _ (JSIdentName _ "main") _ (JSLNil) _ body _)) =
  case me of
    Nothing -> return $ (d, p, Just (decodeBlock body))
    Just _ -> expect_error "only one main function" j 
decodeBody (d, p, me) (JSModuleStatementListItem s@(JSFunction _ _ _ _ _ _ _)) =
  return $ (d ++ decodeDef s, p, me)
decodeBody (d, p, me) (JSModuleStatementListItem (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ who) (JSVarInit _ (JSMemberExpression (JSIdentifier a "participant") _ (JSLOne (JSObjectLiteral _ vs _)) _)))) _)) =
  return $ (d, p', me)
  where p' = M.insert who ((tp a), ds) p
        ds = map decodeVarDecl $ flattenJSCTL vs
--- XXX import
decodeBody (_d, _p, _me) j = expect_error "body element" j

decodeXLProgram :: JSAST -> IO (XLProgram TP)
decodeXLProgram (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach/exe\'") _)):j) a) = do
  init_defs <- stdlib_defs
  (d, p, Just b) <- foldM decodeBody (init_defs, M.empty, Nothing) j
  return $ XL_Prog (tp a) d p b
decodeXLProgram j = expect_error "program" j

decodeXLLibrary :: JSAST -> IO [XLDef TP]
decodeXLLibrary (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach/lib\'") _)):j) a) = do
  (d, p, mm) <- foldM decodeBody ([], M.empty, Nothing) j
  (case mm of
     Nothing -> return ()
     _ -> expect_error "library has no main" a)
  (if M.null p then
     return ()
    else
     expect_error "library has no participants" a)
  return $ d
decodeXLLibrary j = expect_error "library" j

stdlib_defs :: IO [XLDef TP]
stdlib_defs = decodeXLLibrary $ readJsModule $ (B.unpack $(embedFile "../reach/stdlib.reach"))

parseJsModule :: FilePath -> IO JSAST
parseJsModule f = do
  setLocaleEncoding utf8
  s <- readFile f
  return $ readJsModule s

readReachFile :: FilePath -> IO (XLProgram TP)
readReachFile srcp =
  withCurrentDirectory (takeDirectory srcp)
  (do js <- parseJsModule (takeFileName srcp)
      decodeXLProgram js)
