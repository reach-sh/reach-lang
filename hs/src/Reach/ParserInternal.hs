{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Reach.ParserInternal where

import System.Directory
import System.FilePath
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Text.ParserCombinators.Parsec.Number (numberValue)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Control.Monad
import Data.FileEmbed
import GHC.IO.Encoding
import Data.Data
import Test.SmallCheck.Series
import GHC.Generics

import Reach.AST

newtype TP = TP (FilePath, (Maybe TokenPosn))
  deriving (Data)

instance Show TP where
  show (TP (fp, mtp)) = fp ++
    case mtp of
      Nothing -> ""
      Just (TokenPn _ l c) -> ":" ++ show l ++ ":" ++ show c

class ExtractTP a where
  etp :: a -> Maybe TokenPosn

instance ExtractTP TP where
  etp (TP (_, x)) = x

instance ExtractTP JSAnnot where
  etp a = tpa a

instance ExtractTP a => ExtractTP [a] where
  etp [] = Nothing
  etp (a:d) = case etp a of
    Nothing -> etp d
    k -> k

instance ExtractTP a => ExtractTP (Maybe a) where
  etp Nothing = Nothing
  etp (Just a) = etp a

instance ExtractTP JSUnaryOp where
  etp (JSUnaryOpDecr a) = etp a
  etp (JSUnaryOpDelete a) = etp a
  etp (JSUnaryOpIncr a) = etp a
  etp (JSUnaryOpMinus a) = etp a
  etp (JSUnaryOpNot a) = etp a
  etp (JSUnaryOpPlus a) = etp a
  etp (JSUnaryOpTilde a) = etp a
  etp (JSUnaryOpTypeof a) = etp a
  etp (JSUnaryOpVoid a) = etp a

instance ExtractTP JSBinOp where
    etp (JSBinOpAnd a) = etp a
    etp (JSBinOpBitAnd a) = etp a
    etp (JSBinOpBitOr a) = etp a
    etp (JSBinOpBitXor a) = etp a
    etp (JSBinOpDivide a) = etp a
    etp (JSBinOpEq a) = etp a
    etp (JSBinOpGe a) = etp a
    etp (JSBinOpGt a) = etp a
    etp (JSBinOpIn a) = etp a
    etp (JSBinOpInstanceOf a) = etp a
    etp (JSBinOpLe a) = etp a
    etp (JSBinOpLsh a) = etp a
    etp (JSBinOpLt a) = etp a
    etp (JSBinOpMinus a) = etp a
    etp (JSBinOpMod a) = etp a
    etp (JSBinOpNeq a) = etp a
    etp (JSBinOpOf a) = etp a
    etp (JSBinOpOr a) = etp a
    etp (JSBinOpPlus a) = etp a
    etp (JSBinOpRsh a) = etp a
    etp (JSBinOpStrictEq a) = etp a
    etp (JSBinOpStrictNeq a) = etp a
    etp (JSBinOpTimes a) = etp a
    etp (JSBinOpUrsh a) = etp a

instance ExtractTP JSIdent where
  etp (JSIdentName a _) = etp a
  etp JSIdentNone = Nothing

instance ExtractTP JSArrowParameterList where
  etp (JSUnparenthesizedArrowParameter a) = etp a
  etp (JSParenthesizedArrowParameterList a _ _) = etp a

instance ExtractTP JSPropertyName where
  etp (JSPropertyIdent a _) = etp a
  etp (JSPropertyString a _) = etp a
  etp (JSPropertyNumber a _) = etp a
  etp (JSPropertyComputed a _ _) = etp a

instance ExtractTP JSAccessor where
  etp (JSAccessorGet a) = etp a
  etp (JSAccessorSet a) = etp a

instance ExtractTP JSObjectProperty where
  etp (JSPropertyNameandValue a _ _) = etp a
  etp (JSPropertyIdentRef a _) = etp a
  etp (JSObjectMethod a) = etp a

instance ExtractTP JSMethodDefinition where
  etp (JSMethodDefinition a _ _ _ _) = etp a
  etp (JSGeneratorMethodDefinition a _ _ _ _ _) = etp a
  etp (JSPropertyAccessor a _ _ _ _ _) = etp a


instance ExtractTP JSExpression where
  etp (JSIdentifier a _) = etp a
  etp (JSDecimal a _) = etp a
  etp (JSLiteral a _) = etp a
  etp (JSHexInteger a _) = etp a
  etp (JSOctal a _) = etp a
  etp (JSStringLiteral a _) = etp a
  etp (JSRegEx a _) = etp a
  etp (JSArrayLiteral a _ _) = etp a
  etp (JSAssignExpression a _ _) = etp a
  etp (JSAwaitExpression a _) = etp a
  etp (JSCallExpression a _ _ _) = etp a
  etp (JSCallExpressionDot a _ _) = etp a
  etp (JSCallExpressionSquare a _ _ _) = etp a
  etp (JSClassExpression a _ _ _ _ _) = etp a
  etp (JSCommaExpression a _ _) = etp a
  etp (JSExpressionBinary a _ _) = etp a
  etp (JSExpressionParen a _ _) = etp a
  etp (JSExpressionPostfix a _) = etp a
  etp (JSExpressionTernary a _ _ _ _) = etp a
  etp (JSArrowExpression a _ _) = etp a
  etp (JSFunctionExpression a _ _ _ _ _) = etp a
  etp (JSGeneratorExpression a _ _ _ _ _ _) = etp a
  etp (JSMemberDot a _ _) = etp a
  etp (JSMemberExpression a _ _ _) = etp a
  etp (JSMemberNew a _ _ _ _) = etp a
  etp (JSMemberSquare a _ _ _) = etp a
  etp (JSNewExpression a _) = etp a
  etp (JSObjectLiteral a _ _) = etp a
  etp (JSSpreadExpression a _) = etp a
  etp (JSTemplateLiteral a _ _ _) = etp a
  etp (JSUnaryExpression a _) = etp a
  etp (JSVarInitExpression a _) = etp a
  etp (JSYieldExpression a _) = etp a
  etp (JSYieldFromExpression a _ _) = etp a

instance ExtractTP JSStatement where
  etp (JSStatementBlock a _ _ _) = etp a
  etp (JSBreak a _ _) = etp a
  etp (JSLet a _ _) = etp a
  etp (JSClass a _ _ _ _ _ _) = etp a
  etp (JSConstant a _ _) = etp a
  etp (JSContinue a _ _) = etp a
  etp (JSDoWhile a _ _ _ _ _ _) = etp a
  etp (JSFor a _ _ _ _ _ _ _ _) = etp a
  etp (JSForIn a _ _ _ _ _ _) = etp a
  etp (JSForVar a _ _ _ _ _ _ _ _ _) = etp a
  etp (JSForVarIn a _ _ _ _ _ _ _) = etp a
  etp (JSForLet a _ _ _ _ _ _ _ _ _) = etp a
  etp (JSForLetIn a _ _ _ _ _ _ _) = etp a
  etp (JSForLetOf a _ _ _ _ _ _ _) = etp a
  etp (JSForConst a _ _ _ _ _ _ _ _ _) = etp a
  etp (JSForConstIn a _ _ _ _ _ _ _) = etp a
  etp (JSForConstOf a _ _ _ _ _ _ _) = etp a
  etp (JSForOf a _ _ _ _ _ _) = etp a
  etp (JSForVarOf a _ _ _ _ _ _ _) = etp a
  etp (JSAsyncFunction a _ _ _ _ _ _ _) = etp a
  etp (JSFunction a _ _ _ _ _ _) = etp a
  etp (JSGenerator a _ _ _ _ _ _ _) = etp a
  etp (JSIf a _ _ _ _) = etp a
  etp (JSIfElse a _ _ _ _ _ _) = etp a
  etp (JSLabelled a _ _) = etp a
  etp (JSEmptyStatement a) = etp a
  etp (JSExpressionStatement a _) = etp a
  etp (JSAssignStatement a _ _ _) = etp a
  etp (JSMethodCall a _ _ _ _) = etp a
  etp (JSReturn a _ _) = etp a
  etp (JSSwitch a _ _ _ _ _ _ _) = etp a
  etp (JSThrow a _ _) = etp a
  etp (JSTry a _ _ _) = etp a
  etp (JSVariable a _ _) = etp a
  etp (JSWhile a _ _ _ _) = etp a
  etp (JSWith a _ _ _ _ _) = etp a

instance ExtractTP JSModuleItem where
  etp (JSModuleImportDeclaration a _) = etp a
  etp (JSModuleExportDeclaration a _) = etp a
  etp (JSModuleStatementListItem a) = etp a

instance ExtractTP JSAST where
  etp (JSAstProgram _ a) = etp a
  etp (JSAstModule _ a) = etp a
  etp (JSAstStatement _ a) = etp a
  etp (JSAstExpression _ a) = etp a
  etp (JSAstLiteral _ a) = etp a

xtp :: TP -> Maybe TokenPosn
xtp (TP (_, t)) = t

instance ExtractTP (XLExpr TP) where
  etp (XL_Con a _) = xtp a
  etp (XL_Var a _) = xtp a
  etp (XL_Prim a _) = xtp a
  etp (XL_If a _ _ _) = xtp a
  etp (XL_Claim a _ _) = xtp a
  etp (XL_ToConsensus a _ _ _) = xtp a
  etp (XL_FromConsensus a _) = xtp a
  etp (XL_Values a _) = xtp a
  etp (XL_Transfer a _ _) = xtp a
  etp (XL_Declassify a _) = xtp a
  etp (XL_Let a _ _ _ _) = xtp a
  etp (XL_While a _ _ _ _ _ _) = xtp a
  etp (XL_Continue a _) = xtp a
  etp (XL_Interact a _ _ _) = xtp a
  etp (XL_FunApp a _ _) = xtp a
  etp (XL_Lambda a _ _) = xtp a
  etp (XL_Digest a _) = xtp a
  etp (XL_ArrayRef a _ _) = xtp a

instance ExtractTP (XLPartInfo TP) where
  etp x = f $ M.toList x
    where f [] = Nothing
          f ((_,(a,_)):_) = xtp a

data ParseErr
  = PE_HeaderProgram
  | PE_HeaderLibrary
  | PE_LibraryMain
  | PE_LibraryParticipants
  | PE_DoubleMain
  | PE_BodyElement
  | PE_Identifier
  | PE_LetLHS
  | PE_ArrowParams
  | PE_BinaryOp
  | PE_UnaryOp
  | PE_Arg1
  | PE_Expression
  | PE_EmptyBody
  | PE_IfElse
  | PE_AfterReturn
  | PE_ContinueArgs
  | PE_Statement
  | PE_IllegalAt
  | PE_Type
  | PE_VarDecl
  | PE_NoMain
  deriving (Generic, Show)

instance Monad m => Serial m ParseErr

expect_throw :: Data a => ExtractTP a => ParseErr -> FilePath -> a -> b
expect_throw pe fp j = error $ show tp ++ ": " ++ msg ++ " (given: " ++ (show $ toConstr j) ++ ")"
  where tp = TP (fp, (etp j))
        msg = case pe of
          PE_HeaderProgram -> "expected: 'reach 0.1 exe';"
          PE_HeaderLibrary -> "expected: 'reach 0.1 lib';"
          PE_LibraryMain -> "libraries should not have main function"
          PE_LibraryParticipants -> "libraries should not have participants"
          PE_DoubleMain -> "main must occur only once"
          PE_NoMain -> "expected a main function"
          PE_BodyElement -> "expected: participant, main function, definition (constant or library function), or import"
          PE_Identifier -> "expected an identifier"
          PE_LetLHS -> "expected an identifier or array literal of identifiers"
          PE_ArrowParams -> "expected a parenthesized list of identifiers for arrow parameters"
          PE_BinaryOp -> "expected a valid binary operator"
          PE_UnaryOp -> "expected a valid unary operator"
          PE_Arg1 -> "expected a single argument to declassify and claims"
          PE_Expression -> "expected a valid expression form"
          PE_EmptyBody -> "expected a valid terminator statement, such as return or continue"
          PE_IfElse -> "expected an else for if"
          PE_AfterReturn -> "nothing is allowed after a return"
          PE_ContinueArgs -> "expected all loop variables at continue"
          PE_Statement -> "expected a valid statement form"
          PE_IllegalAt -> "expected a participant or anyone"
          PE_Type -> "expected a valid type"
          PE_VarDecl -> "expected a valid variable declaration"

type DecodeStmtsState = (FilePath, Maybe [XLVar], Maybe XLVar)

dss_fp :: DecodeStmtsState -> FilePath
dss_fp (fp, _, _) = fp

make_dss :: FilePath -> DecodeStmtsState
make_dss fp = (fp, Nothing, Nothing)
sub_dss :: DecodeStmtsState -> DecodeStmtsState
sub_dss (fp, lv, _) = (fp, lv, Nothing)

who_dss :: DecodeStmtsState -> Maybe XLVar -> DecodeStmtsState
who_dss (fp, lv, _) who = (fp, lv, who)
dss_who :: DecodeStmtsState -> Maybe XLVar
dss_who (_, _, w) = w

loopvs_dss :: DecodeStmtsState -> [XLVar] -> DecodeStmtsState
loopvs_dss (fp, _, w) lv = (fp, Just lv, w)
dss_loopvs :: DecodeStmtsState -> Maybe [XLVar]
dss_loopvs (_, lv, _) = lv

dss_tp :: DecodeStmtsState -> Maybe TokenPosn -> TP
dss_tp (fp, _, _) mt = TP (fp, mt)

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

expectId :: FilePath -> JSExpression -> XLVar
expectId _ (JSIdentifier _ x) = x
expectId fp j = expect_throw PE_Identifier fp j

decodeLetLHS :: FilePath -> JSExpression -> [XLVar]
decodeLetLHS _ (JSIdentifier _ x) = [x]
decodeLetLHS fp (JSArrayLiteral _ xs _) = map (expectId fp) $ flattenJSArray xs
decodeLetLHS fp j = expect_throw PE_LetLHS fp j

decodeParams :: FilePath -> JSArrowParameterList -> [XLVar]
decodeParams fp j@(JSUnparenthesizedArrowParameter _) = expect_throw PE_ArrowParams fp j
decodeParams fp (JSParenthesizedArrowParameterList _ j _) = map (expectId fp) $ flattenJSCL j

decodeBinOp :: FilePath -> (JSAnnot -> TP) -> JSBinOp -> XLExpr TP -> XLExpr TP -> XLExpr TP
decodeBinOp fp tp o arg1 arg2 =
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
    JSBinOpLsh a -> prim a (CP LSH)
    JSBinOpRsh a -> prim a (CP RSH)
    JSBinOpBitAnd a -> prim a (CP BAND)
    JSBinOpBitOr a -> prim a (CP BIOR)
    JSBinOpBitXor a -> prim a (CP BXOR)
    j -> expect_throw PE_BinaryOp fp j
  where fun a f = XL_FunApp (tp a) (XL_Var (tp a) f) args
        prim a p = XL_FunApp (tp a) (XL_Prim (tp a) p) args
        args = [ arg1, arg2 ]

decodeUnaOp :: FilePath -> (JSAnnot -> TP) -> JSUnaryOp -> XLExpr TP -> XLExpr TP
decodeUnaOp fp tp j arg =
  case j of
    (JSUnaryOpMinus a) -> XL_FunApp (tp a) (XL_Prim (tp a) (CP SUB)) [ (XL_Con (tp a) (Con_I 0)), arg ]
    (JSUnaryOpNot a) -> XL_FunApp (tp a) (XL_Var (tp a) "not") [ arg ]
    _ -> expect_throw PE_UnaryOp fp j

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
    --- No call dot
    (JSCallExpressionSquare ae lb ee _rb) ->
      XL_ArrayRef (tp lb) (decodeExpr dss ae) (decodeExpr dss ee)
    (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier a "transfer") _ (JSLOne amt) _) _ (JSIdentifier _ "to")) _ (JSLOne (JSIdentifier _ p)) _) ->
      XL_Transfer (tp a) p (decodeExpr dss amt)
    --- No comma
    (JSExpressionBinary lhs op rhs) -> (decodeBinOp (dss_fp dss) tp op) (decodeExpr dss lhs) (decodeExpr dss rhs)
    (JSExpressionParen _ j _) -> decodeExpr dss j
    --- No postfix
    (JSExpressionTernary c a t _ f) -> XL_If (tp a) (decodeExpr dss c) (decodeExpr dss t) (decodeExpr dss f)
    (JSArrowExpression params a s) -> XL_Lambda (tp a) (decodeParams (dss_fp dss) params) (decodeStmts (tp a) dss [s])
    --- No function w/ name
    --- No JSMemberDot
    --- XXX/FIXME - Remove this
    (JSMemberDot (JSIdentifier a "txn") _ (JSIdentifier _ "value")) ->
      XL_FunApp (tp a) (XL_Prim (tp a) (CP TXN_VALUE)) []
    --- No JSMemberNew
    (JSMemberSquare ae lb ee _rb) ->
      XL_ArrayRef (tp lb) (decodeExpr dss ae) (decodeExpr dss ee)
    --- No NewExpression
    --- No ObjectLiteral
    --- No SpreadExpression
    (JSUnaryExpression op e) -> (decodeUnaOp (dss_fp dss) tp op) (decodeExpr dss e)
    --- No VarInitExpression
    (JSMemberExpression (JSMemberDot (JSIdentifier a "interact") _ (JSIdentifier _ method)) _ args _) ->
      XL_Interact (tp a) method (XLT_BT (tp a) BT_Bool) (map (decodeExpr dss) $ flattenJSCL args)
    (JSMemberExpression (JSIdentifier a "is") _ (JSLCons (JSLOne te) _ (JSMemberExpression (JSMemberDot (JSIdentifier _ "interact") _ (JSIdentifier _ method)) _ args _)) _) ->
      XL_Interact (tp a) method (decodeType (dss_fp dss) te) (map (decodeExpr dss) $ flattenJSCL args)
    (JSMemberExpression f a eargs _) ->
      case f of
        JSIdentifier _ "assert" -> claim CT_Assert
        JSIdentifier _ "assume" -> claim CT_Assume
        JSIdentifier _ "require" -> claim CT_Require
        JSIdentifier _ "possible" -> claim CT_Possible
        JSIdentifier _ "digest" -> XL_Digest h args
        JSIdentifier _ "balance" -> prim (CP BALANCE)
        JSIdentifier _ "random" -> prim RANDOM
        JSIdentifier _ "declassify" -> XL_Declassify h (arg1 ())
        o -> fun (decodeExpr dss o)
      where fun o = XL_FunApp h o args
            prim p = XL_FunApp h (XL_Prim h p) args
            claim ct = XL_Claim h ct (arg1 ())
            arg1 () = case args of
                        [x] -> x
                        _ -> expect_throw PE_Arg1 (dss_fp dss) je
            h = tp a
            args = (map (decodeExpr dss) $ flattenJSCL eargs)
    j -> expect_throw PE_Expression (dss_fp dss) j
  where tp = dss_tp dss . tpa

data LetNothing
  = LN_Flatten
  | LN_Null
letnothing :: LetNothing -> TP -> DecodeStmtsState -> XLExpr TP -> TP -> [JSStatement] -> XLExpr TP
letnothing flatok t dss e prev ks =
  case ks of
    [] -> case flatok of
      LN_Flatten -> e
      LN_Null -> XL_Let t (dss_who dss) Nothing e (XL_Values t [])
    _ -> XL_Let t (dss_who dss) Nothing e (decodeStmts prev dss ks)

mergeStmts :: JSStatement -> [JSStatement] -> [JSStatement]
mergeStmts (JSStatementBlock _ ss1 _ _) ss2 = ss1 ++ ss2
mergeStmts s ss = s:ss

decodeStmts :: TP -> DecodeStmtsState -> [JSStatement] -> XLExpr TP
decodeStmts prev dss js =
  let ds = decodeStmts in
  case js of
    [] -> expect_throw PE_EmptyBody (dss_fp dss) prev
    ((JSStatementBlock a ss _ preva):k) ->
      letnothing LN_Flatten (tp a) dss (ds (tp a) dss ss) (sp preva) k
    --- No Break
    --- No Let
    ((JSConstant a (JSLOne (JSVarInitExpression v (JSVarInit _ e))) preva):k) ->
      XL_Let (tp a) (dss_who dss) (Just (decodeLetLHS (dss_fp dss) v)) (decodeExpr dss e) (ds (sp preva) dss k)
    --- FIXME Support functions and enums like at top-level?
    --- No DoWhile
    --- No For, ForIn, ForVar, ForVarIn, ForLet, ForLetIn, ForLetOf, ForOf, ForVarOf
    --- No Function
    ((JSFunction a (JSIdentName _ f) _ eargs _ ee preva):k) ->
      XL_Let (tp a) (dss_who dss) (Just [ f ]) (XL_Lambda (tp a) args e) (ds (sp preva) dss k)
      where args = map (expectId (dss_fp dss)) (flattenJSCL eargs)
            e = decodeBlock dss ee
    (j@(JSIf _ _ _ _ _):_k) -> expect_throw PE_IfElse (dss_fp dss) j
    ((JSIfElse a _ cond prev_t true prev_f false):k) ->
      letnothing LN_Flatten (tp a) dss theif tp_pre_k k
      where theif = (XL_If (tp a) (decodeExpr dss cond) (ds (tp prev_t) dss [true]) (ds (tp prev_f) dss [false]))
            tp_pre_k = tp prev_f
    --- No Labelled
    --- No EmptyStatement
    --- No AssignStatement
    --- Publish + Pay + Timeout
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _) _ (JSIdentifier _ "timeout")) _ targs _) preva):ek) ->
      decodeToConsensus a p (Just evs) (Just eamt) (Just targs) (sp preva) ek
    --- Pay + Timeout
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _) _ (JSIdentifier _ "timeout")) _ targs _) preva):ek) ->
      decodeToConsensus a p Nothing (Just eamt) (Just targs) (sp preva) ek
    --- Publish + Timeout
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _) _ (JSIdentifier _ "timeout")) _ targs _) preva):ek) ->
      decodeToConsensus a p (Just evs) Nothing (Just targs) (sp preva) ek
    --- Publish + Pay
    ((JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _) preva):ek) ->
      decodeToConsensus a p (Just evs) (Just eamt) Nothing (sp preva) ek
    --- Pay
    ((JSMethodCall (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "pay")) _ (JSLOne eamt) _ preva):ek) ->
      decodeToConsensus a p Nothing (Just eamt) Nothing (sp preva) ek
    --- Publish
    ((JSMethodCall (JSMemberDot (JSIdentifier a p) _ (JSIdentifier _ "publish")) _ evs _ preva):ek) ->
      decodeToConsensus a p (Just evs) Nothing Nothing (sp preva) ek
    --- Only
    ((JSMethodCall (JSMemberDot (JSIdentifier _a p) _ (JSIdentifier _ "only"))
       _ (JSLOne (JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ s)) _ preva):k) ->
      ds (sp preva) (who_dss dss (Just p)) (mergeStmts s k)
    ((JSMethodCall (JSIdentifier a "commit") _ JSLNil _ preva):k) ->
      XL_FromConsensus (tp a) (ds (sp preva) (sub_dss dss) k)
    ((JSMethodCall f ann1 args ann2 semi):k) ->
      letnothing LN_Null (tp ann1) dss (decodeExpr dss (JSMemberExpression f ann1 args ann2)) (sp semi) k
    (j@(JSReturn a me _):k) ->
      case k of
        [] -> case me of
          Just e -> decodeExpr dss e
          Nothing -> XL_Values (tp a) []
        _ -> expect_throw PE_AfterReturn (dss_fp dss) j
    --- No Switch
    --- No Throw
    --- No Try (yet)
    --- No Variable
    --- No With
    ((JSVariable a (JSLOne (JSVarInitExpression loop_var_je (JSVarInit _ einit_e))) _):(JSMethodCall (JSIdentifier _ "invariant") _ (JSLOne einvariant_e) _ _):(JSWhile _ _ econd_e prev_ebody ebody_e):k) ->
      XL_While h loop_vs (decodeExpr dss einit_e) stop_e (decodeExpr dss einvariant_e) (ds (tp prev_ebody) dss' [ebody_e]) (ds tp_pre_k dss k)
      where dss' = loopvs_dss dss loop_vs
            tp_pre_k = tp prev_ebody --- XXX not the best
            loop_vs = decodeLetLHS (dss_fp dss) loop_var_je
            cond_e = (decodeExpr dss econd_e)
            stop_e = XL_FunApp h (XL_Var h "not") [ cond_e ]
            h = tp a
    ((JSAssignStatement loopvs_je (JSAssign _) rhs _):j@(JSContinue a JSIdentNone _):[]) ->
      let loopvs = decodeLetLHS (dss_fp dss) loopvs_je in
      if Just loopvs == (dss_loopvs dss) then
        XL_Continue (tp a) (decodeExpr dss rhs)
      else
        expect_throw PE_ContinueArgs (dss_fp dss) j
    ((JSExpressionStatement e semi):k) ->
      letnothing LN_Null (sp semi) dss (decodeExpr dss e) (sp semi) k
    (j:_) -> expect_throw PE_Statement (dss_fp dss) j
  where tp = dss_tp dss . tpa
        sp = dss_tp dss . spa
        decodeToConsensus a p mevs meamt metargs prev_ek ek =
          XL_ToConsensus h (p, vs, amt) mto k'
          where k' = XL_Let h Nothing Nothing amt_claim conk
                amt_claim = XL_Claim h CT_Require (XL_FunApp h (XL_Prim h (CP PEQ)) [ (XL_FunApp h (XL_Prim h (CP TXN_VALUE)) []), amt ])
                h = tp a
                vs = case mevs of
                  Nothing -> []
                  Just evs -> map (expectId (dss_fp dss)) $ flattenJSCL evs
                amt = case meamt of
                  Nothing -> XL_Con h (Con_I 0)
                  Just eamt -> decodeExpr dss eamt
                conk = decodeStmts prev_ek (sub_dss dss) ek
                mto = case metargs of
                        Nothing -> Nothing
                        Just etargs -> case flattenJSCL etargs of
                          [ ede, ete ] -> Just (de, te) where
                            de = decodeExpr (sub_dss dss) ede
                            te = XL_FunApp h (decodeExpr (sub_dss dss) ete) []
                          _ -> error "Pattern match fail" -- XXX Nothing?

decodeBlock :: DecodeStmtsState -> JSBlock -> XLExpr TP
decodeBlock dss (JSBlock prev ss _) = decodeStmts (dss_tp dss (tpa prev)) dss ss

decodeDef :: FilePath -> JSStatement -> [XLDef TP]
decodeDef fp j =
  case j of
    (JSConstant a (JSLOne (JSVarInitExpression (JSArrayLiteral _ ((JSArrayElement (JSIdentifier _ x)) : vs) _) (JSVarInit _ (JSMemberExpression (JSIdentifier _ "makeEnum") _ JSLNil _)))) _) ->
      doXLEnum (tp a) x (map (expectId fp) (flattenJSArray vs))
    (JSConstant a (JSLOne (JSVarInitExpression lhs (JSVarInit _ e))) _) ->
      [XL_DefineValues (tp a) (decodeLetLHS fp lhs) $ decodeExpr (make_dss fp) e]
    (JSFunction a (JSIdentName _ f) _ eargs _ ee _) ->
      [XL_DefineFun (tp a) f args e]
      where args = map (expectId fp) (flattenJSCL eargs)
            e = decodeBlock (make_dss fp) ee
    _ -> expect_throw PE_BodyElement fp j
  where tp a = TP (fp, tpa a)

decodeType :: FilePath -> JSExpression -> XLType TP
decodeType fp j =
  case j of
    JSIdentifier a "address" -> XLT_BT (tp a) BT_Address
    JSIdentifier a "uint256" -> XLT_BT (tp a) BT_UInt256
    JSIdentifier a "bool" -> XLT_BT (tp a) BT_Bool
    JSIdentifier a "bytes" -> XLT_BT (tp a) BT_Bytes
    JSMemberSquare left lb unit _rb ->
      XLT_Array (tp lb) left' unit'
      where left' = decodeType fp left
            unit' = decodeExpr (make_dss fp) unit
    _ -> expect_throw PE_Type fp j
  where tp a = TP (fp, tpa a)

decodeVarDecl :: FilePath -> (JSAnnot -> TP) -> JSObjectProperty -> (TP, XLVar, XLType TP)
decodeVarDecl fp tp (JSPropertyNameandValue (JSPropertyIdent _ v) a [ e ]) = ((tp a), v, bt)
  where bt = decodeType fp e
decodeVarDecl fp _ j = expect_throw PE_VarDecl fp j

decodeBody :: FilePath -> ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP)) -> JSModuleItem -> IO ([XLDef TP], (XLPartInfo TP), Maybe (XLExpr TP))
decodeBody fp (d, p, me) msis =
  case msis of
    (JSModuleStatementListItem (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ who) (JSVarInit _ (JSMemberExpression (JSIdentifier a "participant") _ (JSLOne (JSObjectLiteral _ vs _)) _)))) _)) ->
      return $ (d, p', me)
      where p' = M.insert who ((tp a), ds) p
            ds = map (decodeVarDecl fp tp) $ flattenJSCTL vs
    (JSModuleStatementListItem (JSFunction ja (JSIdentName _ "main") _ (JSLNil) _ body _)) ->
      case me of
        Nothing -> return $ (d, p, Just (decodeBlock (make_dss fp) body))
        Just _ -> expect_throw PE_DoubleMain fp ja
    (JSModuleStatementListItem s@(JSConstant _ _ _)) ->
      return $ (d ++ decodeDef fp s, p, me)
    (JSModuleStatementListItem s@(JSFunction _ _ _ _ _ _ _)) ->
      return $ (d ++ decodeDef fp s, p, me)
    (JSModuleImportDeclaration _ (JSImportDeclarationBare _ m _)) -> do
      defs <- readReachLibrary (string_trim_quotes m)
      return $ (d ++ defs, p, me)
    _ -> expect_throw PE_BodyElement fp msis
  where tp a = TP (fp, tpa a)

decodeXLProgram :: FilePath -> JSAST -> IO (XLProgram TP)
decodeXLProgram fp (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach 0.1 exe\'") _)):j) a) = do
  init_defs <- stdlib_defs
  (d, p, mb) <- foldM (decodeBody fp) (init_defs, M.empty, Nothing) j
  case mb of
    Just b ->
      return $ XL_Prog (TP (fp, (tpa a))) d p b
    Nothing ->
      expect_throw PE_NoMain fp j
decodeXLProgram fp j = expect_throw PE_HeaderProgram fp j

decodeXLLibrary :: FilePath -> JSAST -> IO [XLDef TP]
decodeXLLibrary fp (JSAstModule ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach 0.1 lib\'") _)):j) _) = do
  (d, p, mm) <- foldM (decodeBody fp) ([], M.empty, Nothing) j
  (case mm of
     Nothing -> return ()
     Just m -> expect_throw PE_LibraryMain fp m)
  (if M.null p then
     return ()
    else
     expect_throw PE_LibraryParticipants fp p)
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
