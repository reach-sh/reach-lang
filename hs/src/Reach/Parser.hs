{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Reach.Parser
  ( readReachFile
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad
import Control.Monad.Trans
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.FileEmbed

import Reach.AST

type Parser = ParsecT Void String IO

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

exact :: String -> Parser ()
exact s = do
  _ <- L.symbol sc s
  return ()

semi :: Parser ()
semi = exact ";"

comma :: Parser ()
comma = exact ","

integer :: Parser Integer
integer = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = do s <- char '"' >> manyTill L.charLiteral (char '"')
                   sc
                   return s

parens :: Parser a -> Parser a
parens = between (exact "(") (exact ")")

braces :: Parser a -> Parser a
braces = between (exact "{") (exact "}")

rws :: [String] -- list of reserved words
rws = ["if","then","else","import","true","false","msgcons","msgcar","msgcdr","bytes_equal","length","uint256_bytes","digest","random","interact","assert!","assume!","require!","possible?","values","transfer!","<-","const","function","participant","@","uint256","bool","bytes","publish!","w/","commit","pay!"]

parseBaseType :: Parser BaseType
parseBaseType =
  (AT_UInt256 <$ exact "uint256")
  <|> (AT_Bool <$ exact "bool")
  <|> (AT_Bytes <$ exact "bytes")

parseXLVar :: Parser XLVar
parseXLVar = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

parseXLVars :: Parser [XLVar]
parseXLVars = sepBy parseXLVar comma

parseConstant :: Parser Constant
parseConstant =
  (Con_I <$> integer)
  <|> (Con_B True <$ exact "true")
  <|> (Con_B False <$ exact "false")
  <|> ((Con_BS . B.pack) <$> stringLiteral)

parseParticipant :: Parser Participant
parseParticipant = parseXLVar

parseXLBinOp :: Parser EP_Prim
parseXLBinOp =
  (CP ADD <$ exact "+")
  <|> (CP SUB <$ exact "-")
  <|> (CP MUL <$ exact "*")
  <|> (CP DIV <$ exact "/")
  <|> (CP MOD <$ exact "%")
  <|> (CP PLE <$ exact "<=")
  <|> (CP PLT <$ exact "<")
  <|> (CP PEQ <$ exact "==")
  <|> (CP PGE <$ exact ">=")
  <|> (CP PGT <$ exact ">")

parseXLFunOp :: Parser EP_Prim
parseXLFunOp =
  (CP BCAT <$ exact "msgcons")
  <|> (CP BCAT_LEFT <$ exact "msgcar")
  <|> (CP BCAT_RIGHT <$ exact "msgcdr")
  <|> (CP BYTES_EQ <$ exact "bytes_equal")
  <|> (CP BYTES_LEN <$ exact "length")
  <|> (CP UINT256_TO_BYTES <$ exact "uint256_bytes")
  <|> (CP DIGEST <$ exact "digest")
  <|> (CP BALANCE <$ exact "balance")
  <|> (RANDOM <$ exact "random")
  <|> (INTERACT <$ exact "interact")

parseXLStdLibFun :: Parser XLVar
parseXLStdLibFun =
  ("not" <$ exact "not")

parseXLStdLibOp :: Parser XLVar
parseXLStdLibOp =
  ("implies" <$ exact "=>")
  <|> ("or" <$ exact "||")
  <|> ("and" <$ exact "&&")

parseXLPrimApp :: Parser XLExpr
parseXLPrimApp =
  (do pr <- parseXLFunOp
      args <- parens $ parseXLExprs
      return $ XL_PrimApp pr args)
  <|> (do pr <- parseXLStdLibFun
          args <- parens $ parseXLExprs
          return $ XL_FunApp pr args)
  <|> (parens $
  do left <- parseXLExpr1
     ((do exact "?"
          te <- parseXLExpr1
          exact ":"
          fe <- parseXLExpr1
          return $ XL_PrimApp (CP IF_THEN_ELSE) [ left, te, fe ])
      <|> (do pr <- parseXLBinOp
              right <- parseXLExpr1
              return $ XL_PrimApp pr [left, right])
      <|> (do pr <- parseXLStdLibOp
              right <- parseXLExpr1
              return $ XL_FunApp pr [left, right])))

parseXLIf :: Parser XLExpr
parseXLIf = do
  exact "if"
  ce <- parseXLExpr1
  te <- parseXLExpr1
  exact "else"
  fe <- parseXLExpr1
  return $ XL_If False ce te fe

parseXLWhile :: Maybe Participant -> Parser XLExpr
parseXLWhile who = do
  exact "do"
  exact "const"
  loop_v <- parseXLVar
  exact "="
  init_e <- parseXLExpr1
  exact "until"
  stop_e <- parseXLExpr1
  exact "invariant"
  invariant_e <- parseXLExpr1
  body_e <- parseXLExpr1
  semi
  k <- parseXLExprT who
  return $ XL_While loop_v init_e stop_e invariant_e body_e k

parseClaimType :: Parser ClaimType
parseClaimType =
  (CT_Assert <$ exact "assert!")
  <|> (CT_Assume <$ exact "assume!")
  <|> (CT_Require <$ exact "require!")
  <|> (CT_Possible <$ exact "possible?")

parseXLClaim :: Parser XLExpr
parseXLClaim = do
  ct <- parseClaimType
  xe <- parseXLExpr1
  return $ XL_Claim ct xe

parseXLValues :: Parser XLExpr
parseXLValues = do
  exact "values"
  XL_Values <$> parseXLExprs

parseXLTransfer :: Parser XLExpr
parseXLTransfer = do
  exact "transfer!"
  p <- parseParticipant
  exact "<-"
  xe <- parseXLExpr1
  return $ XL_Transfer p xe

parseXLDeclassify :: Parser XLExpr
parseXLDeclassify =
  do exact "declassify"
     xe <- parens $ parseXLExpr1
     return $ XL_Declassify xe

parseXLFunApp :: Parser XLExpr
parseXLFunApp = do
  f <- parseXLVar
  args <- parens $ parseXLExprs
  return $ XL_FunApp f args

parseXLExpr1 :: Parser XLExpr
parseXLExpr1 =
  label "XLExpr1"
  ((XL_Con <$> parseConstant)
   <|> parseXLPrimApp
   <|> parseXLIf
   <|> parseXLClaim
   <|> parseXLValues
   <|> parseXLTransfer
   <|> parseXLDeclassify
   <|> (braces $ parseXLExprT Nothing)
   <|> try parseXLFunApp
   <|> (XL_Var <$> parseXLVar))

parseXLExprs :: Parser [XLExpr]
parseXLExprs = sepBy parseXLExpr1 comma

parseXLToConsensus :: Participant -> Parser XLExpr
parseXLToConsensus who = do
  vs <- ((do exact "publish!"
             vs <- parseXLVars
             exact "w/"
             return vs)
         <|>
         (do exact "pay!"
             return []))
  amount <- parseXLExpr1
  semi
  conk <- parseXLExprT Nothing
  return $ XL_ToConsensus who vs amount (XL_Let Nothing Nothing (XL_Claim CT_Require (XL_PrimApp (CP PEQ) [ (XL_PrimApp (CP TXN_VALUE) []), amount ])) conk)

parseAt :: Parser XLExpr
parseAt = do
  exact "@"
  who <- parseParticipant
  (parseXLToConsensus who <|> parseXLExprT (Just who))

parseXLFromConsensus :: Parser XLExpr
parseXLFromConsensus = do
  exact "commit"
  semi
  k <- parseXLExprT Nothing
  return $ XL_FromConsensus k

parseXLDeclassifyBang :: Maybe Participant -> Parser XLExpr
parseXLDeclassifyBang who =
  do exact "declassify!"
     v <- parseXLVar
     semi
     k <- parseXLExprT who
     return $ XL_Let who (Just [v]) (XL_Declassify (XL_Var v)) k

parseXLLetValues :: Maybe Participant -> Parser XLExpr
parseXLLetValues who = do
  exact "const"
  vs <- parseXLVars
  exact "="
  ve <- parseXLExpr1
  semi
  k <- parseXLExprT who
  return $ XL_Let who (Just vs) ve k

parseXLContinue :: Maybe Participant -> Parser XLExpr
parseXLContinue _who = do
  exact "continue"
  next_e <- parseXLExpr1
  return $ XL_Continue next_e

parseXLExprT :: Maybe Participant -> Parser XLExpr
parseXLExprT who =
  label "XLExprT"
  (parseAt
   <|> parseXLFromConsensus
   <|> parseXLDeclassifyBang who
   <|> parseXLLetValues who
   <|> parseXLContinue who
   <|> parseXLWhile who
   <|> (do before <- parseXLExpr1
           ((do semi
                after <- parseXLExprT who
                return $ XL_Let who Nothing before after)
            <|> (return before))))

parseImport :: Parser [XLDef]
parseImport = do
  exact "import"
  ip <- stringLiteral
  semi
  ds <- liftIO $ readXLLibrary ip
  return ds

parseDefineFun :: Parser [XLDef]
parseDefineFun = do
  exact "function"
  f <- parseXLVar
  args <- parens $ parseXLVars
  e <- ((do exact ":"
            post <- parseXLVar
            body <- parseXLExpr1
            return (XL_Let Nothing (Just ["result"]) body (XL_Let Nothing Nothing (XL_Claim CT_Assert (XL_FunApp post [XL_Var "result"])) (XL_Var "result"))))
         <|> parseXLExpr1)
  return $ [XL_DefineFun f args e]

parseDefine :: Parser [XLDef]
parseDefine = do
  exact "const"
  vs <- parseXLVars
  exact "="
  ve <- parseXLExpr1
  semi
  return [XL_DefineValues vs ve]

parseEnum :: Parser [XLDef]
parseEnum = do
  exact "enum"
  name <- parseXLVar
  vs <- braces $ parseXLVars
  semi
  return $ doXLEnum name vs

doXLEnum :: XLVar -> [XLVar] -> [XLDef]
doXLEnum predv vs = [ dvs, predd ]
  where dvs = XL_DefineValues vs ve
        ve = XL_Values $ zipWith (\_ i -> XL_Con (Con_I i)) vs [0..]
        predd = XL_DefineFun predv [ "x" ] checke
        checke = XL_FunApp "and" [ lee, lte ]
        lee = XL_PrimApp (CP PLE) [ XL_Con (Con_I 0), XL_Var "x" ]
        lte = XL_PrimApp (CP PLT) [ XL_Var "x", XL_Con (Con_I (toInteger (length vs))) ]

parseXLDef :: Parser [XLDef]
parseXLDef = parseImport <|> parseDefineFun <|> parseDefine <|> parseEnum

parseXLDefs :: Parser [XLDef]
parseXLDefs = liftM concat (sepBy parseXLDef sc)

parseXLVarDecl :: Parser (XLVar, BaseType)
parseXLVarDecl = do
  bt <- parseBaseType
  v <- parseXLVar
  return (v, bt)

parseXLPart :: Parser (Participant, [(XLVar, BaseType)])
parseXLPart = do
  exact "participant"
  p <- parseParticipant
  ds <- braces $ sepBy parseXLVarDecl comma
  return (p, ds)

parseXLPartInfo :: Parser XLPartInfo
parseXLPartInfo = M.fromList <$> sepBy parseXLPart sc

parseXLMain :: Parser XLExpr
parseXLMain = do
  exact "main"
  parseXLExpr1

parseXLLibrary :: Parser [XLDef]
parseXLLibrary = do
  sc
  exact "#lang"
  exact "reach/lib"
  parseXLDefs

parseXLProgram :: Parser XLProgram
parseXLProgram = do
  sc
  exact "#lang"
  exact "reach/exe"
  stdlib_defs <- include_stdlib
  defs <- parseXLDefs
  ps <- parseXLPartInfo
  be <- parseXLMain
  return (XL_Prog (stdlib_defs ++ defs) ps be)

readXLProgram :: FilePath -> IO XLProgram
readXLProgram fp = readFile fp >>= runParserT parseXLProgram fp >>= maybeError

readXLLibrary :: FilePath -> IO [XLDef]
readXLLibrary fp = readFile fp >>= runParserT parseXLLibrary fp >>= maybeError

include_stdlib :: Parser [XLDef]
include_stdlib = liftIO $ (runParserT parseXLLibrary "stdlib.reach" (B.unpack $(embedFile "../reach/stdlib.reach")) >>= maybeError)

maybeError :: Either (ParseErrorBundle String Void) a -> IO a
maybeError (Right v) = return v
maybeError (Left peb) = do
  putStrLn $ errorBundlePretty peb
  die "Failed to parse"

readReachFile :: FilePath -> IO XLProgram
readReachFile srcp =
  withCurrentDirectory (takeDirectory srcp) (readXLProgram (takeFileName srcp))
