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

parseXLPrimApp :: Parser (XLExpr SourcePos)
parseXLPrimApp =
  (do h <- getSourcePos
      pr <- parseXLFunOp
      args <- parens $ parseXLExprs
      return $ XL_PrimApp h pr args)
  <|> (do h <- getSourcePos
          pr <- parseXLStdLibFun
          args <- parens $ parseXLExprs
          return $ XL_FunApp h pr args)
  <|> (parens $
  do h <- getSourcePos
     left <- parseXLExpr1
     ((do exact "?"
          te <- parseXLExpr1
          exact ":"
          fe <- parseXLExpr1
          return $ XL_PrimApp h (CP IF_THEN_ELSE) [ left, te, fe ])
      <|> (do pr <- parseXLBinOp
              right <- parseXLExpr1
              return $ XL_PrimApp h pr [left, right])
      <|> (do pr <- parseXLStdLibOp
              right <- parseXLExpr1
              return $ XL_FunApp h pr [left, right])))

parseXLIf :: Parser (XLExpr SourcePos)
parseXLIf = do
  h <- getSourcePos
  exact "if"
  ce <- parseXLExpr1
  te <- parseXLExpr1
  exact "else"
  fe <- parseXLExpr1
  return $ XL_If h False ce te fe

parseXLWhile :: Maybe Participant -> Parser (XLExpr SourcePos)
parseXLWhile who = do
  h <- getSourcePos
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
  return $ XL_While h loop_v init_e stop_e invariant_e body_e k

parseClaimType :: Parser ClaimType
parseClaimType =
  (CT_Assert <$ exact "assert!")
  <|> (CT_Assume <$ exact "assume!")
  <|> (CT_Require <$ exact "require!")
  <|> (CT_Possible <$ exact "possible?")

parseXLClaim :: Parser (XLExpr SourcePos)
parseXLClaim = do
  h <- getSourcePos
  ct <- parseClaimType
  xe <- parseXLExpr1
  return $ XL_Claim h ct xe

parseXLValues :: Parser (XLExpr SourcePos)
parseXLValues = do
  h <- getSourcePos
  exact "values"
  XL_Values h <$> parseXLExprs

parseXLTransfer :: Parser (XLExpr SourcePos)
parseXLTransfer = do
  h <- getSourcePos
  exact "transfer!"
  p <- parseParticipant
  exact "<-"
  xe <- parseXLExpr1
  return $ XL_Transfer h p xe

parseXLDeclassify :: Parser (XLExpr SourcePos)
parseXLDeclassify =
  do h <- getSourcePos
     exact "declassify"
     xe <- parens $ parseXLExpr1
     return $ XL_Declassify h xe

parseXLFunApp :: Parser (XLExpr SourcePos)
parseXLFunApp = do
  h <- getSourcePos
  f <- parseXLVar
  args <- parens $ parseXLExprs
  return $ XL_FunApp h f args

parseXLExpr1 :: Parser (XLExpr SourcePos)
parseXLExpr1 =
  label "XLExpr1"
  ((do h <- getSourcePos
       XL_Con h <$> parseConstant)
   <|> parseXLPrimApp
   <|> parseXLIf
   <|> parseXLClaim
   <|> parseXLValues
   <|> parseXLTransfer
   <|> parseXLDeclassify
   <|> (braces $ parseXLExprT Nothing)
   <|> try parseXLFunApp
   <|> (do h <- getSourcePos
           XL_Var h <$> parseXLVar))

parseXLExprs :: Parser [(XLExpr SourcePos)]
parseXLExprs = sepBy parseXLExpr1 comma

parseXLToConsensus :: Participant -> Parser (XLExpr SourcePos)
parseXLToConsensus who = do
  h <- getSourcePos
  (vs, amount) <-
    ((do exact "publish!"
         vs <- parseXLVars
         amount <-
           ((do exact "w/"
                parseXLExpr1)
            <|>
            return (XL_Con h (Con_I 0)))
         return (vs, amount))
     <|>
     (do exact "pay!"
         amount <- parseXLExpr1
         return ([], amount)))
  semi
  conk <- parseXLExprT Nothing
  return $ XL_ToConsensus h who vs amount (XL_Let h Nothing Nothing (XL_Claim h CT_Require (XL_PrimApp h (CP PEQ) [ (XL_PrimApp h (CP TXN_VALUE) []), amount ])) conk)

parseAt :: Parser (XLExpr SourcePos)
parseAt = do
  exact "@"
  who <- parseParticipant
  (parseXLToConsensus who <|> parseXLExprT (Just who))

parseXLFromConsensus :: Parser (XLExpr SourcePos)
parseXLFromConsensus = do
  h <- getSourcePos
  exact "commit"
  semi
  k <- parseXLExprT Nothing
  return $ XL_FromConsensus h k

parseXLDeclassifyBang :: Maybe Participant -> Parser (XLExpr SourcePos)
parseXLDeclassifyBang who =
  do h <- getSourcePos
     exact "declassify!"
     vh <- getSourcePos
     v <- parseXLVar
     semi
     k <- parseXLExprT who
     return $ XL_Let h who (Just [v]) (XL_Declassify h (XL_Var vh v)) k

parseXLLetValues :: Maybe Participant -> Parser (XLExpr SourcePos)
parseXLLetValues who = do
  h <- getSourcePos
  exact "const"
  vs <- parseXLVars
  exact "="
  ve <- parseXLExpr1
  semi
  k <- parseXLExprT who
  return $ XL_Let h who (Just vs) ve k

parseXLContinue :: Maybe Participant -> Parser (XLExpr SourcePos)
parseXLContinue _who = do
  h <- getSourcePos
  exact "continue"
  next_e <- parseXLExpr1
  return $ XL_Continue h next_e

parseXLExprT :: Maybe Participant -> Parser (XLExpr SourcePos)
parseXLExprT who =
  label "XLExprT"
  (parseAt
   <|> parseXLFromConsensus
   <|> parseXLDeclassifyBang who
   <|> parseXLLetValues who
   <|> parseXLContinue who
   <|> parseXLWhile who
   <|> (do before <- parseXLExpr1
           ((do h <- getSourcePos
                semi
                after <- parseXLExprT who
                return $ XL_Let h who Nothing before after)
            <|> (return before))))

parseImport :: Parser [XLDef SourcePos]
parseImport = do
  exact "import"
  ip <- stringLiteral
  semi
  ds <- liftIO $ readXLLibrary ip
  return ds

parseDefineFun :: Parser [XLDef SourcePos]
parseDefineFun = do
  h <- getSourcePos
  exact "function"
  f <- parseXLVar
  args <- parens $ parseXLVars
  e <- ((do ah <- getSourcePos
            exact ":"
            post <- parseXLVar
            body <- parseXLExpr1
            return (XL_Let ah Nothing (Just ["result"]) body (XL_Let ah Nothing Nothing (XL_Claim ah CT_Assert (XL_FunApp ah post [XL_Var ah "result"])) (XL_Var ah "result"))))
         <|> parseXLExpr1)
  return $ [XL_DefineFun h f args e]

parseDefine :: Parser [XLDef SourcePos]
parseDefine = do
  h <- getSourcePos
  exact "const"
  vs <- parseXLVars
  exact "="
  ve <- parseXLExpr1
  semi
  return [XL_DefineValues h vs ve]

parseEnum :: Parser [XLDef SourcePos]
parseEnum = do
  h <- getSourcePos
  exact "enum"
  name <- parseXLVar
  vs <- braces $ parseXLVars
  semi
  return $ doXLEnum h name vs

doXLEnum :: a -> XLVar -> [XLVar] -> [XLDef a]
doXLEnum ann predv vs = [ dvs, predd ]
  where dvs = XL_DefineValues ann vs ve
        ve = XL_Values ann $ zipWith (\_ i -> XL_Con ann (Con_I i)) vs [0..]
        predd = XL_DefineFun ann predv [ "x" ] checke
        checke = XL_FunApp ann "and" [ lee, lte ]
        lee = XL_PrimApp ann (CP PLE) [ XL_Con ann (Con_I 0), XL_Var ann "x" ]
        lte = XL_PrimApp ann (CP PLT) [ XL_Var ann "x", XL_Con ann (Con_I (toInteger (length vs))) ]

parseXLDef :: Parser [XLDef SourcePos]
parseXLDef = parseImport <|> parseDefineFun <|> parseDefine <|> parseEnum

parseXLDefs :: Parser [XLDef SourcePos]
parseXLDefs = liftM concat (sepBy parseXLDef sc)

parseXLVarDecl :: Parser (SourcePos, XLVar, BaseType)
parseXLVarDecl = do
  h <- getSourcePos
  bt <- parseBaseType
  v <- parseXLVar
  return (h, v, bt)

parseXLPart :: Parser (Participant, (SourcePos, [(SourcePos, XLVar, BaseType)]))
parseXLPart = do
  h <- getSourcePos
  exact "participant"
  p <- parseParticipant
  ds <- braces $ sepBy parseXLVarDecl comma
  return (p, (h, ds))

parseXLPartInfo :: Parser (XLPartInfo SourcePos)
parseXLPartInfo = M.fromList <$> sepBy parseXLPart sc

parseXLMain :: Parser (XLExpr SourcePos)
parseXLMain = do
  exact "main"
  parseXLExpr1

parseXLLibrary :: Parser [XLDef SourcePos]
parseXLLibrary = do
  sc
  exact "#lang"
  exact "reach/lib"
  parseXLDefs

parseXLProgram :: Parser (XLProgram SourcePos)
parseXLProgram = do
  h <- getSourcePos
  sc
  exact "#lang"
  exact "reach/exe"
  stdlib_defs <- include_stdlib
  defs <- parseXLDefs
  ps <- parseXLPartInfo
  be <- parseXLMain
  return (XL_Prog h (stdlib_defs ++ defs) ps be)

readXLProgram :: FilePath -> IO (XLProgram SourcePos)
readXLProgram fp = readFile fp >>= runParserT parseXLProgram fp >>= maybeError

readXLLibrary :: FilePath -> IO [XLDef SourcePos]
readXLLibrary fp = readFile fp >>= runParserT parseXLLibrary fp >>= maybeError

include_stdlib :: Parser [XLDef SourcePos]
include_stdlib = liftIO $ (runParserT parseXLLibrary "stdlib.reach" (B.unpack $(embedFile "../reach/stdlib.reach")) >>= maybeError)

maybeError :: Either (ParseErrorBundle String Void) a -> IO a
maybeError (Right v) = return v
maybeError (Left peb) = do
  putStrLn $ errorBundlePretty peb
  die "Failed to parse"

readReachFile :: FilePath -> IO (XLProgram SourcePos)
readReachFile srcp =
  withCurrentDirectory (takeDirectory srcp) (readXLProgram (takeFileName srcp))
