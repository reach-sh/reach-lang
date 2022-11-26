module Reach.Connector
  ( ConnectorName
  , ConnectorObject
  , ConnectorInfo
  , ConGenConfig (..)
  , Connector (..)
  , Connectors
  , checkIntLiteral
  , checkIntLiteralC
  , conWrite
  , conShowP
  , ConnectorError (..)
  , CCApp
  , ccRead
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Generics.Deriving hiding (conName)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.CL
import Reach.InterferenceGraph
import Reach.Texty
import Reach.Util
import Reach.OutputUtil
import System.Directory

type ConnectorObject = M.Map ConnectorName Value

type ConnectorInfo = Value

data ConGenConfig = ConGenConfig
  { cgOutput :: Outputer
  }

data Connector = Connector
  { conName :: ConnectorName
  , conCons :: DLConstant -> DLLiteral
  , conGen :: ConGenConfig -> IGd CLProg -> IO ConnectorInfo
  , conReserved :: SLVar -> Bool
  , conCompileCode :: Value -> IO (Either String Value)
  , conContractNewOpts :: Maybe Value -> Either String Value
  , conCompileConnectorInfo :: Maybe Value -> Either String Value
  }

instance Eq Connector where
  l == r = conName l == conName r

instance Show Connector where
  show = T.unpack . conName

data ConnectorError
  = Err_IntLiteralRange ConnectorName Integer Integer Integer
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance HasErrorCode ConnectorError where
  errPrefix = const "RC"

  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- If a constructor is obsolete, do NOT delete it nor re-allocate its number.
  -- Add new error codes at the end.
  errIndex = \case
    Err_IntLiteralRange {} -> 0

instance Show ConnectorError where
  show = \case
    Err_IntLiteralRange con rmin x rmax ->
      "integer literal out of range for " <> T.unpack con <> ": " <> show x <> " not in [" <> show rmin <> "," <> show rmax <> "]"

conWriteH :: (String -> a -> IO ()) -> Maybe (T.Text -> String) -> T.Text -> a -> IO ()
conWriteH doWrite moutn which c =
  case moutn of
    Nothing -> return ()
    Just outn -> doWrite (outn which) c

conWrite :: Maybe (T.Text -> String) -> T.Text -> T.Text -> IO ()
conWrite = conWriteH TIO.writeFile

conShowP :: Pretty a => Maybe (T.Text -> String) -> T.Text -> a -> IO ()
conShowP moutn which v =
  conWriteH LTIO.writeFile moutn which (render $ pretty v)

checkIntLiteral :: SrcLoc -> ConnectorName -> Integer -> Integer -> Integer -> Integer
checkIntLiteral at conName rmin rmax x =
  case rmin <= x && x <= rmax of
    True -> x
    False -> expect_thrown at $ Err_IntLiteralRange conName rmin x rmax

checkIntLiteralC :: SrcLoc -> ConnectorName -> (DLConstant -> DLLiteral) -> Integer -> Integer
checkIntLiteralC at conName conCons =
  checkIntLiteral at conName rmin rmax
  where
    rmin = 0
    rmax = case conCons DLC_UInt_max of
      DLL_Int _ _ uim -> uim
      _ -> impossible "uint_max not int"

type Connectors =
  M.Map T.Text Connector

-- conCompileCode helpers

type CCApp = ExceptT String IO

ccRead :: FilePath -> CCApp BS.ByteString
ccRead fp = do
  fpe <- liftIO $ doesFileExist fp
  unless fpe $
    throwE $ "File does not exist: " <> show fp
  liftIO $ BS.readFile fp
