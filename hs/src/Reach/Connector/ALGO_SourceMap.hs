{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Connector.ALGO_SourceMap where

import Control.Monad.Extra
import Data.Aeson ((.:))
import qualified Data.Aeson as AS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Reach.VLQ as VLQ

data SourceMapV3 = SourceMapV3
  { sm_mappings :: T.Text
  }

instance AS.FromJSON SourceMapV3 where
  parseJSON = AS.withObject "SourceMapV3" $ \o -> do
    sm_mappings <- o .: "mapping"
    return $ SourceMapV3 {..}

type SourceMap = M.Map Integer Integer
type CodeAndMap = (BS.ByteString, SourceMap)

interpSourceMap :: SourceMapV3 -> IO SourceMap
interpSourceMap (SourceMapV3 {..}) = do
  let ms = BSL.fromStrict $ B.pack $ T.unpack sm_mappings
  lr <- newIORef 2
  mr <- newIORef mempty
  forM_ (zip [0..] $ BSL.split (BI.c2w ';') ms) $ \(i, group) -> do
    let segs = BSL.split (BI.c2w ',') group
    seg1' <- case segs of
      [] -> return 0
      seg1_b64 : _ -> do
        let seg1_bs = BSL.toStrict seg1_b64
        seg1 <- VLQ.decode' $ BSL.fromStrict seg1_bs
        return $ fromIntegral seg1
    modifyIORef lr $ (+) seg1'
    l <- readIORef lr
    modifyIORef mr $ M.insert i l
  readIORef mr

readSourceMapFile :: FilePath -> IO (Either BS.ByteString SourceMap)
readSourceMapFile fp = do
  AS.eitherDecodeFileStrict fp >>= \case
    Left m -> return $ Left $ B.pack $ "could not decode source map: " <> m
    Right sm -> Right <$> interpSourceMap sm
