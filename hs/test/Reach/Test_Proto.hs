module Reach.Test_Proto
  ( spec_proto
  , main
  ) where

import Network.HTTP.Client
import Reach.Proto
import Safe
import Servant.API
import Servant.Client
import Test.Hspec
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- TODO leverage `servant-quickcheck` package?

main :: IO ()
main = hspec spec_proto

spec_proto :: Spec
spec_proto = around (Warp.testWithApplication app) $ do
  u <- runIO $ parseBaseUrl "http://localhost"
  m <- runIO $ newManager defaultManagerSettings
  let env p = mkClientEnv m $ u { baseUrlPort = p }
  let run p x = runClientM x (env p) >>= either (fail . show) (pure . id)

  describe "Requesting `reach compile`" $ do
    it "with a bogus flag like --nope will fail" $ \p -> do
      Interpret (ExitStderr n t) <- run p . cmd'' "compile" $ Req ["--nope"] Nothing mempty
      n `shouldBe` 1
      headMay (T.splitOn "\n" t) `shouldBe` Just "Invalid option `--nope'"

    it "with a supported flag like --print-keyword-info will succeed" $ \p -> do
      run p (cmd'' "compile" $ Req ["--print-keyword-info"] Nothing mempty) >>= \case
        Interpret (AttachStreamJustListen _ _) -> pure ()
        x -> x `shouldNotBe` x

  describe "`say`ing some freeform text to a `PID`" $ do
    it "should succeed" $ \p ->
      runClientM (say'' "1024" "pipe me through `PID`'s stdin") (env p) >>= (`shouldBe` Right NoContent)

 where
  app = pure . appStubServer stubDispatchReach False $ Just "sleep 2 && echo yes"
