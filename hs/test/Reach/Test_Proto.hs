module Reach.Test_Proto
  ( spec_proto
  , main
  ) where

import Network.HTTP.Client
import Reach.Proto
import Servant.API
import Servant.Client
import Test.Hspec
import qualified Network.Wai.Handler.Warp as Warp

-- TODO leverage `servant-quickcheck` package?

main :: IO ()
main = hspec spec_proto

spec_proto :: Spec
spec_proto = around (Warp.testWithApplication (pure . appStubServer False $ Just "sleep 2 && echo yes")) $ do
  u <- runIO $ parseBaseUrl "http://localhost"
  m <- runIO $ newManager defaultManagerSettings
  let env p = mkClientEnv m $ u { baseUrlPort = p }

  describe "Requesting `reach compile`" $ do
    it "should return a `Res` (which we'll flesh out later)" $ \p ->
      runClientM (cmd'' "compile" $ Req [] Nothing mempty) (env p) >>= (`shouldBe` Right Res)

  describe "`say`ing some freeform text to a `PID`" $ do
    it "should succeed" $ \p ->
      runClientM (say'' "1024" "pipe me through `PID`'s stdin") (env p) >>= (`shouldBe` Right NoContent)
