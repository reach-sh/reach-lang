module Reach.Test_Proto
  ( spec_account
  , spec_headerParse
  , spec_proto
  , main
  ) where

import Control.Monad.Except
import Data.Aeson
import Data.IORef
import Network.HTTP.Client
import Reach.Proto
import Safe
import Servant.API
import Servant.Client
import Servant.Types.SourceT
import Test.Hspec
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Client.Streaming as S

-- TODO leverage `servant-quickcheck` package?

main :: IO ()
main = hspec $ do
  spec_account
  spec_headerParse
  spec_proto

type MXP = Maybe XProject
type EXP = Either String XProject

spec_account :: Spec
spec_account = describe "Parsing an `Account`" $ do
  it "<has yet to be specified/CORE-2029>" True

spec_headerParse :: Spec
spec_headerParse = describe "Parsing an" $ do
  describe "X-Reach-Project header" $ do
    it "without a preceding `@` character will fail" $ do
      let x = "Error in $: (line 1, column 1):\nunexpected \"r\"\nexpecting \"@\""
      eitherDecode "\"reach-sh/foo\"" `shouldBe` (Left x :: EXP)

    it "missing a separating `/` character will fail" $ do
      let x = "Error in $: (line 1, column 10):\nunexpected end of input\nexpecting letter or digit or \"-\""
      eitherDecode "\"@reach-sh\"" `shouldBe` (Left x :: EXP)

    it "will fail if non-alphanumerics occur at beginning or end" $ do
      decode "\"@-reach-sh/foo\"" `shouldBe` (Nothing :: MXP)
      decode "\"@reach-sh/foo-\"" `shouldBe` (Nothing :: MXP)
      decode "\"@a/b$\"" `shouldBe` (Nothing :: MXP)

    it "such as `@jay/rsvp` will succeed" $ do
      eitherDecode "\"@jay/rsvp\"" `shouldBe` Right (XProject "jay" "rsvp")

spec_proto :: Spec
spec_proto = do
  let tr = 1
  let tl = 1
  let lr = 0
  let ll = 0
  c@StubConfig {..} <- runIO $ mkStubConfig tr tl lr ll
  let app = pure . appStubServer c stubDispatchReach False $ Just "sleep 2 && echo yes"

  let reset = liftIO $ do
        writeIORef sc_syncToutStore tr
        writeIORef sc_syncToutStage tl
        writeIORef sc_syncLagStore  lr
        writeIORef sc_syncLagStage  ll

  around (\f -> reset *> Warp.testWithApplication app f) $ do
    u <- runIO $ parseBaseUrl "http://localhost"
    m <- runIO $ newManager defaultManagerSettings
    let env p = mkClientEnv m $ u { baseUrlPort = p }
    let run p x = S.withClientM x (env p) . either (fail . show) $ \a ->
              runExceptT (runSourceT a) >>= either (fail . show) pure

    describe "Requesting `reach compile`" $ do
      it "with a bogus flag like --nope will fail" $ \p -> do
        [Interpret (ExitStderr n t)] <- run p $ cmd'' "compile" Nothing $ Req ["--nope"] Nothing mempty
        n `shouldBe` 1
        headMay (T.splitOn "\n" t) `shouldBe` Just "Invalid option `--nope'"

      it "with a supported flag like --print-keyword-info will succeed" $ \p -> do
        [Interpret (AttachStreamJustListen _ _)] <- run p . cmd'' "compile" Nothing $ Req
          ["--print-keyword-info"]
          (Just mempty)
          mempty
        pure ()

    describe "`say`ing some freeform text to a `PID`" $ do
      it "should succeed" $ \p ->
        runClientM (say'' "1024" "pipe me through `PID`'s stdin") (env p) >>= (`shouldBe` Right NoContent)
