{-# LANGUAGE CPP #-}

module Reach.Connector.ALGO (connect_algo, AlgoError (..)) where

import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Crypto.Hash
import Data.Aeson ((.:), (.=), (.:?))
import qualified Data.Aeson as AS
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encodeBase64', decodeBase64)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import Data.Char
import qualified Data.DList as DL
import Data.Foldable
import Data.Function
import Data.IORef
import Data.List (intercalate, sortOn)
import qualified Data.List as List
import Data.List.Extra (enumerate, mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Vector as Vector
import Data.Word
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.CL
import Reach.Connector
import Reach.Connector.ALGO_SourceMap
import Reach.Counter
import Reach.Dotty
import Reach.FixedPoint
import Reach.InterferenceGraph
import Reach.OutputUtil
import qualified Reach.Texty as T
import Reach.Texty (pretty)
import Reach.UnsafeUtil
import Reach.Util
import Reach.Warning
import Safe (atMay)
import Safe.Foldable (maximumMay)
import System.Exit
import System.FilePath
import System.Process.ByteString
import Text.Read
import qualified Reach.Connector.ALGO_Verify as Verify

-- Errors for ALGO

data AlgoError
  = Err_TransferNewToken
  | Err_PayNewToken
  deriving (Eq, ErrorMessageForJson, ErrorSuggestions, Generic)

instance HasErrorCode AlgoError where
  errPrefix = const "RA"
  errIndex = \case
    Err_TransferNewToken {} -> 0
    Err_PayNewToken {} -> 1

instance Show AlgoError where
  show = \case
    Err_TransferNewToken ->
      "Token cannot be transferred within the same consensus step it was created in on Algorand"
    Err_PayNewToken ->
      "Token cannot be paid within the same consensus step it was shared with the contract on Algorand"

type NotifyFm m = LT.Text -> m ()
type NotifyF = LT.Text -> IO ()
type Notify = Bool -> NotifyF

-- General tools that could be elsewhere

-- Our control-flow graphs have the following structure:
--
-- x ---det---> y
--
-- x is a block label
-- y is a block label
-- det are the details of the transition
--
-- These details are the LPEdge structure: they are the functions that are
-- called (the [a]) and the resources used (the b)
--
type LPEdge a b = ([a], b)
--
-- For each x and y, there may be different edges between them, that's why we
-- have the LPChildren structure: it maps y to a set of details
--
type LPChildren a b = M.Map a (S.Set (LPEdge a b))
--
-- Then, we have a map from the x to each of the ys
--
type LPGraph a b = M.Map a (LPChildren a b)

longestPathBetween :: forall b . LPGraph String b -> String -> String -> (b -> Integer) -> IO Integer
longestPathBetween g f d getc = do
  a2d <- fixedPoint $ \_ (i :: M.Map String Integer) -> do
    flip mapM g $ \(tom :: (LPChildren String b)) -> do
      let ext :: String -> LPEdge String b -> Integer
          ext to (cs, r) = getc r + chase to + sum (map chase cs)
          chase :: String -> Integer
          chase to =
            case to == d of
              True -> 0
              False ->
                case M.lookup to i of
                  Nothing -> 0
                  Just c' -> c'
      let ext' :: String -> S.Set (LPEdge String b) -> Integer
          ext' to es = foldl' max 0 $ map (ext to) $ S.toAscList es
      let tom' :: [Integer]
          tom' = map (uncurry ext') $ M.toAscList tom
      return $ foldl' max 0 tom'
  let r2d x = fromMaybe 0 $ M.lookup x a2d
  let pc = r2d f
  let getMaxPath' x =
        case x == d of
          True -> []
          False -> getMaxPath $ List.maximumBy (compare `on` r2d) $ M.keys $ fromMaybe mempty $ M.lookup x g
      getMaxPath x = x : getMaxPath' x
  let _p = getMaxPath f
  return $ pc

budgetAnalyze :: LPGraph String ResourceCost -> String -> String -> (ResourceCost -> Resource -> Integer) -> IO (Bool, Integer, Integer)
budgetAnalyze g s e getc = do
  -- This function returns...
  --   Bool    --- Are we over budget
  --   Integer --- How much cost we spent
  --   Integer --- How much budget we had
  --
  -- c in this function always stands for the cost
  -- b always stands for the budget
  let from c b l = do
        -- What is the budget spend from l to e?
        case l == e of
          True ->
            -- If l is e, we just figured it out!
            return $ (False, c, b)
          False ->
            -- We look where g can go...
            froml ("has no node in the CFG" <> show (M.keys g)) from1 l c b $ M.toAscList $ fromMaybe mempty $ M.lookup l g
      froml :: forall a . String -> (String -> Integer -> Integer -> a -> IO (Bool, Integer, Integer)) -> String -> Integer -> Integer -> [a] -> IO (Bool, Integer, Integer)
      froml el f l c b = \case
        -- We error if l can't go anywhere... because that's impossible...
        -- there's an invariant that every label can get to BOT
        --
        -- If this error happens, basically it means that the CFG restriction
        -- removed something it shouldn't
        [] -> return $ impossible $ "budgetAnalyze null: " <> show l <> " " <> el
        -- Otherwise, we view the next places in order and combine the analyses
        [x] -> f l c b x
        x : xs -> cbas (f l c b x) (froml el f l c b xs)
      cbas m1 m2 = do
        -- _C_om_B_ine the _A_nalyse_S_
        --
        -- We return whichever one errors first and if they both don't error,
        -- then we return the one that has the bigger budget/cost
        m1 >>= \case
          r@(True, _, _) -> return r
          r1@(False, c1, b1) ->
            m2 >>= \case
              r@(True, _, _) -> return r
              r2@(False, c2, b2) -> do
                case b1 > b2 of
                  True -> return r1
                  False ->
                    case c1 > c2 of
                      True -> return r1
                      False -> return r2
      from1 ol c b (l, es) =
        -- We look through each of the edges
        froml ("has no edges in " <> show ol <> "->" <> show l) from1e l c b $ S.toAscList es
      from1e l c b (cs, r) = do
        -- When we get to an edge, we look at the current budget
        let gr = getc r
        let c' = c + gr R_Cost
        let b' = b + gr R_Budget
        case c' > b' of
          True -> return (True, c', b')
          -- If it is okay, then we go through its calls
          False -> fromcs c' b' l cs
      fromcs c b k = \case
        [] -> from c b k
        x : xs ->
          from c b x >>= \case
            r@(True, _, _) -> return r
            (False, c', b') ->
              fromcs c' b' k xs
  from 0 0 s

-- This function's job is to take the control-flow graph for the whole program
-- and return just the part of it that is connected to the given node, `n`
--
-- The point is to get a piece of the graph so we can figure out how expensive
-- individual API calls are
restrictGraph :: forall a b . (Show a, Ord a, Ord b) => LPGraph a b -> a -> IO (LPGraph a b)
restrictGraph g n = do
  -- The function has two parts...
  loud $ "restrict " <> show n
  -- First, we discover the set of nodes that we want to include
  ns <- connectedWith g n
  -- putStrLn $ "--> ns = " <> show ns
  -- Second, we construct the subgraph that contains only those nodes
  subgraph g ns

connectedWith :: forall a b . (Show a, Ord a, Ord b) => LPGraph a b -> a -> IO (S.Set a)
connectedWith g n = do
  -- This function is going to find the set of nodes that come FROM the n and
  -- those that go TO it
  --
  --    z
  --    |
  --    x   y
  --     \ /
  --      n
  --     / \
  --    a   b
  --        |
  --        c
  --
  -- {x, y, z}  goes   TO n
  -- {a, b, c} comes FROM n
  --
  loud $ "connectedWith " <> show n
  -- We do this by doing a fixed-point computation of these two sets
  (from, to) <- fixedPoint $ \i ((from0 :: S.Set a), (to0 :: S.Set a)) -> do
    loud $ show i
    -- putStrLn $ "        #" <> show i
    -- putStrLn $ "  FROM = " <> show from0
    -- putStrLn $ "    TO = " <> show to0
    -- We put n into the TO set and FROM sets
    let to1 = S.insert n to0
    let from1 = S.insert n from0
    -- We go through the graph and decide what should be in the TO set
    let to2 = mconcatMap inclTo $ M.toAscList g
        inclTo :: (a, (LPChildren a b)) -> S.Set a
        inclTo (z, cs) = realIncl
          -- z gets included if it goes to x which is already in the TO set
          where
            realIncl = if shouldIncld then incl else mempty
            shouldIncld = not $ S.null nextInToS
            nextInToS = S.intersection (gcNodesAsSet cs) to1
            incl = S.union me nextInToCallsS
            me = S.singleton z
            -- We include this node and anything that it calls in its path to
            -- to n by looking to see if n is connected to a TO
            nextInToCallsS = mconcat nextInToCallsL
            nextInToCallsL = map gcEdgeCalls nextInToEdgeL
            nextInToEdgeL = M.elems $ M.restrictKeys cs nextInToS
    -- Now we do the FROM side
    let from2 = mconcatMap inclFrom $ M.toAscList g
        inclFrom :: (a, (LPChildren a b)) -> S.Set a
        inclFrom (b, cs) =
          -- c gets included if it is pointed to by something in FROM (ie b)
          -- We do this by looking if b is in FROM then adding its dests
          case S.member b from1 of
            True -> gcDestsAsSet cs
            False -> mempty
    return (from2, to2)
  return $ S.insert n $ S.union from to

gcDestsAsSet :: forall a b . (Ord a, Ord b) => LPChildren a b -> S.Set a
gcDestsAsSet cs = gcNodesAsSet cs <> gcCallsAsSet cs

gcNodesAsSet :: forall a b . LPChildren a b -> S.Set a
gcNodesAsSet = M.keysSet

gcCallsAsSet :: forall a b . (Ord a, Ord b) => LPChildren a b -> S.Set a
gcCallsAsSet = gcEdgeCalls . mconcat . M.elems

gcEdgeCalls :: forall a b . (Ord a) => (S.Set (LPEdge a b)) -> S.Set a
gcEdgeCalls = mconcatMap (S.fromList . fst) . S.toList

subgraph :: forall a b . (Ord a) => LPGraph a b -> S.Set a -> IO (LPGraph a b)
subgraph g ns = do
  -- This removes everything from g that is not in ns
  let inSet = flip S.member ns
  let removeDisconnected = M.filterWithKey $ \k _v -> inSet k
  -- We first filter all of the nodes, then all of their edges
  return $ M.map removeDisconnected $ removeDisconnected g

ensureAllPaths :: (Show a, Ord a) => String -> LPGraph a b -> a -> a -> (b -> Integer) -> IO (Maybe [a])
ensureAllPaths rlab g s e getc = checkFrom 0 mempty s
  where
    checkFrom t p l = do
      loud $ rlab <> " " <> show l
      when (elem l p) $ do
        impossible $ "loop: " <> show l
      case l == e of
        True ->
          case t == 1 of
            True -> return $ Nothing
            False -> return $ Just p
        False ->
          checkChildren t (l : p) $ M.toAscList $ fromMaybe mempty $ M.lookup l g
    checkChildren t p = \case
      [] -> return $ Nothing
      (d, x) : xs -> checkEdges t p d (S.toAscList x) `cmb` checkChildren t p xs
    checkEdges t p d = \case
      [] -> return $ Nothing
      x : xs -> checkEdge t p d x `cmb` checkEdges t p d xs
    checkEdge t p d (_cs, r) =
      checkFrom (t + getc r) p d
    cmb mx my = do
      mx >>= \case
        Just x -> return $ Just x
        Nothing -> my

aarray :: [AS.Value] -> AS.Value
aarray = AS.Array . Vector.fromList

aobject :: M.Map T.Text AS.Value -> AS.Value
aobject = aesonObject . M.toAscList

mergeIORef :: IORef a -> (a -> a -> a) -> IORef a -> IO ()
mergeIORef dst f src = do
  srca <- readIORef src
  modifyIORef dst $ f srca

type ErrorSet = S.Set LT.Text
type ErrorSetRef = IORef ErrorSet
bad_io :: ErrorSetRef -> NotifyF
bad_io x = modifyIORef x . S.insert
newErrorSetRef :: IO (ErrorSetRef, NotifyF)
newErrorSetRef = do
  r <- newIORef mempty
  return (r, bad_io r)

-- Algorand constants

conName' :: T.Text
conName' = "ALGO"

conCons' :: DLConstant -> DLLiteral
conCons' = \case
  DLC_UInt_max  -> DLL_Int sb UI_Word $ 2 ^ (64 :: Integer) - 1
  DLC_Token_zero -> DLL_Int sb UI_Word $ 0

appLocalStateNumUInt :: Integer
appLocalStateNumUInt = 0

appLocalStateNumBytes :: Integer
appLocalStateNumBytes = 0

appGlobalStateNumUInt :: Integer
appGlobalStateNumUInt = 0

appGlobalStateNumBytes :: Integer
appGlobalStateNumBytes = 1

algoMaxStringSize :: Integer
algoMaxStringSize = 4096

algoMaxGlobalSchemaEntries :: Integer
algoMaxGlobalSchemaEntries = 64

algoMaxGlobalSchemaEntries_usable :: Integer
algoMaxGlobalSchemaEntries_usable = algoMaxGlobalSchemaEntries - appGlobalStateNumBytes

algoMaxAppBytesValueLen :: Integer
algoMaxAppBytesValueLen = 128

algoMaxAppBytesValueLen_usable :: Integer
algoMaxAppBytesValueLen_usable =
  -- We guarantee that every key is exactly one byte, so all the rest of the
  -- space goes to the value
  algoMaxAppBytesValueLen - 1

algoMaxAppTotalArgLen :: Integer
algoMaxAppTotalArgLen = 2048

algoMinimumBalance :: Integer
algoMinimumBalance = 100000

algoMaxTxGroupSize :: Integer
algoMaxTxGroupSize = 16

-- not actually the limit, but this is the name of the variable in the
-- consensus configuration
algoMaxInnerTransactions :: Integer
algoMaxInnerTransactions = 16

algoMaxAppTxnAccounts :: Integer
algoMaxAppTxnAccounts = 4

algoMaxAppBoxReferences :: Integer
algoMaxAppBoxReferences = 8

algoMaxAppKeyLen :: Integer
algoMaxAppKeyLen = 64

algoMaxBoxSize :: Integer
algoMaxBoxSize = 32768

reachMaxBoxSize :: Integer
reachMaxBoxSize = 1024

algoBoxFlatMinBalance :: Integer
algoBoxFlatMinBalance = 2500

algoBoxByteMinBalance :: Integer
algoBoxByteMinBalance = 400

algoMaxAppTxnForeignAssets :: Integer
algoMaxAppTxnForeignAssets = 8
algoMaxAppTxnForeignApps :: Integer
algoMaxAppTxnForeignApps = 8
algoMaxAppTotalTxnReferences :: Integer
algoMaxAppTotalTxnReferences = 8

algoMaxAppProgramCost :: Integer
algoMaxAppProgramCost = 700

-- We're making up this name. It is not in consensus.go, but only in the docs
algoMaxLogLen :: Integer
algoMaxLogLen = 1024

algoMaxLogCalls :: Integer
algoMaxLogCalls = 32

algoMaxAppProgramLen :: Integer
algoMaxAppProgramLen = 2048

algoMaxExtraAppProgramPages :: Integer
algoMaxExtraAppProgramPages = 3

algoMaxAppProgramLen_really :: Integer
algoMaxAppProgramLen_really = (1 + algoMaxExtraAppProgramPages) * algoMaxAppProgramLen

minimumBalance_l :: DLLiteral
minimumBalance_l = DLL_Int sb UI_Word algoMinimumBalance

tealVersionPragma :: LT.Text
tealVersionPragma = "#pragma version 8"

-- Algo specific stuff

extraPages :: Integral a => a -> Integer
extraPages totalLen = ceiling ((fromIntegral totalLen :: Double) / fromIntegral algoMaxAppProgramLen) - 1

data AppInfo = AppInfo
  { ai_GlobalNumUint :: Integer
  , ai_GlobalNumByteSlice :: Integer
  , ai_LocalNumUint :: Integer
  , ai_LocalNumByteSlice :: Integer
  , ai_ExtraProgramPages :: Integer
  }

data ApplTxnType
  = ApplTxn_Create
  | ApplTxn_OptIn

minimumBalance_app :: AppInfo -> ApplTxnType -> Integer
minimumBalance_app (AppInfo {..}) = \case
  ApplTxn_Create ->
    100000*(1+ai_ExtraProgramPages) + (25000+3500)*ai_GlobalNumUint + (25000+25000)*ai_GlobalNumByteSlice
  ApplTxn_OptIn ->
    100000 + (25000+3500)*ai_LocalNumUint + (25000+25000)*ai_LocalNumByteSlice

maxTypeSize_ :: M.Map a DLType -> Maybe Integer
maxTypeSize_ m = do
  ts <- mapM typeSizeOf_ $ M.elems m
  return $ fromMaybe 0 $ maximumMay ts

typeSig_ :: Bool -> Bool -> DLType -> App String
typeSig_ addr2acc isRet = \case
  -- XXX hack until algosdk is fixed
  T_Null -> r $ if isRet then "void" else "byte[0]"
  T_Bool -> r "byte" -- "bool"
  T_UInt UI_Word -> r "uint64"
  T_UInt UI_256 -> r "uint256"
  T_Bytes sz -> r $ "byte" <> array sz
  T_BytesDyn -> r "bytes"
  T_StringDyn -> r "string"
  T_Digest -> r "digest"
  T_Address -> r $ if addr2acc then "account" else "address"
  T_Contract -> typeSig $ T_UInt UI_Word
  T_Token -> typeSig $ T_UInt UI_Word
  T_Array t sz -> do
    s <- typeSig t
    r $ s <> array sz
  T_Tuple ts -> do
    s <- mapM typeSig ts
    r $ "(" <> intercalate "," s <> ")"
  T_Object m -> typeSig $ T_Tuple $ M.elems m
  T_Data m -> do
    m' <- maxTypeSize m
    r $ "(byte,byte" <> array m' <> ")"
  T_Struct ts -> typeSig $ T_Tuple $ map snd ts
  where
    r = return
    -- The ABI allows us to do this, but we don't know how to do in the remote
    -- call generator
    -- rec = typeSig_ addr2acc False
    array sz = "[" <> show sz <> "]"

typeSig :: DLType -> App String
typeSig = typeSig_ False False

typeSizeOf_ :: DLType -> Maybe Integer
typeSizeOf_ = \case
  T_Null -> r 0
  T_Bool -> r 1
  T_UInt UI_Word -> r word
  T_UInt UI_256 -> r 32
  T_Bytes sz -> r sz
  T_BytesDyn -> Nothing
  T_StringDyn -> Nothing
  T_Digest -> r 32
  T_Address -> r 32
  T_Contract -> typeSizeOf_ $ T_UInt UI_Word
  T_Token -> typeSizeOf_ $ T_UInt UI_Word
  T_Array t sz -> (*) sz <$> typeSizeOf_ t
  T_Tuple ts -> sum <$> mapM typeSizeOf_ ts
  T_Object m -> sum <$> (mapM typeSizeOf_ $ M.elems m)
  T_Data m -> (+) 1 <$> maxTypeSize_ m
  T_Struct ts -> sum <$> mapM (typeSizeOf_ . snd) ts
  where
    r = return
    word = 8

maybeOrDynType :: Monad m => NotifyFm m -> a -> Maybe a -> m a
maybeOrDynType notify d mx =
  case mx of
    Just x -> return x
    Nothing -> do
      notify $ "Uses a dynamically sized type, like BytesDyn or StringDyn."
      return d

typeSizeOf__ :: Monad m => NotifyFm m -> DLType -> m Integer
typeSizeOf__ notify t = maybeOrDynType notify 32 (typeSizeOf_ t)

typeSizeOf :: DLType -> App Integer
typeSizeOf = typeSizeOf__ bad_nc

maxTypeSize :: M.Map a DLType -> App Integer
maxTypeSize m = maybeOrDynType bad_nc 0 (maxTypeSize_ m)

encodeBase64 :: B.ByteString -> LT.Text
encodeBase64 bs = LT.pack $ B.unpack $ encodeBase64' bs

texty :: Show a => a -> LT.Text
texty x = LT.pack $ show x

textyt :: Show a => a -> DLType -> LT.Text
textyt x ty = texty x <> " :: " <> texty ty

textyv :: DLVar -> LT.Text
textyv v = texty v --t v (varType v)

type ScratchSlot = Word8

type TealOp = LT.Text

type TealArg = LT.Text

type Label = LT.Text

data IndentDir
  = INo
  | IUp
  | IDo
  deriving (Show)

data TEAL
  = TCode TealOp [TealArg]
  | Titob Bool
  | TInt Integer
  | TConst LT.Text
  | TBytes B.ByteString
  | TExtract Word8 Word8
  | TReplace2 Word8
  | TSubstring Word8 Word8
  | TComment IndentDir LT.Text
  | TLabel Label
  | TFor_top Integer
  | TFor_bnz Label Integer Label
  | TLog Integer
  | TStore ScratchSlot LT.Text
  | TLoad ScratchSlot LT.Text
  | TResource Resource
  | TCostCredit Integer
  | TCheckOnCompletion
  deriving (Show)

type TEALt = [LT.Text]

type TEALs = DL.DList TEAL

builtin :: S.Set TealOp
builtin = S.fromList ["byte", "int", "substring", "extract", "log", "store", "load", "itob"]

base64d :: BS.ByteString -> LT.Text
base64d bs = "base64(" <> encodeBase64 bs <> ")"

render :: IORef Int -> TEAL -> IO TEALt
render ilvlr = \case
  TInt x -> r ["int", texty x]
  TConst x -> r ["int", x]
  TBytes bs -> r ["byte", base64d bs]
  TExtract x y -> r ["extract", texty x, texty y]
  TReplace2 x -> r ["replace2", texty x]
  TSubstring x y -> r ["substring", texty x, texty y]
  Titob _ -> r ["itob"]
  TCode f args ->
    case S.member f builtin of
      True -> impossible $ show $ "cannot use " <> f <> " directly"
      False -> r $ f : args
  TComment il t -> do
    case il of
      INo -> return ()
      IUp -> modifyIORef ilvlr $ \x -> x + 1
      IDo -> modifyIORef ilvlr $ \x -> x - 1
    case t of
      "" -> return []
      _ -> r ["//", t]
  TLabel lab -> r [lab <> ":"]
  TFor_top maxi ->
    r [("// for runs " <> texty maxi <> " times")]
  TFor_bnz top_lab maxi _ ->
    r ["bnz", top_lab, ("// for runs " <> texty maxi <> " times")]
  TLog sz ->
    r ["log", ("// up to " <> texty sz <> " bytes")]
  TStore sl lab -> r ["store", texty sl, ("// " <> lab)]
  TLoad sl lab -> r ["load", texty sl, ("// " <> lab)]
  TResource rs -> r [("// resource: " <> texty rs)]
  TCostCredit i -> r [("// cost credit: " <> texty i)]
  TCheckOnCompletion -> r [("// checked on completion")]
  where
    r l = do
      i <- readIORef ilvlr
      let i' = replicate i " "
      return $ i' <> l

renderOut :: [TEAL] -> IO T.Text
renderOut tscl' = do
  ilvlr <- newIORef $ 0
  tsl' <- mapM (render ilvlr) tscl'
  let lts = tealVersionPragma : (map LT.unwords tsl')
  let lt = LT.unlines lts
  let t = LT.toStrict lt
  return t

optimize :: TEALs -> IO [TEAL]
optimize ts00 = do
  ts05 <- opt_cfg ts00
  let ts10 = opt_peep ts05
  let ts20 = opt_bs ts10
  let tsN = ts20
  return tsN

-- Optimize the order of basic blocks to decrease the number of jumps, by
-- putting destinations immediately after the jump. This also means that blocks
-- that are only jumped to once are effectively inlined.
--
-- NOTE: Extend this to optimize away callsub-once
type Blocks = M.Map Label BasicBlock
type CallCounts = M.Map Label Int
data BasicBlock = BasicBlock
  { bb_ts :: TEALs
  , bb_mayInline :: Bool
  , bb_ec :: Maybe Label
  }

instance Show BasicBlock where
  show _ = "bb"

type CFGAcc = Maybe (Label, BasicBlock)

opt_cfg :: TEALs -> IO [TEAL]
opt_cfg ts0 = do
  bmr <- newIORef (mempty :: Blocks)
  ccr <- newIORef (mempty :: CallCounts)
  let bb0 lab ts = Just (lab, (BasicBlock ts True mempty))
  let save :: CFGAcc -> IO ()
      save = \case
        Nothing -> return ()
        Just (lab, bb) -> modifyIORef bmr $ M.insert lab bb
  let go :: CFGAcc -> TEAL -> IO CFGAcc
      go acc = \case
        t@(TLabel lab') ->
          case acc of
            Nothing -> do
              save acc
              return $ bb0 lab' (DL.singleton t)
            _ -> impossible $ "cfg: label without b " <> show lab'
        t ->
          case acc of
            Nothing -> impossible $ "cfg: non-label without block: " <> show t
            Just (lab, BasicBlock {..}) ->
              case t of
                TCode "b" [ lab' ] -> do
                  save $ Just (lab, BasicBlock bb_ts False (Just lab'))
                  return $ Nothing
                _ -> do
                  let ts' = DL.snoc bb_ts t
                  let bb' mi = BasicBlock ts' mi Nothing
                  let mk_acc' mi = Just $ (lab, bb' mi)
                  let acc' = mk_acc' bb_mayInline
                  let next = save acc' >> return Nothing
                  let this = return acc'
                  let cant_inline = return $ mk_acc' False
                  case t of
                    TCode "err" [] -> next
                    TCode "return" [] -> next
                    TCode "retsub" [] -> next
                    TCode "bnz" _ -> cant_inline
                    TCode "bz" _ -> cant_inline
                    TCode "callsub" [l] -> do
                      modifyIORef ccr $ M.alter (Just . (+) 1 . fromMaybe 0) l
                      this
                    _ -> this
  let lTOP = "TOP"
  save =<< foldlM go (bb0 lTOP mempty) ts0
  q <- do
    bm <- readIORef bmr
    cc <- readIORef ccr
    let ccs = reverse $ map fst $ sortOn snd $ M.toAscList cc
    return $ lTOP : (M.keys $ M.withoutKeys bm (M.keysSet cc)) <> ccs
  ansr <- newIORef mempty
  let add1 t = modifyIORef ansr $ flip DL.snoc t
  let tryInline :: Bool -> TEAL -> IO ()
      tryInline rmRetSub = \case
        TCode "retsub" [] | rmRetSub -> return ()
        t@(TCode "callsub" [l]) -> do
          ((,) <$> (M.lookup l <$> readIORef ccr) <*> (M.lookup l <$> readIORef bmr)) >>= \case
            (Just 1, Just (BasicBlock l_bs True Nothing)) -> do
              snarf True l l_bs
            _ -> add1 t
        t -> add1 t
      snarf :: Bool -> Label -> TEALs -> IO ()
      snarf rmRetSub lab ts = do
        modifyIORef bmr $ M.delete lab
        mapM_ (tryInline rmRetSub) ts
  let grab :: Label -> IO ()
      grab lab = do
        (M.lookup lab <$> readIORef bmr) >>= \case
          Nothing -> add1 $ TCode "b" [lab]
          Just (BasicBlock {..}) -> do
            snarf False lab bb_ts
            grabm bb_ec
      grabm :: Maybe Label -> IO ()
      grabm = \case
        Just lab -> grab lab
        Nothing -> return ()
      grabq :: [Label] -> IO ()
      grabq = \case
        [] -> return ()
        l : q' -> do
          (M.member l <$> readIORef bmr) >>= \case
            True -> grab l >> grabq q'
            False -> grabq q'
  grabq q
  DL.toList <$> readIORef ansr

-- Optimize the definition of BYTES: We don't do this in the peep-hole optimize
-- because we want them to be removed if possible
opt_bs :: [TEAL] -> [TEAL]
opt_bs = \case
  [] -> []
  x@(TBytes bs) : l | B.all (== '\0') bs ->
    case B.length bs of
      len | len < 3 -> x : opt_bs l
      32 -> opt_bs $ (TCode "global" ["ZeroAddress"]) : l
      -- Space is more important than cost?
      len ->
        let spaceMoreThanCost = True in
        case spaceMoreThanCost of
          True -> opt_bs $ (TInt $ fromIntegral len) : (TCode "bzero" []) : l
          False -> x : opt_bs l
  x : l -> x : opt_bs l

-- Peep-hole optimizer
opt_peep :: [TEAL] -> [TEAL]
opt_peep = foldr (\a b -> opt_peep1 $ a : b) mempty

data TType
  = TT_UInt
  | TT_Bytes
  | TT_Unknown

opType :: TEAL -> Maybe ([TType], [TType])
opType = \case
  Titob {} -> u2b
  TInt {} -> _2u
  TConst {} -> _2u
  TBytes {} -> _2b
  TExtract {} -> b2b
  TReplace2 {} -> b2b
  TSubstring {} -> b2b
  TComment {} -> Nothing
  TLabel {} -> Nothing
  TFor_top {} -> Nothing
  TFor_bnz {} -> Nothing
  TLog {} -> eff
  TStore {} -> eff
  TLoad {} -> j [] [k]
  TResource {} -> eff
  TCostCredit {} -> eff
  TCheckOnCompletion -> Nothing
  TCode o _ ->
    -- XXX Fill in this table
    case o of
      "err" -> eff
      "sha256" -> b2b
      "keccak256" -> b2b
      -- ....
      "global" -> j [] [k]
      -- ....
      _ -> Nothing
  where
    _2b = j [] [b]
    _2u = j [] [u]
    u2b = j [u] [b]
    b2b = j [b] [b]
    eff = Nothing
    k = TT_Unknown
    u = TT_UInt
    b = TT_Bytes
    j x y = Just (x, y)

opArity :: TEAL -> Maybe (Int, Int)
opArity = fmap f . opType
  where
    f (x, y) = (length x, length y)

opt_peep1 :: [TEAL] -> [TEAL]
opt_peep1 = \case
  [] -> []
  [(TCode "return" [])] -> []
  -- This relies on knowing what "done" is
  (TCode "assert" []) : (TCode "b" ["done"]) : x -> (TCode "return" []) : x
  x@(TBytes _) : y@(TBytes _) : (TCode "swap" []) : l ->
    opt_peep1 $ y : x : l
  (TBytes "") : (TCode "concat" []) : l -> l
  (TBytes "") : b@(TLoad {}) : (TCode "concat" []) : l -> opt_peep1 $ b : l
  (TBytes x) : (TBytes y) : (TCode "concat" []) : l ->
    opt_peep1 $ (TBytes $ x <> y) : l
  -- XXX generalize this optimization and make it so we don't do it to things
  -- with effects
  -- If x doesn't consume anything and we pop it, just pop it
  x : (TCode "pop" []) : l | opArity x == Just (0, 1) -> l
  -- If x consumes something and we pop it, then pop the argument too
  x : y@(TCode "pop" []) : l | opArity x == Just (1, 1) -> y : l
  -- If x consumes 2 things and we pop it, then pop the arguments too
  x : y@(TCode "pop" []) : l | opArity x == Just (2, 1) -> y : y : l
  (TCode "b" [x]) : b@(TLabel y) : l | x == y -> b : l
  (TCode "btoi" []) : (Titob True) : (TSubstring 7 8) : l -> l
  (TCode "btoi" []) : (Titob _) : l -> l
  (TInt 0) : (TCode "-" []) : l -> l
  (TInt 0) : (TCode "+" []) : l -> l
  (Titob _) : (TCode "btoi" []) : l -> l
  (TCode "==" []) : (TCode "!" []) : l -> (TCode "!=" []) : l
  (TCode "b==" []) : (TCode "!" []) : l -> (TCode "b!=" []) : l
  (TInt 0) : (TCode "!=" []) : (TCode "assert" []) : l ->
    (TCode "assert" []) : l
  (TCode "*" []) : (TInt x) : (TCode "/" []) : (TInt y) : l | x == y ->
    l
  (TExtract x 8) : (TCode "btoi" []) : l ->
    (TInt $ fromIntegral x) : (TCode "extract_uint64" []) : l
  x@(TInt _) : (TInt 8) : (TCode "extract3" []) : (TCode "btoi" []) : l ->
    x : (TCode "extract_uint64" []) : l
  a@(TLoad x _) : (TLoad y _) : l
    | x == y ->
      -- This misses if there is ANOTHER load of the same thing
      a : (TCode "dup" []) : l
  a@(TStore x _) : (TLoad y _) : l
    | x == y ->
      (TCode "dup" []) : a : l
  a@(TSubstring s0w _) : b@(TInt xn) : c@(TCode "getbyte" []) : l ->
    case xn < 256 && s0xnp1 < 256 of
      True -> opt_peep1 $ (TSubstring (fromIntegral s0xn) (fromIntegral s0xnp1)) : (TCode "btoi" []) : l
      False -> a : b : c : l
    where
      s0xn :: Integer
      s0xn = (fromIntegral s0w) + xn
      s0xnp1 :: Integer
      s0xnp1 = s0xn + 1
  a@(TSubstring s0w _) : b@(TSubstring s1w e1w) : l ->
    case s2n < 256 && e2n < 256 of
      True -> opt_peep1 $ (TSubstring (fromIntegral s2n) (fromIntegral e2n)) : l
      False -> a : b : l
    where
      s0n = fromIntegral s0w
      s2n :: Integer
      s2n = s0n + (fromIntegral s1w)
      e2n :: Integer
      e2n = s0n + (fromIntegral e1w)
  (TLoad x _xlab) : (TStore y _ylab) : l | x == y -> opt_peep1 $ l
  (TInt 0) : (TCode "==" []) : (TCode "bz" [lab]) : l ->
    -- We are jumping TO lab if what is on the stack is NOT 0
    --
    -- int 1
    -- int 0
    -- ==     => bool false  =>
    -- bz lab    bz lab         b lab
    --
    -- int 0
    -- int 0
    -- ==     => bool true  =>
    -- bz lab    bz lab         nop
    --
    opt_peep1 $ (TCode "bnz" [lab]) : l
  (TInt x) : (Titob _) : l ->
    opt_peep1 $ (TBytes $ itob 8 x) : l
  (TBytes xbs) : (TCode "btoi" []) : l ->
    opt_peep1 $ (TInt $ btoi xbs) : l
  (TBytes xbs) : (TCode "sha256" []) : l ->
    opt_peep1 $ (TBytes $ sha256bs xbs) : l
  (TBytes xbs) : (TCode "sha512_256" []) : l ->
    opt_peep1 $ (TBytes $ sha512_256bs xbs) : l
  (TBytes xbs) : (TSubstring s e) : l ->
    opt_peep1 $ (TBytes $ bsSubstring xbs (fromIntegral s) (fromIntegral e)) : l
  (TBytes xbs) : (TExtract s len) : l | len /= 0 ->
    opt_peep1 $ (TBytes $ bsSubstring xbs (fromIntegral s) (fromIntegral $ s + len)) : l
  x : l -> x : l

sha256bs :: BS.ByteString -> BS.ByteString
sha256bs = BA.convert . hashWith SHA256
sha512_256bs :: BS.ByteString -> BS.ByteString
sha512_256bs = BA.convert . hashWith SHA512t_256

bsSubstring :: BS.ByteString -> Int -> Int -> BS.ByteString
bsSubstring bs s e = BS.take e $ BS.drop s bs

padTo :: Int -> a -> [a] -> [a]
padTo p d l = replicate (p - length l) d <> l

itob :: Int -> Integer -> BS.ByteString
itob howMany = BS.pack . padTo howMany 0 . reverse . unroll

btoi :: BS.ByteString -> Integer
btoi = roll . reverse . BS.unpack

unroll :: Integer -> [Word8]
unroll = List.unfoldr go
  where
    go 0 = Nothing
    go i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

type RestrictCFG = Label -> IO (DotGraph, AnalyzeCFG, BudgetCFG)
type BudgetCFG = IO (Bool, Integer, Integer)
type AnalyzeCFG = Resource -> IO Integer
type ResourceCost = M.Map Resource Integer

buildCFG :: String -> [TEAL] -> IO (DotGraph, RestrictCFG)
buildCFG rlab ts = do
  res_gr :: IORef (LPGraph String ResourceCost) <- newIORef mempty
  let lTop = "TOP"
  let lBot = "BOT"
  (labr :: IORef String) <- newIORef $ lTop
  (k_r :: IORef Integer) <- newIORef $ 1
  (res_r :: IORef ResourceCost) <- newIORef $ mempty
  (calls_r :: IORef [String]) <- newIORef $ mempty
  let modK = modifyIORef k_r
  let l2s = LT.unpack
  let recResource rs c = do
        k <- readIORef k_r
        let f old = Just $ (k * c) + fromMaybe 0 old
        modifyIORef res_r $ M.alter f rs
  let recCost = recResource R_Cost
  let incBudget = recResource R_Budget algoMaxAppProgramCost
  let jump_ :: String -> IO ()
      jump_ t = do
        lab <- readIORef labr
        calls <- readIORef calls_r
        c <- readIORef res_r
        let ff = S.insert (calls, c)
        let fg = Just . ff . fromMaybe mempty
        let f = M.alter fg t
        let g = Just . f . fromMaybe mempty
        modifyIORef res_gr $ M.alter g lab
  let switch t = do
        writeIORef labr t
        writeIORef res_r mempty
        writeIORef calls_r mempty
  let call t = modifyIORef calls_r $ (:) (l2s t)
  let jump t = recCost 1 >> jump_ (l2s t)
  let fswitch ls = recCost 1 >> (mapM_ jump_ $ map l2s ls)
  incBudget -- initial budget
  forM_ ts $ \case
    TFor_top cnt -> do
      modK (\x -> x * cnt)
    TFor_bnz _ cnt lab' -> do
      recCost 1
      modK (\x -> x `div` cnt)
      jump lab'
    TCode "match" labs -> fswitch labs
    TCode "switch" labs -> fswitch labs
    TCode "bnz" [lab'] -> jump lab'
    TCode "bz" [lab'] -> jump lab'
    TCode "b" [lab'] -> do
      jump lab'
      switch ""
    TCode "return" [] -> do
      jump lBot
      switch ""
    TCode "callsub" [lab'] -> do
      call lab'
      recCost 1
    TCode "retsub" [] -> do
      recCost 1
      jump lBot
      switch ""
    TCode "err" [] -> do
      -- recCost 1 -- Doesn't cost anything, because we know the txn fails
      recResource R_CheckedCompletion 1
      jump lBot
      switch ""
    TLog len -> do
      recResource R_Log len
      recResource R_LogCalls 1
      recCost 1
    TComment {} -> return ()
    TLabel lab' -> do
      let lab'' = l2s lab'
      jump_ lab''
      switch lab''
    TBytes _ -> recCost 1
    TConst _ -> recCost 1
    TStore {} -> recCost 1
    TLoad {} -> recCost 1
    TInt _ -> recCost 1
    TExtract {} -> recCost 1
    TReplace2 {} -> recCost 1
    TSubstring {} -> recCost 1
    Titob {} -> recCost 1
    TResource r -> recResource r 1
    TCostCredit i -> do
      incBudget
      recCost i
    TCheckOnCompletion -> do
      recResource R_CheckedCompletion 1
      recCost 0
    TCode f _ ->
      case f of
        "sha256" -> recCost 35
        "sha3_256" -> recCost 130
        "keccak256" -> recCost 130
        "sha512_256" -> recCost 45
        "ed25519verify" -> recCost 1900
        "ed25519verify_bare" -> recCost 1900
        "ecdsa_verify" -> recCost 1700
        "ecdsa_pk_decompress" -> recCost 650
        "ecdsa_pk_recover" -> recCost 2000
        "divmodw" -> recCost 20
        "sqrt" -> recCost 4
        "expw" -> recCost 10
        "b+" -> recCost 10
        "b-" -> recCost 10
        "b/" -> recCost 20
        "b*" -> recCost 20
        "b%" -> recCost 20
        "b|" -> recCost 6
        "b&" -> recCost 6
        "b^" -> recCost 6
        "b~" -> recCost 4
        "bsqrt" -> recCost 40
        "bn256_add" -> recCost 70
        "bn256_scalar_mul" -> recCost 970
        "bn256_pairing" -> recCost 8700
        "itxn_begin" -> do
          recResource R_ITxn 1
          recCost 1
        "itxn_next" -> do
          recResource R_ITxn 1
          recCost 1
        _ -> recCost 1
  let renderRc m = intercalate "/" $ map f allResources
        where f r = show $ fromMaybe 0 $ M.lookup r m
  let renderCalls = \case
        [] -> ""
        cls -> show cls <> "/"
  let gs :: LPGraph String ResourceCost -> DotGraph
      gs g =
        flip concatMap (M.toAscList g) $ \(from, cs) ->
          case (from == mempty) of
            True -> []
            False ->
              flip concatMap (M.toAscList cs) $ \(to, es) ->
                flip concatMap (S.toAscList es) $ \(cls, c) ->
                  [(from, to, (M.fromList $ [("label", renderCalls cls <> renderRc c)]))]
  g <- readIORef res_gr
  let getc rs c = fromMaybe 0 $ M.lookup rs c
  let lBots = l2s lBot
  let restrict mustLab = do
        -- putStrLn $ "restrict and analyze " <> show mustLab
        g' <- restrictGraph g $ l2s mustLab
        let analyzeCFG = longestPathBetween g' lTop lBots . getc
        let budgetCFG = budgetAnalyze g' lTop lBots (flip getc)
        return $ (gs g', analyzeCFG, budgetCFG)
  loud $ rlab <> " OnCompletion"
  do
    ensureAllPaths (rlab <> ".OnC") g lTop lBots (getc R_CheckedCompletion) >>= \case
      Nothing -> return ()
      Just p ->
        impossible $ "found a path where OnCompletion was not checked: " <> show (reverse p)
  return (gs g, restrict)

data LabelRec = LabelRec
  { lr_lab :: Label
  , lr_at :: SrcLoc
  , lr_what :: String
  }
  deriving (Show)

type CompanionCalls = M.Map Label Integer
type CompanionInfo = Maybe CompanionCalls
data CompanionAdds
  = CA_AddCompanion
  | CA_IncrementCalls Label
  deriving (Eq, Ord)
data CompanionRec = CompanionRec
  { cr_ro :: DLArg
  , cr_approval :: B.ByteString
  , cr_clearstate :: B.ByteString
  , cr_ctor :: Integer
  , cr_call :: Integer
  , cr_del :: Integer
  }

showTeal :: T.Text -> Outputer -> [TEAL] -> IO ()
showTeal lab disp ts = do
  ts' <- renderOut ts
  mayOutput (disp False $ ".teal" <> lab) $
    flip TIO.writeFile ts'

checkCost :: String -> Notify -> Outputer -> [LabelRec] -> CompanionInfo -> [TEAL] -> IO (Either String CompanionInfo)
checkCost rlab notify disp ls ci ts = do
  msgR <- newIORef mempty
  let addMsg x = modifyIORef msgR $ flip (<>) $ x <> "\n"
  caR <- newIORef (mempty :: S.Set CompanionAdds)
  let rgs lab gs = do
        mayOutput (disp False ("." <> lab <> "dot")) $
          flip LTIO.writeFile (T.render $ dotty gs)
  loud $ rlab <> " buildCFG"
  (gs, restrictCFG) <- buildCFG rlab ts
  rgs "" gs
  addMsg $ "Conservative analysis on Algorand found:"
  forM_ ls $ \LabelRec {..} -> do
    let starts_at = " starts at " <> show lr_at <> "."
    addMsg $ " * " <> lr_what <> ", which" <> starts_at
    loud $ rlab <> " restrictCFG " <> show lr_lab
    (gs', analyzeCFG, budgetCFG) <- restrictCFG lr_lab
    when True $
      rgs (T.pack $ LT.unpack lr_lab <> ".") gs'
    let doReport precise tooMuch msg_ = do
          let msg = msg_ <> "."
          when tooMuch $ do
            notify precise $ LT.pack $ lr_what <> " " <> msg <> " " <> lr_what <> starts_at
          addMsg $ "   + " <> msg
    let reportCost precise ler algoMax c = do
          let units = ler $ c /= 1
          let uses = if precise then "uses" else "may use up to"
          let pre = uses <> " " <> show c <> " " <> units
          let tooMuch = c > algoMax
          let post = if tooMuch then ", but the limit is " <> show algoMax else ""
          unless (c == 0) $
            doReport precise tooMuch $ pre <> post
    let allResourcesM' = M.withoutKeys allResourcesM $ S.fromList [ R_Cost, R_Budget ]
    costM <- flip mapWithKeyM allResourcesM' $ \rs _ -> do
      let am = maxOf rs
      c <- analyzeCFG rs
      let pe = rPrecise rs
      let ler = flip rLabel rs
      reportCost pe ler am c
      return c
    let sums = foldr (+) 0 . M.elems . M.restrictKeys costM . S.fromList
    let refs = sums [R_App, R_Asset, R_Account, R_Box]
    void $ reportCost False (flip plural "transaction reference") algoMaxAppTotalTxnReferences refs
    do
      (over, cost, budget) <- budgetCFG
      let residue = budget - cost
      let doReport' = doReport True
      let uses = "uses " <> show cost
      let budget' x = "its budget " <> x <> " " <> show budget
      case over of
        False -> doReport' False $
          uses <> " of " <> budget' "of" <> " (" <> show residue <> " is left over)"
        True -> do
          modifyIORef caR $ S.insert $
            case ci of
              Nothing -> CA_AddCompanion
              Just _ -> CA_IncrementCalls lr_lab
          doReport' True $
            uses <> ", but " <> budget' "is"
    let fees = sums [R_Txn, R_ITxn]
    addMsg $ "   + costs " <> show fees <> " " <> plural (fees /= 1) "fee" <> "."
  msg <- readIORef msgR
  cas <- S.toAscList <$> readIORef caR
  --let cr = Left msg
  cr <- case cas of
    [] ->
      return $ Left msg
    [ CA_AddCompanion ] ->
      return $ Right $ Just mempty
    as ->
      case ci of
        Nothing -> impossible "inc nothing"
        Just cim -> do
          let f = \case
                CA_AddCompanion -> impossible "add just"
                CA_IncrementCalls lab -> M.insertWith (+) lab 1
          let cim' = foldr f cim as
          return $ Right $ Just cim'
  return $ cr

type Lets = M.Map DLVar (App ())

data Env = Env
  { eApiLs :: IORef [LabelRec]
  , eProgLs :: IORef (Maybe [LabelRec])
  , eMaxApiRetSize :: IORef Integer
  , eFailuresR :: ErrorSetRef
  , eWarningsR :: ErrorSetRef
  , eCounter :: Counter
  , eStateSizeR :: IORef Integer
  , eWhich :: Maybe Int
  , eLabel :: Counter
  , eOutputR :: IORef TEALs
  , eColoring :: M.Map DLVar ScratchSlot
  , eVars :: M.Map DLVar (App ())
  , eLets :: Lets
  , eResources :: ResourceSets
  , eNewToks :: IORef (S.Set DLArg)
  , eInitToks :: IORef (S.Set DLArg)
  , eCompanion :: CompanionInfo
  , eCompanionRec :: CompanionRec
  , eLibrary :: IORef (M.Map LibFun (Label, IORef Bool, App ()))
  , eGetStateKeys :: IO Int
  , eABI :: IORef (M.Map String ABInfo)
  , eRes :: IORef (M.Map T.Text AS.Value)
  , eFunVars :: FunVars
  , eStateMap :: CLState
  , eDoesMapSetR :: IORef Bool
  , eDoesContractNewR :: IORef Bool
  }

instance HasStateMap Env where
  getStateMap = eStateMap

instance HasFunVars Env where
  getFunVars = eFunVars

data ABInfo = ABInfo
  { abiPure :: Bool
  }

instance HasCounter Env where
  getCounter = eCounter

insertResult :: T.Text -> AS.Value -> App ()
insertResult k v = do
  r <- asks eRes
  liftIO $ modifyIORef r $ M.insert k v

type App = ReaderT Env IO

class CompileK a where
  cpk :: App b -> a -> App b

class Compile a where
  cp :: a -> App ()

class CompileLabel a where
  cpl :: a -> App (Label, App ())

data LibFun
  = LF_mapRef
  | LF_mapDel
  | LF_mapSet
  | LF_checkTxn Bool
  | LF_makeTxn Bool Bool Bool Bool
  | LF_makeTxnK Bool Bool
  | LF_checkUInt256ResultLen
  | LF_mbrAdd
  | LF_mbrSub
  | LF_wasntMeth
  | LF_fromSome
  | LF_svsLoad Int
  | LF_svsDump Int
  | LF_companionCall
  | LF_timeCheck
  | LF_updateMbr
  | LF_blockRead ALGOBlockField
  | LF_blockCheck
  deriving (Eq, Ord, Show)

libDefns :: App ()
libDefns = do
  libr <- asks eLibrary
  lib <- liftIO $ readIORef libr
  wroteSomeR <- liftIO $ newIORef False
  forM_ lib $ \(_, wroteR, impl) -> do
    wrote <- liftIO $ readIORef wroteR
    unless wrote $ do
      impl
      liftIO $ writeIORef wroteR True
      liftIO $ writeIORef wroteSomeR True
  wroteSome <- liftIO $ readIORef wroteSomeR
  when wroteSome $ libDefns

libImpl :: LibFun -> App () -> App Label
libImpl lf impl = do
  libr <- asks eLibrary
  lib <- liftIO $ readIORef libr
  case M.lookup lf lib of
    Nothing -> do
      wroteR <- liftIO $ newIORef False
      lab <- freshLabel $ show lf
      let impl' = label lab >> impl
      liftIO $ modifyIORef libr $ M.insert lf (lab, wroteR, impl')
      return $ lab
    Just (lab, _, _) -> return lab

libCall :: LibFun -> App () -> App ()
libCall lf impl = do
  lab <- libImpl lf impl
  code "callsub" [ lab ]

libJump :: LibFun -> App () -> App ()
libJump lf impl = do
  lab <- libImpl lf impl
  code "b" [ lab ]

cUpdateMbr :: App ()
cUpdateMbr = libCall LF_updateMbr $ do
  gvLoad GV_mbrAdd
  gvLoad GV_mbrSub
  op "dup2"
  op ">="
  code "bz" [ "updateMbr_neg" ]
  code "b" ["updateMbr_pos_eq"]
  label "updateMbr_pos_eq"
  op "-"
  op "dup"
  code "bz" [ "updateMbr_eq" ]
  do
    gvStore GV_mbrAdd
    let ct_at = sb
    let ct_mtok = Nothing
    let ct_amt = aDMbr
    store_let vDMbr (gvLoad GV_mbrAdd) $
      void $ checkTxn $ CheckTxn {..}
  op "retsub"
  label "updateMbr_eq"
  op "pop"
  op "retsub"
  label "updateMbr_neg"
  op "swap"
  op "-"
  do
    gvStore GV_mbrAdd
    incResource R_Account aDeployer
    let mt_amt = aDMbr
    let mt_at = sb
    let mt_mtok = Nothing
    let mt_always = True
    let mt_mrecv = Just $ Right $ cDeployer
    let mt_mcclose = Nothing
    let mt_next = False
    let mt_submit = True
    store_let vDMbr (gvLoad GV_mbrAdd) $
      void $ makeTxn $ MakeTxn {..}
  op "retsub"

cGetTag :: App ()
cGetTag = do
  cint 0
  op "getbyte"

-- Cost Analysis...
--
-- Inlined:
--   int 1
--   store 4
--   ... n times
--
--   Cost : 2n
--   Space: 4n
--
-- Unlined:
--   callsub LF_wasntMeth
--   ... n times
--   wasntMeth:
--   int 1
--   store 4
--   retsub
--
--   Cost : 4n
--   Space: 2n + 5
--
-- Unlined is always worse for cost
-- But, it is better for space when n > 2
--
--   n= 1 ---  4 vs 7
--   n= 2 ---  8 vs 9
--   n= 3 --- 12 vs 11
--   n=10 --- 40 vs 25
--
-- Also, this is "wasntMeth" rather than "wasMeth", because we expect that the
-- number of publishes is less than the number of API calls (because each
-- publish has many publishes in it)
cWasntMeth :: App ()
cWasntMeth = libCall LF_wasntMeth $ do
  cp True
  gvStore GV_wasntMeth
  op "retsub"

mbrAdd :: App ()
mbrAdd = libCall LF_mbrAdd $ do
  gvLoad GV_mbrAdd
  op "+"
  gvStore GV_mbrAdd
  op "retsub"

mbrSub :: App ()
mbrSub = libCall LF_mbrSub $ do
  gvLoad GV_mbrSub
  op "+"
  gvStore GV_mbrSub
  op "retsub"

-- XXX maybe store w/ tag to simplify this
cMapRef :: App ()
cMapRef = libCall LF_mapRef $ do
  -- [ tagSpot, nothing, key ]
  op "box_get"
  -- [ tagSpot, nothing, raw_data, exists ]
  op "dup"
  -- [ tagSpot, nothing, raw_data, exists, exists ]
  ctobs $ T_Bool
  -- [ tagSpot, nothing, raw_data, exists, exists_as_bytes ]
  code "bury" [ "4" ]
  -- [ tag, nothing, raw_data, exists ]
  op "select"
  -- [ tag, data ]
  op "concat"
  -- [ tagged-data ]
  op "retsub"

cMapDel :: App ()
cMapDel = libCall LF_mapDel $ do
  -- [ mbr, key ]
  op "box_del"
  -- [ mbr, existed ]
  cint 0
  -- [ mbr, existed, zero ]
  code "cover" ["2"]
  -- [ zero, mbr, existed ]
  op "select"
  -- [ mbr if existed (ow zero) ]
  mbrSub
  op "retsub"

cMapSet :: App ()
cMapSet = libCall LF_mapSet $ do
  -- [ mbr, key, data ]
  op "swap"
  -- [ mbr, data, key ]
  op "dup"
  -- [ mbr, data, key, key ]
  op "box_len"
  -- [ mbr, data, key, len, exists ]
  after <- freshLabel "boxSet"
  code "bnz" [ after ]
  -- [ mbr, data, key, len ]
  code "dig" [ "3" ]
  -- [ mbr, data, key, len, mbr ]
  mbrAdd
  code "b" [after]
  -- [ mbr, data, key, len ]
  label after
  -- [ mbr, data, key, len ]
  op "pop"
  op "swap"
  -- [ mbr, key, data ]
  op "box_put"
  -- [ mbr ]
  op "pop"
  -- []
  op "retsub"

checkMapSize :: DLType -> App Integer
checkMapSize t = do
  s <- typeSizeOf t
  unless (s <= reachMaxBoxSize) $ do
    let msg = "This program uses a map where the data is " <> show s <> " bytes long."
    case (s <= algoMaxBoxSize) of
      True ->
        bad $ LT.pack $ msg <> " But, the maximum size Reach supports is " <> show reachMaxBoxSize <> ", so this program cannot be compiled. Try to break up the map into smaller pieces. However, Algorand can support boxes up to " <> show algoMaxBoxSize <> "; contact us and we'll expand what Reach supports---we thought 4k would be enough for anyone!"
      False ->
        bad $ LT.pack $ msg <> " But, the maximum size is " <> show algoMaxBoxSize <> ", so this program cannot be compiled. Try to break up the map into smaller pieces."
  return s

separateResources :: App a -> App a
separateResources = dupeResources . resetToks

recordWhich :: Maybe Int -> App a -> App a
recordWhich mn = local (\e -> e {eWhich = mn}) . separateResources

data Resource
  = R_Asset
  | R_App
  | R_Account
  | R_Box
  | R_Log
  | R_LogCalls
  | R_Budget
  | R_Cost
  | R_ITxn
  | R_Txn
  | R_CheckedCompletion
  deriving (Eq, Ord, Enum, Bounded, Show)

allResources :: [Resource]
allResources = enumerate List.\\ [ R_CheckedCompletion ]

allResourcesM :: M.Map Resource ()
allResourcesM = M.fromList $ map (flip (,) ()) allResources

useResource :: Resource -> App ()
useResource = output . TResource

type ResourceSet = S.Set ResourceArg

type ResourceArg = (Int, DLArg)

type ResourceSets = IORef (M.Map Resource ResourceSet)

plural :: Bool -> String -> String
plural ph x = x <> if ph then "s" else ""

rLabel :: Bool -> Resource -> String
rLabel ph = \case
  R_Txn -> "input " <> p "transaction"
  R_ITxn -> "inner " <> p "transaction"
  R_Asset -> p "asset"
  R_Box -> "box" <> if ph then "es" else ""
  R_App -> "foreign " <> p "application"
  R_Account -> p "account"
  R_Budget -> p "unit" <> " of budget"
  R_Cost -> p "unit" <> " of cost"
  R_Log -> p "byte" <> " of logs"
  R_LogCalls -> "log " <> p "call"
  R_CheckedCompletion -> p "completion" <> " checked"
  where
    p = plural ph

rPrecise :: Resource -> Bool
rPrecise = \case
  R_Txn -> True
  R_ITxn -> True
  R_App -> False
  R_Box -> False
  R_Asset -> False
  R_Account -> False
  R_Budget -> True
  R_Cost -> True
  R_Log -> True
  R_LogCalls -> True
  R_CheckedCompletion -> True

maxOf :: Resource -> Integer
maxOf = \case
  R_App -> algoMaxAppTxnForeignApps
  R_Asset -> algoMaxAppTxnForeignAssets
  R_Account -> algoMaxAppTxnAccounts
  R_Box -> algoMaxAppBoxReferences
  R_Txn -> algoMaxTxGroupSize
  R_ITxn -> algoMaxInnerTransactions * algoMaxTxGroupSize
  R_Budget -> impossible "budget"
  R_Cost -> impossible "cost"
  R_Log -> algoMaxLogLen
  R_LogCalls -> algoMaxLogCalls
  R_CheckedCompletion -> 1

newResources :: IO ResourceSets
newResources = newIORef $ mempty

dupeResources :: App a -> App a
dupeResources m = do
  c' <- (liftIO . dupeIORef) =<< asks eResources
  local (\e -> e {eResources = c'}) m

readResource :: Resource -> App ResourceSet
readResource r = do
  rsr <- asks eResources
  m <- liftIO $ readIORef rsr
  return $ fromMaybe mempty $ M.lookup r m

freeResource :: Resource -> ResourceArg -> App ()
freeResource r a = do
  vs <- readResource r
  let vs' = S.insert a vs
  rsr <- asks eResources
  liftIO $ modifyIORef rsr $ M.insert r vs'

incResource :: Resource -> DLArg -> App ()
incResource r a = incResource_ r (0, a)

incResource_ :: Resource -> ResourceArg -> App ()
incResource_ r a = do
  vs <- readResource r
  case S.member a vs of
    True -> return ()
    False -> do
      useResource r
      freeResource r a

resetToks :: App a -> App a
resetToks m = do
  ntoks <- liftIO $ newIORef mempty
  itoks <- liftIO $ newIORef mempty
  local (\e -> e {eNewToks = ntoks, eInitToks = itoks}) m

addTok :: (Env -> IORef (S.Set DLArg)) -> DLArg -> App ()
addTok ef tok = do
  r <- asks ef
  liftIO $ modifyIORef r (S.insert tok)

addNewTok :: DLArg -> App ()
addNewTok = addTok eNewToks

addInitTok :: DLArg -> App ()
addInitTok = addTok eInitToks

isTok :: (Env -> IORef (S.Set DLArg)) -> DLArg -> App Bool
isTok ef tok = do
  ts <- (liftIO . readIORef) =<< asks ef
  return $ tok `S.member` ts

isNewTok :: DLArg -> App Bool
isNewTok = isTok eNewToks

isInitTok :: DLArg -> App Bool
isInitTok = isTok eInitToks

output :: TEAL -> App ()
output t = do
  Env {..} <- ask
  liftIO $ modifyIORef eOutputR (flip DL.snoc t)

code :: LT.Text -> [LT.Text] -> App ()
code f args = output $ TCode f args

label :: LT.Text -> App ()
label = output . TLabel

comment :: LT.Text -> App ()
comment = output . TComment INo

block :: Label -> App a -> App a
block lab m = label lab >> m

dupn :: Int -> App ()
dupn = \case
  0 -> nop
  1 -> op "dup"
  k -> code "dupn" [ texty k ]

assert :: App ()
assert = op "assert"

asserteq :: App ()
asserteq = op "==" >> assert

op :: TealOp -> App ()
op = flip code []

nop :: App ()
nop = return ()

dont_concat_first :: [App ()]
dont_concat_first = nop : repeat (op "concat")

padding :: Integer -> App ()
padding = cp . bytesZeroLit

badlike :: (Env -> ErrorSetRef) -> LT.Text -> App ()
badlike eGet lab = do
  r <- asks eGet
  liftIO $ bad_io r lab

bad_nc :: LT.Text -> App ()
bad_nc = badlike eFailuresR

bad :: LT.Text -> App ()
bad lab = do
  bad_nc lab
  mapM_ comment $ LT.lines $ "BAD " <> lab

warn :: LT.Text -> App ()
warn = badlike eWarningsR

freshLabel :: String -> App LT.Text
freshLabel d = do
  i <- (liftIO . incCounter) =<< (eLabel <$> ask)
  let tr c = if isAlphaNum c then c else '_'
  return $ "l" <> LT.pack (show i) <> "_" <> LT.pack (map tr d)

store_let :: DLVar -> App () -> App a -> App a
store_let dv cgen m = do
  Env {..} <- ask
  local
    (\e ->
       e
         { eLets = M.insert dv cgen eLets
         })
    $ m

fakeVar :: DLVar -> App ()
fakeVar v = do
  cbs ""
  cfrombs $ typeOf v

lookup_let :: DLVar -> App ()
lookup_let dv = do
  Env {..} <- ask
  case M.lookup dv eLets of
    Just m -> m
    Nothing -> do
      bad $ LT.pack $ show eWhich <> " lookup_let " <> show (pretty dv) <> " not in " <> (List.intercalate ", " $ map (show . pretty) $ M.keys eLets)
      fakeVar dv

store_var :: DLVar -> App () -> App a -> App a
store_var dv ss m = do
  Env {..} <- ask
  local (\e -> e {eVars = M.insert dv ss eVars}) $
    m

lookup_var :: DLVar -> App (App ())
lookup_var dv = do
  Env {..} <- ask
  case M.lookup dv eVars of
    Just x -> return $ x
    Nothing -> impossible $ "lookup_var " <> show dv

lookupVarColoring :: String -> DLVar -> App (Maybe ScratchSlot)
lookupVarColoring lab dv = maybeLoc dv >>= \case
  Nothing -> do
    bad $ LT.pack $ "no coloring for " <> show dv <> " for " <> lab
    return Nothing
  Just l -> return $ Just l

class MaybeLoc a where
  maybeLoc :: a -> App (Maybe ScratchSlot)

instance MaybeLoc DLVar where
  maybeLoc y = M.lookup y <$> asks eColoring

instance MaybeLoc DLArg where
  maybeLoc = \case
    DLA_Var v -> maybeLoc v
    _ -> return $ Nothing

data CMove
  = CM_Bad
  | CM_Nop
  | CM_Move (App ()) (App ())

cmove_ :: (Show a, MaybeLoc a, Compile a) => DLVar -> a -> App CMove
cmove_ dst src = do
  lookupVarColoring ("cmove from " <> show src) dst >>= \case
    Nothing -> do
      bad $ LT.pack $ "could not move " <> show src <> " into " <> show dst
      return $ CM_Bad
    Just dstl -> do
      msrcl <- maybeLoc src
      case msrcl of
        Just srcl | dstl == srcl -> return $ CM_Nop
        _ -> return $ CM_Move (cp src) $ output $ TStore dstl $ texty dst

cmove :: (Show a, MaybeLoc a, Compile a) => DLVar -> a -> App ()
cmove dst src = cmove_ dst src >>= \case
  CM_Bad -> return ()
  CM_Nop -> return ()
  CM_Move csrc cstore -> do
    csrc
    cstore

cmoveMany :: (Show a, MaybeLoc a, Compile a) => [(DLVar, a)] -> App ()
cmoveMany dss = do
  dss' <- mapM (uncurry cmove_) dss
  forM_ dss' $ \case
    CM_Move csrc _ -> csrc
    _ -> return ()
  forM_ (reverse dss') $ \case
    CM_Move _ cstore -> cstore
    _ -> return ()

sallocVar :: String -> DLVar -> (App () -> App () -> App a) -> App a
sallocVar vlab dv fm = do
  let lab = textyv dv
  let fakeStore = op "pop"
  let fakeLoad = fakeVar dv
  let fake = fm fakeStore fakeLoad
  mloc <- lookupVarColoring ("sallocVar: " <> vlab) dv
  case mloc of
    Nothing -> do
      fake
    Just loc -> do
      let store = output $ TStore loc lab
      let load = output $ TLoad loc lab
      fm store load

sallocLetNoStore :: String -> DLVar -> App a -> App a
sallocLetNoStore lab dv km = do
  sallocVar ("LetNoStore: " <> lab) dv $ \_ cload -> do
    store_let dv cload km

sallocLet :: DLVar -> App () -> App a -> App a
sallocLet dv cgen km = do
  sallocVar "Let" dv $ \cstore cload -> do
    cgen
    cstore
    store_let dv cload km

sallocLetMay :: DLVar -> App () -> App a -> App a
sallocLetMay dv cgen km =
  maybeLoc dv >>= \case
    Nothing ->
      store_let dv cgen km
    Just _ ->
      sallocLet dv cgen km

sallocVarLet :: DLVarLet -> App () -> App a -> App a
sallocVarLet (DLVarLet mvc dv) cgen km = do
  let once = sallocLetMay dv cgen km
  case mvc of
    Nothing -> km
    Just DVC_Once -> once
    Just DVC_Many -> sallocLet dv cgen km

ctobs :: DLType -> App ()
ctobs = \case
  T_UInt UI_Word -> output (Titob False)
  T_UInt UI_256 -> nop
  T_Bool -> output (Titob True) >> output (TSubstring 7 8)
  T_Null -> nop
  T_Bytes _ -> nop
  T_BytesDyn -> nop
  T_StringDyn -> nop
  T_Digest -> nop
  T_Address -> nop
  T_Contract -> ctobs $ T_UInt UI_Word
  T_Token -> ctobs $ T_UInt UI_Word
  T_Array {} -> nop
  T_Tuple {} -> nop
  T_Object {} -> nop
  T_Data {} -> nop
  T_Struct {} -> nop

cfrombs :: DLType -> App ()
cfrombs = \case
  T_UInt UI_Word -> op "btoi"
  T_UInt UI_256 -> nop
  T_Bool -> op "btoi"
  T_Null -> nop
  T_Bytes _ -> nop
  T_BytesDyn -> nop
  T_StringDyn -> nop
  T_Digest -> nop
  T_Address -> nop
  T_Contract -> cfrombs $ T_UInt UI_Word
  T_Token -> cfrombs $ T_UInt UI_Word
  T_Array {} -> nop
  T_Tuple {} -> nop
  T_Object {} -> nop
  T_Data {} -> nop
  T_Struct {} -> nop

ctzero :: DLType -> App ()
ctzero = \case
  T_UInt UI_Word -> cint_ sb 0
  t -> do
    padding =<< typeSizeOf t
    cfrombs t

chkint :: SrcLoc -> Integer -> Integer
chkint at = checkIntLiteralC at conName' conCons'

cint_ :: SrcLoc -> Integer -> App ()
cint_ at i = output $ TInt $ chkint at i

instance Compile Integer where
  cp = cint_ sb

instance Compile Int where
  cp x = cp y
    where
      y :: Integer = fromIntegral x

cint :: Integer -> App ()
cint = cp

instance Compile DLLiteral where
  cp = \case
    DLL_Null -> cbs ""
    DLL_Bool b -> cint $ (if b then 1 else 0)
    DLL_Int at UI_Word i -> cint_ at i
    DLL_Int at UI_256 i ->
      cp $ itob 32 $ checkIntLiteral at "UInt256" 0 uint256_Max i
    DLL_TokenZero -> cint 0

instance Compile Bool where
  cp = cp . DLL_Bool

ca_boolb :: DLArg -> Maybe B.ByteString
ca_boolb = \case
  DLA_Literal (DLL_Bool b) ->
    Just $ B.singleton $ toEnum $ if b then 1 else 0
  _ -> Nothing

cas_boolbs :: [DLArg] -> Maybe B.ByteString
cas_boolbs = mconcat . map ca_boolb

instance Compile DLVar where
  cp = lookup_let

instance Compile DLArg where
  cp = \case
    DLA_Var v -> cp v
    DLA_Constant c -> cp $ conCons' c
    DLA_Literal c -> cp c
    DLA_Interact {} -> impossible "consensus interact"

czpad :: Integer -> App ()
czpad 0 = return ()
czpad xtra = do
  padding xtra
  op "concat"

abfField :: ALGOBlockField -> LT.Text
abfField = \case
  ABF_Seed -> "BlkSeed"
  ABF_Timestamp -> "BlkTimestamp"

cAlgoBlock :: ALGOBlockField -> [DLArg] -> App ()
cAlgoBlock which = \case
  [n] -> do
    cp n
    libCall (LF_blockRead which) $ do
      op "dup"
      libCall LF_blockCheck $ do
        -- [ n ]
        op "dup"
        -- [ n, n ]
        code "txn" [ "LastValid" ]
        cint 1002
        op "dup2"
        -- [ n, n, lv, 1002, lv, 1002 ]
        op "<"
        -- [ n, n, lv, 1002, would-underflow ]
        lNoOverflow <- freshLabel "NoOverflow"
        lFirstAvail <- freshLabel "FirstAvail"
        lBad <- freshLabel "Bad"
        code "bz" [ lNoOverflow ]
        -- [ n, n, lv, 1002 ]
        code "popn" ["2"]
        cint 1
        code "b" [ lFirstAvail ]
        label lNoOverflow
        -- [ n, n, lv, 1002 ]
        op "-"
        code "b" [ lFirstAvail ]
        label lFirstAvail
        -- [ n, n, firstAvail ]
        op "swap"
        -- [ n, firstAvail, n ]
        op ">"
        -- [ n, bad ]
        code "bnz" [ lBad ]
        code "txn" [ "FirstValid" ]
        -- [ n, fv ]
        op "dup"
        -- [ n, fv, fv ]
        code "bz" [ lBad ]
        -- [ n, fv ]
        cint 1
        op "-"
        -- [ n, lastAvail ]
        op ">"
        code "bnz" [ lBad ]
        cp True
        op "retsub"
        label lBad
        cp False
        op "retsub"
      -- [ n, okay ]
      lOkay <- freshLabel "Okay"
      code "bnz" [ lOkay ]
      let bfT = abfType which
      let f a = do
            cp $ mdaToMaybeLA bfT a
            op "retsub"
      -- [ n ]
      op "pop"
      -- []
      f Nothing
      label lOkay
      -- [ n ]
      code "block" [ abfField which ]
      -- [ fv ]
      fv <- allocVar sb bfT
      store_let fv (op "swap") $
        f $ Just $ DLA_Var fv
  _ -> impossible "cAlgoBlock"

cprim :: PrimOp -> [DLArg] -> App ()
cprim = \case
  SELF_ADDRESS {} -> impossible "self address"
  ADD t _ -> bcallz t "+"
  SUB t _ -> bcallz t "-"
  MUL t _ -> bcallz t "*"
  DIV t _ -> bcallz t "/"
  MOD t _ -> bcallz t "%"
  PLT t -> bcall t "<"
  PLE t -> bcall t "<="
  PEQ t -> bcall t "=="
  PGT t -> bcall t ">"
  PGE t -> bcall t ">="
  SQRT t -> bcallz t "sqrt"
  UCAST from to trunc pv -> \case
    [v] -> do
      case (from, to) of
        (UI_Word, UI_256) -> do
          padding $ 3 * 8
          cp v
          output $ Titob False
          op "concat"
        (UI_256, UI_Word) -> do
          cp v
          -- [ v ]
          let ext i = cint (8 * i) >> op "extract_uint64"
          unless (trunc || pv == PV_Veri) $ do
            comment "Truncation check"
            op "dup"
            op "bitlen"
            cint 64
            op "<="
            assert
          ext 3
        x -> impossible $ "ucast " <> show x
    _ -> impossible "cprim: UCAST args"
  MUL_DIV _ -> \case
    [x, y, z] -> do
      cp x
      cp y
      op "mulw"
      cp z
      op "divw"
    _ -> impossible "cprim: MUL_DIV args"
  LSH -> call "<<"
  RSH -> call ">>"
  BAND t -> bcallz t "&"
  BIOR t -> bcallz t "|"
  BXOR t -> bcallz t "^"
  DIGEST_XOR -> call "b^"
  BYTES_XOR -> call "b^"
  DIGEST_EQ -> call "=="
  ADDRESS_EQ -> call "=="
  TOKEN_EQ -> call "=="
  BTOI_LAST8 {} -> \case
    [x] -> do
      bl <- fromIntegral <$> (typeSizeOf $ argTypeOf x)
      let (start, len) = if bl > 8 then (bl - 8, 8) else (0, 0)
      cp x
      output $ TExtract start len
      op "btoi"
    _ -> impossible "btoiLast8"
  BYTES_ZPAD xtra -> \case
    [x] -> do
      cp x
      czpad xtra
    _ -> impossible $ "zpad"
  IF_THEN_ELSE -> \case
    [be, DLA_Literal (DLL_Bool True), DLA_Literal (DLL_Bool False)] -> do
      cp be
    [be, DLA_Literal (DLL_Bool False), DLA_Literal (DLL_Bool True)] -> do
      cp be
      op "!"
    [be, DLA_Literal (DLL_Bool True), fe] -> do
      cp be
      cp fe
      op "||"
    [be, DLA_Literal (DLL_Bool False), fe] -> do
      -- be \ fe |  T  | F
      --    T    |  F  | F
      --    F    |  T  | F
      cp be
      op "!"
      cp fe
      op "&&"
    [be, te, DLA_Literal (DLL_Bool False)] -> do
      cp be
      cp te
      op "&&"
    [be, te, DLA_Literal (DLL_Bool True)] -> do
      -- be \ te |  T  | F
      --    T    |  T  | F
      --    F    |  T  | T
      cp be
      op "!"
      cp te
      op "||"
    [be, te, fe] -> do
      cp fe
      cp te
      cp be
      op "select"
    _ -> impossible "ite args"
  CTC_ADDR_EQ -> \case
    [ ctca, aa ] -> do
      cContractToAddr ctca
      cp aa
      op "=="
    _ -> impossible "ctcAddrEq args"
  STRINGDYN_CONCAT -> call "concat" -- assumes two-args/type safe
  UINT_TO_STRINGDYN ui -> \case
    [i] -> do
      cp i
      case ui of
        UI_256 -> return ()
        UI_Word -> output $ Titob False
      bad "Uses UInt.toStringDyn"
    _ -> impossible "UInt.toStringDyn"
  GET_CONTRACT -> const $ do
    code "txn" ["ApplicationID"]
  GET_ADDRESS -> const $ cContractAddr
  GET_COMPANION -> const $ callCompanion sb CompanionGet
  ALGO_BLOCK bf -> cAlgoBlock bf
  where
    call o = \args -> do
      forM_ args cp
      op o
    bcall t o = call $ (if t == UI_256 then "b" else "") <> o
    bcallz t o args = do
      bcall t o args
      when (t == UI_256) $ do
        libCall LF_checkUInt256ResultLen $ do
          op "dup"
          op "len"
          cp =<< (typeSizeOf $ T_UInt UI_256)
          op "swap"
          op "-"
          -- This traps on purpose when the result is longer than 256
          op "bzero"
          op "swap"
          op "concat"
          op "retsub"

cContractToAddr :: DLArg -> App ()
cContractToAddr ctca = do
  cbs "appID"
  cp ctca
  ctobs $ T_UInt UI_Word
  op "concat"
  op "sha512_256"

cconcatbs_ :: (DLType -> App ()) -> [(DLType, App ())] -> App ()
cconcatbs_ f l = do
  totlen <- typeSizeOf $ T_Tuple $ map fst l
  check_concat_len totlen
  case l of
    [] -> padding 0
    _ -> do
      forM_ (zip l dont_concat_first) $ \((t, m), a) ->
        m >> f t >> a

cconcatbs :: [(DLType, App ())] -> App ()
cconcatbs = cconcatbs_ ctobs

check_concat_len :: Integer -> App ()
check_concat_len totlen =
  unless (totlen <= algoMaxStringSize) $ do
    bad $
      "Cannot `concat` " <> texty totlen
        <> " bytes; the resulting byte array must be <= 4096 bytes."
        <> " This is caused by a Reach data type being too large."

cdigest :: [(DLType, App ())] -> App ()
cdigest l = cconcatbs l >> op "sha256"

cextract :: Integer -> Integer -> App ()
cextract _s 0 = do
  op "pop"
  padding 0
cextract s l =
  case s < 256 && l < 256 && l /= 0 of
    True -> do
      output $ TExtract (fromIntegral s) (fromIntegral l)
    False -> do
      cp s
      cp l
      op "extract3"

creplace :: Integer -> App () -> App ()
creplace s cnew = do
  case s < 256 of
    True -> do
      cnew
      output $ TReplace2 (fromIntegral s)
    False -> do
      cp s
      cnew
      op "replace3"

computeExtract :: [DLType] -> Integer -> App (DLType, Integer, Integer)
computeExtract ts idx = do
  szs <- mapM typeSizeOf ts
  let starts = scanl (+) 0 szs
  let idx' = fromIntegral idx
  let tsz = zip3 ts starts szs
  case atMay tsz idx' of
    Nothing -> impossible "bad idx"
    Just x -> return x

cfor :: DLVar -> Integer -> (App () -> App ()) -> App ()
cfor _ 0 _ = return ()
cfor _ 1 body = body (cint 0)
cfor iv maxi body = do
  when (maxi < 2) $ impossible "cfor maxi=0"
  top_lab <- freshLabel "forTop"
  end_lab <- freshLabel "forEnd"
  sallocVar "cfor" iv $ \store_idx load_idx -> do
    cint 0
    store_idx
    code "b" [top_lab]
    label top_lab
    output $ TFor_top maxi
    body load_idx
    load_idx
    cint 1
    op "+"
    op "dup"
    store_idx
    cp maxi
    op "<"
    output $ TFor_bnz top_lab maxi end_lab
    code "b" [end_lab]
  label end_lab
  return ()

doArrayRef :: SrcLoc -> DLArg -> Bool -> Either DLArg (App ()) -> App ()
doArrayRef at aa frombs ie = do
  let (t, _) = argArrTypeLen aa
  cp aa
  cArrayRef at t frombs ie

cArrayRef :: SrcLoc -> DLType -> Bool -> Either DLArg (App ()) -> App ()
cArrayRef _at t frombs ie = do
  tsz <- typeSizeOf t
  let ie' =
        case ie of
          Left ia -> cp ia
          Right x -> x
  case t of
    T_Bool -> do
      ie'
      op "getbyte"
      case frombs of
        True -> nop
        False -> ctobs T_Bool
    _ -> do
      case ie of
        Left (DLA_Literal (DLL_Int _ UI_Word ii)) -> do
          let start = ii * tsz
          cextract start tsz
        _ -> do
          cp tsz
          ie'
          op "*"
          cp tsz
          op "extract3"
      case frombs of
        True -> cfrombs t
        False -> nop

instance Compile DLLargeArg where
  cp = \case
    DLLA_Array t as ->
      case t of
        T_Bool ->
          case cas_boolbs as of
            Nothing -> normal
            Just x -> cp x
        _ -> normal
      where
        normal = cconcatbs $ map (\a -> (t, cp a)) as
    DLLA_Tuple as ->
      cconcatbs $ map (\a -> (argTypeOf a, cp a)) as
    DLLA_Obj m -> cp $ DLLA_Struct $ M.toAscList m
    DLLA_Data tm vn va -> do
      let h ((k, v), i) = (k, (i, v))
      let tm' = M.fromList $ map h $ zip (M.toAscList tm) [0 ..]
      let (vi, vt) = fromMaybe (impossible $ "dla_data") $ M.lookup vn tm'
      cp $ B.singleton $ BI.w2c vi
      cp va
      ctobs vt
      vlen <- (+) 1 <$> typeSizeOf (argTypeOf va)
      op "concat"
      dlen <- typeSizeOf $ T_Data tm
      czpad $ fromIntegral $ dlen - vlen
      check_concat_len dlen
    DLLA_Struct kvs ->
      cconcatbs $ map (\a -> (argTypeOf a, cp a)) $ map snd kvs
    DLLA_Bytes bs -> cp bs
    DLLA_BytesDyn bs -> cp bs
    DLLA_StringDyn t -> cp $ bpack $ T.unpack t

instance Compile B.ByteString where
  cp bs = do
    when (B.length bs > fromIntegral algoMaxStringSize) $
      bad $ "Cannot create raw bytes of length greater than " <> texty algoMaxStringSize <> "."
    output $ TBytes bs

cbs :: B.ByteString -> App ()
cbs = cp

cTupleRef_ :: Bool -> DLType -> Integer -> App ()
cTupleRef_ optOne tt idx = do
  -- [ Tuple ]
  let ts = tupleTypes tt
  (t, start, sz) <- computeExtract ts idx
  case (ts, idx) of
    ([_], 0) | optOne ->
      return ()
    _ -> do
      cextract start sz
  -- [ ValueBs ]
  cfrombs t
  -- [ Value ]
  return ()

cTupleRef :: DLType -> Integer -> App ()
cTupleRef = cTupleRef_ True

cTupleSet :: SrcLoc -> App () -> DLType -> Integer -> App ()
cTupleSet _at cnew tt idx = do
  -- [ Tuple ]
  let ts = tupleTypes tt
  (t, start, _) <- computeExtract ts idx
  creplace start $ cnew >> ctobs t

divup :: Integer -> Integer -> Integer
divup x y = ceiling $ (fromIntegral x :: Double) / (fromIntegral y)

computeStateSizeAndKeys :: Monad m => NotifyFm m -> LT.Text -> Integer -> Integer -> m (Integer, [Word8])
computeStateSizeAndKeys badx prefix size limit = do
  let keys = size `divup` algoMaxAppBytesValueLen_usable
  when (keys > limit) $ do
    badx $ "Too many " <> prefix <> " keys, " <> texty keys <> ", but limit is " <> texty limit
  let keysl = take (fromIntegral keys) [0 ..]
  return (keys, keysl)

cSvsGet :: [Word8] -> App ()
cSvsGet [] = cbs ""
cSvsGet keysl = do
  -- [ SvsData_0? ]
  forM_ (zip keysl $ False : repeat True) $ \(mi, doConcat) -> do
    -- [ SvsData_N? ]
    cp $ keyVary mi
    -- [ SvsData_N?, Key ]
    op "app_global_get"
    -- [ SvsData_N?, NewPiece ]
    case doConcat of
      True -> op "concat"
      False -> nop
    -- [ SvsData_N+1 ]
    return ()
  -- [ SvsData_k ]
  return ()

getStateVars :: Int -> App [DLVar]
getStateVars which =
  (M.lookup which <$> asks getStateMap) >>= \case
    Nothing -> impossible $ "No state map for " <> show which
    Just vs -> return vs

cSvsLoad :: Int -> App ()
cSvsLoad which = libCall (LF_svsLoad which) $ do
  vs <- getStateVars which
  let t = T_Tuple $ map typeOf vs
  forM_ (labelLast $ zip vs [0..]) $ \((v, vi), isLast) -> do
    unless isLast $ op "dup"
    cTupleRef_ False t vi
    lookupVarColoring "svsLoad" v >>= \case
      Just loc -> output $ TStore loc $ texty v
      Nothing -> op "pop"
  op "retsub"

labelLast :: [a] -> [(a, Bool)]
labelLast l = reverse $
  case reverse l of
    x : xs -> (x, True) : map (flip (,) False) xs
    _ -> []

cSvsPut :: Integer -> [Word8] -> App ()
cSvsPut _ [] = op "pop"
cSvsPut size keysl = do
  -- [ SvsData ]
  forM_ (labelLast $ keysl) $ \(vi, isLast) -> do
    -- [ SvsData ]
    cp $ keyVary vi
    -- [ SvsData, Key ]
    case isLast of
      False -> do
        code "dig" ["1"]
        -- [ SvsData, Key, SvsData ]
        cStateSlice size vi
        -- [ SvsData, Key, ViewData' ]
        op "app_global_put"
        -- [ SvsData ]
        return ()
      True -> do
        op "swap"
        -- [ Key, SvsData ]
        cStateSlice size vi
        -- [ Key, ViewData' ]
        op "app_global_put"
        -- [ ]
        return ()

stBindAll :: App a -> App a
stBindAll k = do
  stMap <- asks getStateMap
  let stAllVars = mconcat $ map S.fromList $ M.elems stMap
  foldr (sallocLetNoStore "stBindAll") k stAllVars

cSvsDump :: Int -> App ()
cSvsDump which = libCall (LF_svsDump which) $ do
  vs <- getStateVars which
  stBindAll $ cp $ DLLA_Tuple $ map DLA_Var vs
  stActSize <- typeSizeOf $ T_Tuple $ map typeOf vs
  stMaxSize <- (liftIO . readIORef) =<< asks eStateSizeR
  czpad $ stMaxSize - stActSize
  op "retsub"

cGetBalance :: SrcLoc -> Maybe (App ()) -> Maybe DLArg -> App ()
cGetBalance _at mmin = \case
  Nothing -> do
    -- []
    cContractAddr
    op "balance"
    -- [ bal ]
    case mmin of
      Nothing -> do
        cContractAddr
        op "min_balance"
      Just m -> m
    -- [ bal, min_bal ]
    op "-"
  Just tok -> do
    cContractAddr
    incResource R_Asset tok
    cp tok
    code "asset_holding_get" [ "AssetBalance" ]
    op "pop"

cMapKey :: Int -> DLArg -> App (Integer, App ())
cMapKey i a = do
  let t = argTypeOf a
  la <- typeSizeOf t
  let isByte = i <= 255
  let rawLen = 1 + la
  let canBeRaw = isByte && rawLen <= algoMaxAppKeyLen
  let ca = cp a >> ctobs t
  let ctag = cp i >> (ctobs $ T_UInt UI_Word)
  case canBeRaw of
    True -> do
      return $ (,) rawLen $ do
        cbs $ BS.pack $ [ fromIntegral i ]
        ca
        op "concat"
    False -> do
      return $ (,) 32 $ do
        ctag
        ca
        op "concat"
        op "sha256"

instance Compile DLExpr where
  cp = \case
    DLE_Arg _ a -> cp a
    DLE_LArg _ a -> cp a
    DLE_Impossible at _ (Err_Impossible_Case s) ->
      impossible $ "ce: impossible case `" <> s <> "` encountered at: " <> show at
    DLE_Impossible at _ err -> expect_thrown at err
    DLE_VerifyMuldiv at _ _ _ err ->
      expect_thrown at err
    DLE_PrimOp _ p args -> cprim p args
    DLE_ArrayRef at aa ia -> doArrayRef at aa True (Left ia)
    DLE_ArraySet _ aa ia va -> do
      let (t, _) = argArrTypeLen aa
      case t of
        T_Bool -> do
          cp aa
          cp ia
          cp va
          op "setbyte"
        _ -> do
          let cnew = cp va >> ctobs t
          cp aa
          --- [ big ]
          tsz <- typeSizeOf t
          case ia of
            -- Static index
            DLA_Literal (DLL_Int _ UI_Word ii) -> do
              --- [ big ]
              creplace (ii * tsz) cnew
            _ -> do
              --- [ big ]
              cp ia
              cp tsz
              op "*"
              --- [ big, start ]
              cnew
              op "replace3"
    DLE_ArrayConcat _ x y -> do
      let (xt, xlen) = argArrTypeLen x
      let (_, ylen) = argArrTypeLen y
      cp x
      cp y
      xtz <- typeSizeOf xt
      check_concat_len $ (xlen + ylen) * xtz
      op "concat"
    DLE_BytesDynCast _ v -> do
      cp v
    DLE_TupleRef _ ta idx -> do
      cp ta
      cTupleRef (argTypeOf ta) idx
    DLE_TupleSet at tup_a index val_a -> do
      cp tup_a
      cTupleSet at (cp val_a) (argTypeOf tup_a) index
    DLE_ObjectRef _ obj_a fieldName -> do
      cp obj_a
      uncurry cTupleRef $ objectRefAsTupleRef obj_a fieldName
    DLE_ObjectSet at obj_a fieldName val_a -> do
      cp obj_a
      uncurry (cTupleSet at (cp val_a)) $ objectRefAsTupleRef obj_a fieldName
    DLE_Interact {} -> impossible "consensus interact"
    DLE_Digest _ args -> cdigest $ map go args
      where
        go a = (argTypeOf a, cp a)
    DLE_Transfer mt_at who mt_amt mt_mtok -> do
      let mt_always = False
      let mt_mrecv = Just $ Left who
      let mt_mcclose = Nothing
      let mt_next = False
      let mt_submit = True
      void $ makeTxn $ MakeTxn {..}
    DLE_TokenInit mt_at tok -> do
      let mt_always = True
      let mt_mtok = Just tok
      let mt_amt = DLA_Literal $ DLL_Int sb UI_Word 0
      let mt_mrecv = Nothing
      let mt_next = False
      let mt_submit = True
      let mt_mcclose = Nothing
      addInitTok tok
      cp minimumBalance_l >> mbrAdd
      void $ makeTxn $ MakeTxn {..}
    DLE_TokenAccepted _ addr tok -> do
      cp addr
      cp tok
      incResource R_Account addr
      incResource R_Asset tok
      code "asset_holding_get" [ "AssetBalance" ]
      op "swap"
      op "pop"
    DLE_CheckPay ct_at fs ct_amt ct_mtok -> do
      did <- checkTxn $ CheckTxn {..}
      when did $ show_stack "CheckPay" Nothing ct_at fs
    DLE_Claim at fs t a mmsg -> do
      let check = cp a >> assert
      case t of
        CT_Assert -> impossible "assert"
        CT_Assume -> check
        CT_Enforce -> check
        CT_Require -> check
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      show_stack "Claim" mmsg at fs
    DLE_Wait {} -> nop
    DLE_PartSet _ _ a -> cp a
    DLE_MapRef _ (DLMVar i) fa vt -> do
      incResource_ R_Box (i, fa)
      cbs ""
      padding =<< typeSizeOf vt
      (_, go) <- cMapKey i fa
      go
      void $ checkMapSize vt
      cMapRef
    DLE_MapSet _ (DLMVar i) fa vt mva -> do
      (liftIO . flip writeIORef True) =<< asks eDoesMapSetR
      incResource_ R_Box (i, fa)
      (ks, go) <- cMapKey i fa
      vts <- checkMapSize vt
      cint $ algoBoxFlatMinBalance + algoBoxByteMinBalance * (ks + vts)
      go
      case mva of
        Nothing -> cMapDel
        Just a -> do
          cp a
          ctobs vt
          cMapSet
    DLE_Remote at fs ro rng_ty (DLRemote rm' (DLPayAmt pay_net pay_ks) as (DLWithBill _nRecv nnRecv _nnZero) malgo) -> do
      let DLRemoteALGO {..} = malgo
      warn_lab <- asks eWhich >>= \case
        Just which -> return $ "Step " <> show which
        Nothing -> return $ "This program"
      warn $ LT.pack $
        warn_lab <> " calls a remote object at " <> show at <> ". This means that Reach's conservative analysis of resource utilization and fees is incorrect, because we cannot take into account the needs of the remote object. Furthermore, the remote object may require special transaction parameters which are not expressed in the Reach API or the Algorand ABI standards."
      let ts = map argTypeOf as
      let rm = fromMaybe (impossible "XXX") rm'
      sig <- signatureStr ra_addr2acc rm ts (Just rng_ty)
      remoteTxns <- liftIO $ newCounter 0
      let mayIncTxn m = do
            b <- m
            when b $
              void $ liftIO $ incCounter remoteTxns
            return b
      -- XXX It would be nice to remove the use of GV_remote* by looking at the
      -- stack
      --
      -- Figure out what we're calling
      cContractToAddr ro
      gvStore GV_remoteCallee
      cContractAddr
      op "min_balance"
      gvStore GV_remoteMinB
      cbs ""
      -- XXX We are caching the minimum balance because if we are deleting an
      -- application we made, then our minimum balance will decrease. The
      -- alternative is to track how much exactly it will go down by.
      let mmin = Just $ gvLoad GV_remoteMinB
      let mtoksBill = Nothing : map Just nnRecv
      let mtoksiAll = zip [0..] mtoksBill
      let (mtoksiBill, mtoksiZero) = splitAt (length mtoksBill) mtoksiAll
      let paid = M.fromList $ (Nothing, pay_net) : (map (\(x, y) -> (Just y, x)) pay_ks)
      let balsT = T_Tuple $ map (const $ T_UInt UI_Word) mtoksiAll
      let gb_pre _ mtok = do
            cGetBalance at mmin mtok
            case M.lookup mtok paid of
              Nothing -> return ()
              Just amt -> do
                cp amt
                op "-"
      cconcatbs $ map (\(i, mtok) -> (T_UInt UI_Word, gb_pre i mtok)) mtoksiAll
      gvStore GV_remoteBals
      -- Start the call
      let mt_at = at
      let mt_mcclose = Nothing
      let mt_mrecv = Just $ Right $ gvLoad GV_remoteCallee
      let mt_always = ra_strictPay
      let doNet isBefore mt_next = do
            let go = do
                  let mt_amt = pay_net
                  let mt_mtok = Nothing
                  let mt_submit = False
                  x <- mayIncTxn $ makeTxn $ MakeTxn {..}
                  return $ mt_next || x
            case (ra_txnOrderForward, isBefore) of
              (True, True) -> go
              (False, False) -> go
              _ -> return mt_next
      hadNetBefore <- doNet True False
      let foldMy a l f = foldM f a l
      let payOrder = if ra_txnOrderForward then id else reverse
      hadSome <- foldMy hadNetBefore (payOrder pay_ks) $ \mt_next (mt_amt, tok) -> do
        let mt_mtok = Just tok
        let mt_submit = False
        x <- mayIncTxn $ makeTxn $ MakeTxn {..}
        return $ mt_next || x
      hadNetAfter <- doNet False hadSome
      itxnNextOrBegin hadNetAfter
      output $ TConst "appl"
      makeTxn1 "TypeEnum"
      cp ro
      incResource R_App ro
      makeTxn1 "ApplicationID"
      unless ra_rawCall $ do
        cp $ sigStrToBytes sig
        makeTxn1 "ApplicationArgs"
      accountsR <- liftIO $ newCounter 1
      let processArg a = do
            cp a
            let t = argTypeOf a
            ctobs t
            case t of
              -- XXX This is bad and will not work in most cases
              T_Address -> do
                incResource R_Account a
                let m = makeTxn1 "Accounts"
                case ra_addr2acc of
                  False -> do
                    op "dup"
                    m
                  True -> do
                    i <- liftIO $ incCounter accountsR
                    m
                    cp i
                    ctobs $ T_UInt UI_Word
              _ -> return ()
      let processArg' a = do
            processArg a
            makeTxn1 "ApplicationArgs"
      let processArgTuple tas = do
            cconcatbs_ (const $ return ()) $
              map (\a -> (argTypeOf a, processArg a)) tas
            makeTxn1 "ApplicationArgs"
      case splitArgs as of
        (_, Nothing) -> do
          forM_ as processArg'
        (as14, Just asMore) -> do
          forM_ as14 processArg'
          processArgTuple asMore
      -- XXX If we can "inherit" resources, then this needs to be removed and
      -- we need to check that nnZeros actually stay 0
      forM_ (ra_assets <> map snd pay_ks <> nnRecv) $ \a -> do
        incResource R_Asset a
        cp a
        makeTxn1 "Assets"
      forM_ ra_boxes $ \a -> do
        incResource_ R_Box (-1, a)
      forM_ ra_accounts $ \a -> do
        incResource R_Account a
        cp a
        makeTxn1 "Accounts"
      forM_ ra_apps $ \a -> do
        incResource R_App a
        cp a
        makeTxn1 "Applications"
      let oc f = do
            output $ TConst f
            makeTxn1 "OnCompletion"
      case ra_onCompletion of
        RA_NoOp -> return ()
        RA_OptIn -> oc "OptIn"
        RA_CloseOut -> oc "CloseOut"
        RA_ClearState -> oc "ClearState"
        RA_UpdateApplication -> oc "UpdateApplication"
        RA_DeleteApplication -> oc "DeleteApplication"
      op "itxn_submit"
      show_stack ("Remote: " <> sig) Nothing at fs
      appl_idx <- liftIO $ readCounter remoteTxns
      let gb_post idx mtok = do
            cGetBalance at mmin mtok
            gvLoad GV_remoteBals
            cTupleRef balsT idx
            op "-"
      cconcatbs $ map (\(i, mtok) -> (T_UInt UI_Word, gb_post i mtok)) mtoksiBill
      forM_ mtoksiZero $ \(idx, mtok) -> do
        cGetBalance at mmin mtok
        gvLoad GV_remoteBals
        cTupleRef balsT idx
        asserteq
      code "gitxn" [ texty appl_idx, "LastLog" ]
      output $ TExtract 4 0 -- (0 = to the end)
      op "concat"
    DLE_TokenNew at (DLTokenNew {..}) -> do
      cp minimumBalance_l >> mbrAdd
      itxnNextOrBegin False
      let vTypeEnum = "acfg"
      output $ TConst vTypeEnum
      makeTxn1 "TypeEnum"
      cp dtn_supply >> makeTxn1 "ConfigAssetTotal"
      maybe (cint_ at 6) cp dtn_decimals >> makeTxn1 "ConfigAssetDecimals"
      cp dtn_sym >> makeTxn1 "ConfigAssetUnitName"
      cp dtn_name >> makeTxn1 "ConfigAssetName"
      cp dtn_url >> makeTxn1 "ConfigAssetURL"
      cp dtn_metadata >> makeTxn1 "ConfigAssetMetadataHash"
      cContractAddr >> makeTxn1 "ConfigAssetManager"
      op "itxn_submit"
      code "itxn" ["CreatedAssetID"]
    DLE_TokenBurn {} ->
      -- Burning does nothing on Algorand, because we already own it and we're
      -- the creator, and that's the rule for being able to destroy
      return ()
    DLE_TokenDestroy _at aida -> do
      itxnNextOrBegin False
      let vTypeEnum = "acfg"
      output $ TConst vTypeEnum
      makeTxn1 "TypeEnum"
      incResource R_Asset aida
      cp aida
      makeTxn1 "ConfigAsset"
      op "itxn_submit"
      cp minimumBalance_l >> mbrSub
    DLE_TimeOrder {} -> impossible "timeorder"
    DLE_EmitLog at k vs -> do
      let internal = do
            (v, n) <- case vs of
              [v'@(DLVar _ _ _ n')] -> return (v', n')
              _ -> impossible "algo ce: Expected one value"
            clog $
              [ DLA_Literal (DLL_Int at UI_Word $ fromIntegral n)
              , DLA_Var v
              ]
            cp v
            return $ v
      case k of
        L_Internal -> void $ internal
        L_Api {} -> do
          v <- internal
          -- `internal` just pushed the value of v onto the stack.
          -- We know it is not going to be used, so we can consume it.
          -- We know that CLMemorySet will consume it and doesn't do stack
          -- manipulation, so we say that to compile it, you do a nop op, thus
          -- it will be consumed.
          store_let v nop $
            cpk nop $ CLMemorySet at "api" (DLA_Var v)
        L_Event ml en -> do
          let name = maybe en (\l -> bunpack l <> "_" <> en) ml
          clogEvent name vs
          -- Event log values are never used, so we don't push anything
          return ()
    DLE_setApiDetails at p _ _ _ -> do
      Env {..} <- ask
      let which = fromMaybe (impossible "setApiDetails no which") eWhich
      let p' = LT.pack $ adjustApiName (LT.unpack $ apiLabel p) which True
      let lr_at = at
      let lr_lab = p'
      let lr_what = bunpack $ "API " <> p
      liftIO $ modifyIORef eApiLs $ (<>) [LabelRec {..}]
      callCompanion at $ CompanionLabel True p'
    DLE_GetUntrackedFunds at mtok tb -> do
      after_lab <- freshLabel "getActualBalance"
      cGetBalance at Nothing mtok
      -- [ bal ]
      cp tb
      -- [ bal, rsh_bal ]
      case mtok of
        Nothing -> do
          -- [ bal, rsh_bal ]
          op "-"
          -- [ extra ]
          code "b" [ after_lab ]
        Just _ -> do
          cb_lab <- freshLabel $ "getUntrackedFunds" <> "_z"
          -- [ bal, rsh_bal ]
          op "dup2"
          -- [ bal, rsh_bal, bal, rsh_bal ]
          op "<"
          -- [ bal, rsh_bal, {0, 1} ]
          -- Branch IF the bal < rsh_bal
          code "bnz" [ cb_lab ]
          -- [ bal, rsh_bal ]
          op "-"
          code "b" [ after_lab ]
          -- This happens because of clawback
          label cb_lab
          -- [ bal, rsh_bal ]
          op "pop"
          -- [ bal ]
          op "pop"
          -- [  ]
          cint 0
          -- [ extra ]
          code "b" [ after_lab ]
      label after_lab
    DLE_DataTag _ d -> do
      cp d
      cGetTag
    DLE_FromSome _ mo da -> do
      let t = typeOf da
      cp da
      ctobs t
      cp mo
      libCall LF_fromSome $ do
        -- [ Default, Object ]
        op "dup"
        -- [ Default, Object, Object ]
        output $ TExtract 1 0 -- (0 = to the end)
        -- [ Default, Object, Value ]
        op "swap"
        -- [ Default, Value, Object ]
        cGetTag
        -- [ Default, Object,  Tag ]
        -- [   False,   True, Cond ]
        op "select"
        op "retsub"
      cfrombs t
    DLE_ContractFromAddress _at _addr -> do
      cp $ mdaToMaybeLA T_Contract Nothing
    DLE_ContractNew _at cns dr -> do
      (liftIO . flip writeIORef True) =<< asks eDoesContractNewR
      let DLContractNew {..} = cns M.! conName'
      let ALGOCodeOut {..} = either impossible id $ aesonParse dcn_code
      let ALGOCodeOpts {..} = either impossible id $ aesonParse dcn_opts
      let ai_GlobalNumUint = aco_globalUints
      let ai_GlobalNumByteSlice = aco_globalBytes
      let ai_LocalNumUint = aco_localUints
      let ai_LocalNumByteSlice = aco_localBytes
      let ai_ExtraProgramPages =
            extraPages $ length aco_approval + length aco_clearState
      let appInfo = AppInfo {..}
      (cp $ minimumBalance_app appInfo ApplTxn_Create) >> mbrAdd
      itxnNextOrBegin False
      let vTypeEnum = "appl"
      output $ TConst vTypeEnum
      makeTxn1 "TypeEnum"
      let cbss f bs = do
            let (before, after) = B.splitAt (fromIntegral algoMaxStringSize) bs
            cp before
            makeTxn1 f
            unless (B.null after) $
              cbss f after
      cbss "ApprovalProgramPages" $ B.pack aco_approval
      cbss "ClearStateProgramPages" $ B.pack aco_clearState
      let unz f n = unless (n == 0) $ cp n >> makeTxn1 f
      unz "GlobalNumUint" $ ai_GlobalNumUint
      unz "GlobalNumByteSlice" $ ai_GlobalNumByteSlice
      unz "LocalNumUint" $ ai_LocalNumUint
      unz "LocalNumByteSlice" $ ai_LocalNumByteSlice
      unz "ExtraProgramPages" $ ai_ExtraProgramPages
      -- XXX support all of the DLRemote options
      let DLRemote _ _ as _ _ = dr
      forM_ as $ \a -> do
        cp a
        let t = argTypeOf a
        ctobs t
        makeTxn1 "ApplicationArgs"
      op "itxn_submit"
      code "itxn" ["CreatedApplicationID"]
    where
      -- On ALGO, objects are represented identically to tuples of their fields in ascending order.
      -- Consequently, we can pretend objects are tuples and use tuple functions as a shortcut.
      objectRefAsTupleRef :: DLArg -> String -> (DLType, Integer)
      objectRefAsTupleRef obj_a fieldName = (objAsTup_t, fieldIndex)
        where
          fieldIndex = objstrFieldIndex obj_t fieldName
          objAsTup_t = T_Tuple $ map snd $ objstrTypes obj_t
          obj_t = argTypeOf obj_a
      show_stack :: String -> Maybe BS.ByteString -> SrcLoc -> [SLCtxtFrame] -> App ()
      show_stack what msg at fs = do
        let msg' =
              case msg of
                Nothing -> ""
                Just x -> ": " <> x
        comment $ LT.pack $ "^ " <> what <> (bunpack msg')
        comment $ LT.pack $ "at " <> (unsafeRedactAbsStr $ show at)
        forM_ fs $ \f ->
          comment $ LT.pack $ unsafeRedactAbsStr $ show f

splitArgs :: [a] -> ([a], Maybe [a])
splitArgs l =
  -- If there are more than 15 args to an API on ALGO,
  -- args 15+ are packed as a tuple in arg 15.
  case 15 < (length l) of
    False -> (l, Nothing)
    True -> (before, Just after) where (before, after) = splitAt 14 l

signatureStr :: Bool -> String -> [DLType] -> Maybe DLType -> App String
signatureStr addr2acc f args mret = do
  args' <- mapM (typeSig_ addr2acc False) args
  rets <- fromMaybe "" <$> traverse (typeSig_ False True) mret
  return $ f <> "(" <> intercalate "," args' <> ")" <> rets

sigStrToBytes :: String -> BS.ByteString
sigStrToBytes sig = shabs
  where
    sha = hashWith SHA512t_256 $ bpack sig
    shabs = BS.take 4 $ BA.convert sha

clogEvent :: String -> [DLVar] -> App ()
clogEvent eventName vs = do
  sigStr <- signatureStr False eventName (map varType vs) Nothing
  let as = map DLA_Var vs
  let cheader = cp (bpack sigStr) >> op "sha512_256" >> output (TSubstring 0 4)
  cconcatbs $ (T_Bytes 4, cheader) : map (\a -> (argTypeOf a, cp a)) as
  sz <- typeSizeOf $ largeArgTypeOf $ DLLA_Tuple as
  clog_ $ 4 + sz
  comment $ LT.pack $ "^ log: " <> show eventName <> " " <> show vs <> " " <> show sigStr

clog_ :: Integer -> App ()
clog_ = output . TLog

clog :: [DLArg] -> App ()
clog as = do
  let la = DLLA_Tuple as
  cp la
  sz <- typeSizeOf $ largeArgTypeOf la
  clog_ sz

data CheckTxn = CheckTxn
  { ct_at :: SrcLoc
  , ct_amt :: DLArg
  , ct_mtok :: Maybe DLArg
  }

data MakeTxn = MakeTxn
  { mt_at :: SrcLoc
  , mt_mrecv :: Maybe (Either DLArg (App ()))
  , mt_mcclose :: Maybe (App ())
  , mt_amt :: DLArg
  , mt_always :: Bool
  , mt_mtok :: Maybe DLArg
  , mt_next :: Bool
  , mt_submit :: Bool
  }

makeTxn1 :: LT.Text -> App ()
makeTxn1 f = code "itxn_field" [f]

checkTxnUsage_ :: (DLArg -> App Bool) -> AlgoError -> SrcLoc -> Maybe DLArg -> App ()
checkTxnUsage_ isXTok err at mtok = do
  case mtok of
    Just tok -> do
      x <- isXTok tok
      when x $ do
        bad $ LT.pack $ getErrorMessage [] at True err
    Nothing -> return ()

makeTxnUsage :: SrcLoc -> Maybe DLArg -> App ()
makeTxnUsage = checkTxnUsage_ isNewTok Err_TransferNewToken

checkTxnUsage :: SrcLoc -> Maybe DLArg -> App ()
checkTxnUsage = checkTxnUsage_ isInitTok Err_PayNewToken

ntokFields :: (LT.Text, LT.Text, LT.Text, LT.Text)
ntokFields = ("pay", "Receiver", "Amount", "CloseRemainderTo")

tokFields :: (LT.Text, LT.Text, LT.Text, LT.Text)
tokFields = ("axfer", "AssetReceiver", "AssetAmount", "AssetCloseTo")

checkTxn_lib :: Bool -> App ()
checkTxn_lib tok = libCall (LF_checkTxn tok) $ do
  let get1 f = code "gtxns" [f]
  let (vTypeEnum, fReceiver, fAmount, _fCloseTo) =
        if tok then tokFields else ntokFields
  -- init: False: [ amt ]
  -- init:  True: [ amt, tok ]
  useResource R_Txn
  code "txn" ["GroupIndex"]
  gvLoad GV_txnCounter
  cint 1
  op "+"
  op "dup"
  gvStore GV_txnCounter
  op "-"
  dupn $ 2 + (if tok then 1 else 0)
  -- init <> [ id, id, id, id? ]
  get1 fReceiver
  cContractAddr
  cfrombs T_Address
  asserteq
  get1 "TypeEnum"
  output $ TConst vTypeEnum
  asserteq
  -- init <> [ id, id? ]
  when tok $ do
    get1 "XferAsset"
    code "uncover" [ "2" ]
    asserteq
  get1 fAmount
  asserteq
  op "retsub"

checkTxn :: CheckTxn -> App Bool
checkTxn (CheckTxn {..}) =
  case staticZero ct_amt of
    True -> return False
    False -> do
      checkTxnUsage ct_at ct_mtok
      cp ct_amt
      case ct_mtok of
        Nothing -> do
          checkTxn_lib False
        Just tok -> do
          cp tok
          checkTxn_lib True
      return True

itxnNextOrBegin_ :: Bool -> App ()
itxnNextOrBegin_ isNext = do
  op (if isNext then "itxn_next" else "itxn_begin")

itxnNextOrBegin :: Bool -> App ()
itxnNextOrBegin isNext = do
  itxnNextOrBegin_ isNext
  makeTxn1Fee

makeTxn1Fee :: App ()
makeTxn1Fee = do
  -- We do this because by default it will inspect the remaining fee and only
  -- set it to zero if there is a surplus, which means that sometimes it means
  -- 0 and sometimes it means "take money from the escrow", which is dangerous,
  -- so we force it to be 0 here. The alternative would be to check that other
  -- fees were set correctly, but I believe that would be more annoying to
  -- track, so I don't.
  cint 0
  makeTxn1 "Fee"

makeTxn :: MakeTxn -> App Bool
makeTxn (MakeTxn {..}) =
  case (mt_always || not (staticZero mt_amt)) of
    False -> return False
    True -> do
      makeTxnUsage mt_at mt_mtok
      cp mt_amt
      whenJust mt_mtok $ \tok -> do
        incResource R_Asset tok
        cp tok
      case mt_mrecv of
        Nothing -> cContractAddr
        Just (Left a) -> do
          incResource R_Account a
          cp a
        Just (Right cr) -> cr
      whenJust mt_mcclose $ \cclose -> cclose
      makeTxn_lib mt_next (isJust mt_mcclose) (isJust mt_mtok) mt_submit
      return True

makeTxn_lib :: Bool -> Bool -> Bool -> Bool -> App ()
makeTxn_lib isNext isClose isTokTxn isSubmit = libCall (LF_makeTxn isNext isClose isTokTxn isSubmit) $ do
  let (vTypeEnum, fReceiver, fAmount, fCloseTo) = if isTokTxn then tokFields else ntokFields
  itxnNextOrBegin_ isNext
  when isClose $ do
    cfrombs T_Address
    makeTxn1 fCloseTo
  libJump (LF_makeTxnK isTokTxn isSubmit) $ do
    makeTxn1Fee
    output $ TConst vTypeEnum
    makeTxn1 "TypeEnum"
    cfrombs T_Address
    makeTxn1 fReceiver
    when isTokTxn $ do
      makeTxn1 "XferAsset"
    makeTxn1 fAmount
    when isSubmit $ op "itxn_submit"
    op "retsub"

cmatch :: (CompileLabel a) => App () -> [(BS.ByteString, a)] -> App ()
cmatch ca es = do
  code "pushbytess" $ map (base64d . fst) es
  ca
  cswatchTail "match" (map snd es)

cswatchTail :: (CompileLabel a) => TealOp -> [a] -> App ()
cswatchTail w es = do
  els <- forM es $ \e -> do
    (l, ce) <- cpl e
    return (l, ce)
  code w $ map fst els
  code "b" ["errl"]
  forM_ els $ \(l, ce) -> label l >> ce

doSwitch :: (a -> App ()) -> DLVar -> SwitchCases a -> App ()
doSwitch ck dv csm = do
  cp dv
  cGetTag
  cswatchTail "switch" (map (SwitchX ck) $ switchUses dv csm)

data SwitchX a = SwitchX (a -> App ()) (SwitchCaseUse a)

instance CompileLabel (SwitchX a) where
  cpl (SwitchX ck (SwitchCaseUse dv vn (SwitchCase {..}))) = do
    l <- freshLabel vn
    return $ (,) l $ do
      sallocVarLet sc_vl (cextractDataOf (cp dv) (typeOf sc_vl)) $
        ck sc_k

cextractDataOf :: App () -> DLType -> App ()
cextractDataOf cd vt = do
  sz <- typeSizeOf vt
  case sz == 0 of
    True -> padding 0
    False -> do
      cd
      cextract 1 sz
      cfrombs vt

instance CompileK DLStmt where
  cpk km = \case
    DL_Nop _ -> km
    DL_Let _ DLV_Eff de -> cp de >> km
    DL_Let _ (DLV_Let vc dv) de -> do
      recordNew <-
        case de of
          DLE_TokenNew {} -> do
            return True
          DLE_EmitLog _ _ [dv'] -> do
            isNewTok $ DLA_Var dv'
          _ -> do
            return False
      when recordNew $
        addNewTok $ DLA_Var dv
      sallocVarLet (DLVarLet (Just vc) dv) (cp de) km
    DL_ArrayMap at (DLV_Let _ ansv) as xs (DLVarLet _ iv) (DLBlock _ _ body ra) -> do
      anssz <- typeSizeOf $ argTypeOf $ DLA_Var ansv
      let xlen = arraysLength as
      let rt = argTypeOf ra
      check_concat_len anssz
      sallocVar "ArrayMap" ansv $ \store_ans load_ans -> do
        cbs ""
        store_ans
        cfor iv xlen $ \load_idx -> do
          load_ans
          arrayBody at ra body iv load_idx xs as
          ctobs rt
          op "concat"
          store_ans
        store_let ansv load_ans km
    DL_ArrayMap at DLV_Eff as xs (DLVarLet _ iv) (DLBlock _ _ body ra) -> do
      let xlen = arraysLength as
      cfor iv xlen $ \load_idx -> do
        arrayBody at ra body iv load_idx xs as
      km
    DL_ArrayReduce at (DLV_Let _ ansv) as za (DLVarLet _ av) xs (DLVarLet _ iv) (DLBlock _ _ body ra) -> do
      let xlen = arraysLength as
      sallocVar "ArrayReduce" ansv $ \store_ans load_ans -> do
        cp za
        store_ans
        store_let av load_ans $ do
          cfor iv xlen $ \load_idx -> do
            arrayBody at ra body iv load_idx xs as
            store_ans
        store_let ansv load_ans km
    DL_ArrayReduce at DLV_Eff as za (DLVarLet _ av) xs (DLVarLet _ iv) (DLBlock _ _ body ra) -> do
      let xlen = arraysLength as
      sallocVar "ArrayReduce Eff" av $ \store_a load_a -> do
        cp za
        store_a
        cfor iv xlen $ \load_idx -> do
          store_let av load_a $
            arrayBody at ra body iv load_idx xs as
          store_a
      km
    DL_Var _ dv ->
      sallocVar "Var" dv $ \cstore cload -> do
        store_var dv cstore $
          store_let dv cload $
            km
    DL_Set _ dv da -> do
      cstore <- lookup_var dv
      cp da
      cstore
      km
    DL_LocalIf _ _ a tp fp -> do
      cp a
      false_lab <- freshLabel "localIfF"
      join_lab <- freshLabel "localIfK"
      code "bz" [false_lab]
      let j = code "b" [join_lab]
      cpk j tp
      label false_lab
      cpk j fp
      label join_lab
      km
    DL_LocalSwitch _ dv csm -> do
      end_lab <- freshLabel $ "LocalSwitchK"
      doSwitch (cpk (code "b" [end_lab])) dv csm
      label end_lab
      km
    DL_MapReduce {} ->
      impossible $ "cannot inspect maps at runtime"
    DL_Only {} ->
      impossible $ "only in CP"
    DL_LocalDo _ _ t -> cpk km t
    where
      arrayBody at ra body iv load_idx xs as = do
        let finalK = cpk (cp ra) body
        let finalK' = store_let iv load_idx finalK
        let bodyF (vl, a) =
              sallocVarLet vl (doArrayRef at a True $ Right load_idx)
        foldr bodyF finalK' $ zip xs as

instance CompileK DLTail where
  cpk km = \case
    DT_Return _ -> km
    DT_Com m k -> cpk (cpk km k) m

-- Reach Constants
reachAlgoBackendVersion :: Int
reachAlgoBackendVersion = 13

-- State:
keyState :: B.ByteString
keyState = ""

keyVary :: Word8 -> B.ByteString
keyVary = B.singleton . BI.w2c

cContractAddr :: App ()
cContractAddr = code "global" ["CurrentApplicationAddress"]

cDeployer :: App ()
cDeployer = code "global" ["CreatorAddress"]

aDeployer :: DLArg
aDeployer = DLA_Var $ DLVar sb Nothing T_Address (-1)
vDMbr :: DLVar
vDMbr = DLVar sb Nothing (T_UInt UI_Word) (-2)
aDMbr :: DLArg
aDMbr = DLA_Var vDMbr

data GlobalVar
  = GV_txnCounter
  | GV_currentStep
  | GV_currentTime
  | GV_wasntMeth
  | GV_apiRet
  | GV_companion
  | GV_mbrAdd
  | GV_mbrSub
  | GV_remoteCallee
  | GV_remoteMinB
  | GV_remoteBals
  deriving (Eq, Ord, Show, Enum, Bounded)

gvSlot :: GlobalVar -> ScratchSlot
gvSlot ai = fromIntegral $ fromEnum ai

gvOutput :: (ScratchSlot -> LT.Text -> TEAL) -> GlobalVar -> App ()
gvOutput f gv = output $ f (gvSlot gv) (textyt gv (gvType gv))

gvStore :: GlobalVar -> App ()
gvStore = gvOutput TStore

gvLoad :: GlobalVar -> App ()
gvLoad = gvOutput TLoad

gvType :: GlobalVar -> DLType
gvType = \case
  GV_txnCounter -> T_UInt UI_Word
  GV_currentStep -> T_UInt UI_Word
  GV_currentTime -> T_UInt UI_Word
  GV_companion -> T_Contract
  GV_wasntMeth -> T_Bool
  GV_apiRet -> T_Null
  GV_mbrAdd -> T_UInt UI_Word
  GV_mbrSub -> T_UInt UI_Word
  GV_remoteCallee -> T_Address
  GV_remoteMinB -> T_Tuple []
  GV_remoteBals -> T_Tuple []

cRound :: App ()
cRound = code "global" ["Round"]

apiLabel :: SLPart -> Label
apiLabel w = "api_" <> (LT.pack $ bunpack w)

data CompanionCall
  = CompanionCreate
  | CompanionLabel Bool Label
  | CompanionDelete
  | CompanionDeletePre
  | CompanionGet
  deriving (Eq, Show)
callCompanion :: SrcLoc -> CompanionCall -> App ()
callCompanion at cc = do
  mcr <- asks eCompanion
  CompanionRec {..} <- asks eCompanionRec
  let credit = output . TCostCredit
  let startCall ctor del = do
        itxnNextOrBegin False
        output $ TConst "appl"
        makeTxn1 "TypeEnum"
        case ctor of
          True -> do
            incResource R_App $ DLA_Literal $ DLL_Int at UI_Word 0
            freeResource R_App $ (0, cr_ro)
          False -> do
            cp cr_ro
            unless del $ do
              incResource R_App cr_ro
            makeTxn1 "ApplicationID"
  case cc of
    CompanionGet -> do
      comment $ texty cc
      let t = T_Contract
      let go = cp . mdaToMaybeLA t
      case mcr of
        Nothing -> go Nothing
        Just _ -> do
          dv <- allocVar at t
          store_let dv (gvLoad GV_companion) $
            go $ Just $ DLA_Var dv
    CompanionCreate -> do
      let mpay pc = (cp $ pc * algoMinimumBalance) >> mbrAdd
      case mcr of
        Nothing -> do
          mpay 1
        Just _ -> do
          mpay 2
          comment $ texty cc
          startCall True False
          cp cr_approval
          makeTxn1 "ApprovalProgram"
          cp cr_clearstate
          makeTxn1 "ClearStateProgram"
          op "itxn_submit"
          credit cr_ctor
          code "itxn" ["CreatedApplicationID"]
          gvStore GV_companion
          return ()
    CompanionLabel mk l -> do
      when mk $ do
        code "b" [l]
        label l
      whenJust mcr $ \cim -> do
        let howManyCalls = fromIntegral $ fromMaybe 0 $ M.lookup l cim
        -- XXX bunch into groups of 16, slightly less cost
        comment $ texty cc
        replicateM_ howManyCalls $ libCall LF_companionCall $ do
          startCall False False
          op "itxn_submit"
          op "retsub"
        replicateM_ howManyCalls $ do
          credit cr_call
        return ()
    CompanionDelete ->
      whenJust mcr $ \_ -> do
        comment $ texty cc
        startCall False True
        output $ TConst $ "DeleteApplication"
        makeTxn1 "OnCompletion"
        op "itxn_submit"
        credit cr_del
        cp algoMinimumBalance >> mbrSub
        return ()
    CompanionDeletePre ->
      whenJust mcr $ \_ -> do
        comment $ texty cc
        incResource R_App cr_ro

cStateSlice :: Integer -> Word8 -> App ()
cStateSlice size iw = do
  let i = fromIntegral iw
  let k = algoMaxAppBytesValueLen_usable
  let s = k * i
  let e = min size $ k * (i + 1)
  cextract s (e - s)

compileTEAL_ :: String -> IO (Either BS.ByteString CodeAndMap)
compileTEAL_ tealf = do
  let bcf = tealf <> ".tok"
  (ec, _stdout, stderr) <- readProcessWithExitCode "goal" ["clerk", "compile", "-m", tealf, "-o", bcf] mempty
  case ec of
    ExitFailure _ -> return $ Left stderr
    ExitSuccess -> do
      bc <- BS.readFile bcf
      readSourceMapFile (bcf <> ".map") >>= \case
        Left m -> return $ Left m
        Right sm -> return $ Right (bc, sm)

compileTEAL :: String -> IO CodeAndMap
compileTEAL tealf = compileTEAL_ tealf >>= \case
  Left stderr -> do
    let failed = impossible $ "The TEAL compiler failed with the message:\n" <> show stderr
    let tooBig = bpack tealf <> ": app program size too large: "
    case BS.isPrefixOf tooBig stderr of
      True -> do
        let notSpace = (32 /=)
        let sz_bs = BS.takeWhile notSpace $ BS.drop (BS.length tooBig) stderr
        let mlen :: Maybe Int = readMaybe $ bunpack sz_bs
        case mlen of
          Nothing -> failed
          Just sz -> return $ (BS.replicate sz 0, mempty)
      False -> failed
  Right x -> return x

-- CL Case
instance CompileK CLStmt where
  cpk k = \case
    CLDL m -> cpk k m
    CLBindSpecial _ lv sp ->
      case lv of
        DLV_Eff -> k
        DLV_Let _ v -> do
          let c = case sp of
                CLS_TxnFrom -> do
                  freeResource R_Account $ (0, DLA_Var v)
                  code "txn" ["Sender"]
                CLS_TxnTime -> cRound
                CLS_TxnSecs -> code "global" ["LatestTimestamp"]
                CLS_StorageState -> gvLoad GV_currentStep
          sallocLetMay v c k
    CLTimeCheck _ given -> do
      cp given
      libCall LF_timeCheck $ do
        op "dup"
        cint 0
        op "=="
        op "swap"
        gvLoad GV_currentTime
        op "=="
        op "||"
        assert
        op "retsub"
      k
    CLEmitPublish _ which vars -> do
      clogEvent ("_reach_e" <> show which) vars >> k
    CLStateBind _at isSafe _svs_vl prev -> do
      unless isSafe $ do
        cp prev
        gvLoad GV_currentStep
        asserteq
      cSvsLoad prev
      -- XXX statically assert prev's vars are svs_vl
      k
    CLIntervalCheck _ timev secsv (CBetween ifrom ito) -> do
      let checkTime1 :: LT.Text -> App () -> DLArg -> App ()
          checkTime1 cmp clhs rhsa = do
            clhs
            cp rhsa
            op cmp
            assert
      let checkFrom_ = checkTime1 ">="
      let checkTo_ = checkTime1 "<"
      let makeCheck check_ = \case
            Left x -> check_ (cp timev) x
            Right x -> check_ (cp secsv) x
      let checkFrom = makeCheck checkFrom_
      let checkTo = makeCheck checkTo_
      let checkBoth v xx yy = do
            cp v
            checkFrom_ (op "dup") xx
            checkTo_ (return ()) yy
      case (ifrom, ito) of
        (Nothing, Nothing) -> return ()
        (Just x, Nothing) -> checkFrom x
        (Nothing, Just y) -> checkTo y
        (Just x, Just y) ->
          case (x, y) of
            (Left xx, Left yy) -> checkBoth timev xx yy
            (Right xx, Right yy) -> checkBoth secsv xx yy
            (_, _) -> checkFrom x >> checkFrom y
      k
    CLStateSet _at which svs -> do
      mapM_ (\SvsPut {..} -> cmove svsp_svs svsp_val) svs
      cSvsDump which
      cp which
      cRound
      k
    CLTokenUntrack at tok -> do
      incResource R_Account aDeployer
      let mt_at = at
      let mt_always = True
      let mt_mrecv = Nothing
      let mt_submit = True
      let mt_next = False
      let mt_mcclose = Just $ cDeployer
      let mt_amt = DLA_Literal $ DLL_Int at UI_Word 0
      let mt_mtok = Just tok
      void $ makeTxn $ MakeTxn {..}
      -- NOTE We don't do this, because we know that it only happens on halt
      -- cp minimumBalance_l >> mbrSub
      k
    CLMemorySet _ _ a -> do
      cp a
      ctobs $ argTypeOf a
      gvStore GV_apiRet
      k

instance Compile CLTail where
  cp = \case
    CL_Com m k -> cpk (cp k) m
    CL_If _ a tt ft -> do
      cp a
      false_lab <- freshLabel "ifF"
      code "bz" [false_lab]
      nct tt
      label false_lab
      nct ft
    CL_Switch _ dv csm ->
      doSwitch nct dv csm
    CL_Jump _at f args isApi _mmret -> do
      vs <- map varLetVar <$> askFunVars f
      case isApi of
        True ->
          -- XXX maybe don't treat APIs specially in ALGO... requires changing
          -- the ABI of Reach's message handers to not have time in tuple
          -- rather than separate arg
          case vs of
            [v] -> do
              cp $ DLLA_Tuple args
              lookupVarColoring "jump api" v >>= \case
                Nothing -> op "pop"
                Just vl -> output $ TStore vl $ texty v
            _ -> impossible $ "ALGO: CL_Jump w/ isAPI and more than 1 vs"
        False -> cmoveMany $ zip vs args
      code "b" [ LT.pack $ bunpack f]
    CL_Halt at ht ->
      case ht of
        HM_Pure -> code "b" ["apiReturn_check"]
        HM_Impure -> code "b" ["updateStateNoOp"]
        HM_Forever -> do
          callCompanion at $ CompanionDeletePre
          code "b" ["updateStateHalt"]
    where
      nct = dupeResources . cp

symToSig :: CLSym -> App String
symToSig (CLSym f d r) = signatureStr False (bunpack f) d (Just r)

sigToLab :: String -> CLExtKind -> LT.Text
sigToLab x = \case
  CE_Publish n -> LT.pack $ "_reachp_" <> show n
  _ -> LT.pack $ map go final
    where
      final = take 16 x <> hashed
      hashed = B.unpack $ encodeBase64' $ sha256bs $ B.pack x
      go :: Char -> Char
      go c =
        case isAlphaNum c of
          True -> c
          False -> '_'

data CLFX = CLFX LT.Text (Maybe Int) CLFun
data CLEX = CLEX String CLExtFun
data CLIX = CLIX CLVar CLIntFun

instance Compile CLFX where
  cp (CLFX lab mwhich (CLFun {..})) = recordWhich mwhich $ do
    let at = clf_at
    callCompanion at $ CompanionLabel False lab
    cp clf_tail

instance Compile CLIX where
  cp (CLIX n (CLIntFun {..})) = do
    let CLFun {..} = cif_fun
    let lab = LT.pack $ bunpack n
    block lab $
      bindFromMove (map varLetVar clf_dom) $
        cp $ CLFX lab cif_mwhich cif_fun

bindFromMove :: [DLVar] -> App a -> App a
bindFromMove vs m = foldr (sallocLetNoStore "bindFromMove") m vs

checkArgSize :: String -> SrcLoc -> [DLVarLet] -> App ()
checkArgSize lab at msg = do
  -- The extra 4 bytes are the selector
  argSize <- (+) 4 <$> (typeSizeOf $ T_Tuple $ map (varType . varLetVar) msg)
  when (argSize > algoMaxAppTotalArgLen) $
    bad $ LT.pack $
      lab <> "'s argument length is " <> show argSize
      <> ", but the limit is " <> show algoMaxAppTotalArgLen
      <> ". " <> lab <> " starts at " <> show at <> "."

bindFromArgs :: [DLVarLet] -> App a -> App a
bindFromArgs vs m = do
  let goSingle (v, i) = sallocVarLet v (code "txna" ["ApplicationArgs", texty i] >> cfrombs (varLetType v))
  let goSingles singles k = foldl' (flip goSingle) k (zip singles [(1 :: Integer) ..])
  case splitArgs vs of
    (vs', Nothing) -> do
      goSingles vs' m
    (vs14, Just vsMore) -> do
      let tupleTy = T_Tuple $ map varLetType vsMore
      let goTuple (v, i) = sallocVarLet v
            (code "txna" ["ApplicationArgs", texty (15 :: Integer)]
             >> cTupleRef tupleTy i)
      goSingles vs14 (foldl' (flip goTuple) m (zip vsMore [(0 :: Integer) ..]))

instance CompileLabel CLEX where
  cpl (CLEX sig (CLExtFun {..})) = do
    let CLFun {..} = cef_fun
    let at = clf_at
    let lab = sigToLab sig cef_kind
    checkArgSize (show $ pretty cef_kind) at $ clf_dom
    let mwhich = case cef_kind of
                   CE_Publish n -> Just n
                   _ -> Nothing
    return $ (,) lab $ do
      case cef_kind of
        CE_Publish n -> do
          cWasntMeth
          when (n == 0) $
            callCompanion at CompanionCreate
        CE_View {} -> nop
        CE_API {} -> nop
      bindFromArgs clf_dom $
        cp $ CLFX lab mwhich cef_fun

instance Compile CLProg where
  cp (CLProg {..}) = do
    Env {..} <- ask
    let sig_go (sym, f) = do
          sig <- symToSig sym
          return (sig, (CLEX sig f))
    sig_api <- M.fromList <$> (mapM sig_go $ M.toAscList clp_api)
    let mkABI (CLEX _ (CLExtFun {..})) = ABInfo {..}
          where
            abiPure = clf_view cef_fun
    liftIO $ writeIORef eABI $ M.map mkABI sig_api
    let apiret_go (CLEX _ (CLExtFun {..})) = cef_rng
    maxApiRetSize <- maxTypeSize $ M.map apiret_go sig_api
    liftIO $ writeIORef eMaxApiRetSize maxApiRetSize
    liftIO $ writeIORef eApiLs $ []
    -- This is where the actual code starts
    code "b" ["preamble"]
    -- We branch on the method
    label "preamble"
    let getMeth = code "txna" ["ApplicationArgs", "0"]
    cmatch getMeth $ M.toAscList $ M.mapKeys sigStrToBytes sig_api
    -- Now we dump the implementation of internal functions
    mapM_ (cp . uncurry CLIX) $ M.toAscList clp_funs
    -- After looking at the code, we learned about the APIs
    let mkRec :: CLEX -> LabelRec
        mkRec (CLEX sig (CLExtFun {..})) = LabelRec {..}
          where
            CLFun {..} = cef_fun
            lr_at = clf_at
            lr_lab = sigToLab sig cef_kind
            lr_what = show $ pretty cef_kind
    apiLs <- liftIO $ readIORef eApiLs
    let pub_go e@(CLEX _ (CLExtFun {..})) =
          case cef_kind of
            CE_Publish {} -> Just $ mkRec e
            _ -> Nothing
    let pubLs = mapMaybe pub_go $ M.elems sig_api
    liftIO $ writeIORef eProgLs $ Just $ pubLs <> apiLs

cp_shellColor :: (HasALGOExitMode a, Compile a) => IGd a -> App ()
cp_shellColor (IGd x g) = do
  let vs = igVars g
  let maxSlot = 255
  let maxUsedSlot = fromEnum (maxBound :: GlobalVar)
  let maxAvailSlot = maxSlot - maxUsedSlot
  y <- liftIO $ colorHardLim g vs maxAvailSlot
  case y of
    Left m -> impossible $ "Could not allocate variables to registers using scratch space; we could hack the planet and get more registers by using frame variables: " <> m
    Right (_mc, c) -> do
      c' <- forWithKeyM c $ \v l -> do
        let l' = l + maxUsedSlot + 1
        unless (l' <= 255) $ do
          bad $ LT.pack $ "illegal coloring for " <> show v <> " " <> show l
        return $ fromIntegral l'
      local (\e -> e { eColoring = c' }) $
        cp_shell x

-- General Shell
cp_shell :: (HasALGOExitMode a, Compile a) => a -> App ()
cp_shell x = do
  Env {..} <- ask
  let mGV_companion =
        case eCompanion of
          Nothing -> []
          Just _ -> [GV_companion]
  let keyState_gvs :: [GlobalVar]
      keyState_gvs = [GV_currentStep, GV_currentTime] <> mGV_companion
  let keyState_ty :: DLType
      keyState_ty = T_Tuple $ map gvType keyState_gvs
  useResource R_Txn
  code "txn" ["ApplicationID"]
  code "bz" ["alloc"]
  -- Load the global state
  cp keyState
  op "app_global_get"
  forM_ (labelLast $ zip keyState_gvs [0..]) $ \((gv, i), isLast) -> do
    unless isLast $ op "dup"
    cTupleRef keyState_ty i
    gvStore gv
  -- Load the saved state
  stMap <- asks getStateMap
  let stEachType = map (T_Tuple . map typeOf) $ [] : M.elems stMap
  stMaxSize <- maximum <$> mapM typeSizeOf stEachType
  liftIO $ writeIORef eStateSizeR $ stMaxSize
  (_, stKeysl) <- computeStateSizeAndKeys bad "svs" stMaxSize algoMaxGlobalSchemaEntries_usable
  cSvsGet stKeysl
  stBindAll $ cp x
  -- Continuations
  label "updateStateHalt"
  code "txn" ["OnCompletion"]
  output $ TConst $ "DeleteApplication"
  asserteq
  output $ TCheckOnCompletion
  callCompanion sb $ CompanionDelete
  -- XXX technically this is not necessary, because we're about to return
  -- everything anyways and Algorand actually only checks the MBR for things
  -- that exist afterwards, but NOT doing this makes it so that the amounts are
  -- less than we think they're going to be on remote calls. We could get
  -- around that by adjusting `cGetBalance` and pretending that they are given
  -- when we're about to be deleted
  cUpdateMbr
  do
    incResource R_Account aDeployer
    let mt_at = sb
    let mt_always = True
    let mt_mrecv = Nothing
    let mt_mtok = Nothing
    let mt_submit = True
    let mt_next = False
    case getALGOExitMode x of
      DeleteAndCloseOutAll_SoundASAs_UnsoundElse -> do
        let f = liftIO . readIORef
        doesMapSet <- f eDoesMapSetR
        doesContractNew <- f eDoesContractNewR
        let unsoundRisk = doesMapSet || doesContractNew
        when unsoundRisk $ do
          warn $ "This program uses 'ALGOExitMode: DeleteAndCloseOutAll_SoundASAs_UnsoundElse' (the default) _and_ " <> (if doesMapSet then "creates Map entries and " else "") <> (if doesContractNew then "creates a child contract and " else "") <> "Reach cannot guarantee that these closed at application exit, but we are generating a close out anyways. If those resources are not freed, then this close out will fail and the final transaction will always be rejected."
        let mt_mcclose = Just $ cDeployer
        let mt_amt = argLitZero
        void $ makeTxn $ MakeTxn {..}
      DeleteAndCloseOutASAs -> do
        let mt_mcclose = Nothing
        cp $ DLE_GetUntrackedFunds sb Nothing argLitZero
        gvStore GV_mbrAdd
        let mt_amt = aDMbr
        store_let vDMbr (gvLoad GV_mbrAdd) $ do
          void $ makeTxn $ MakeTxn {..}
  code "b" ["updateState"]
  label "updateStateNoOp"
  gvStore GV_currentTime
  gvStore GV_currentStep
  cSvsPut stMaxSize stKeysl
  -- Put global state
  cp keyState
  cconcatbs $ flip map keyState_gvs $ \gv -> (gvType gv, gvLoad gv)
  op "app_global_put"
  -- Checking the OC
  code "txn" ["OnCompletion"]
  output $ TConst $ "NoOp"
  asserteq
  output $ TCheckOnCompletion
  cUpdateMbr
  code "b" ["updateState"]
  label "updateState"
  gvLoad GV_wasntMeth
  code "bnz" ["done"]
  code "b" [ "apiReturn_noCheck" ]
  label "apiReturn_noCheck"
  -- SHA-512/256("return")[0..4] = 0x151f7c75
  cp $ BS.pack [0x15, 0x1f, 0x7c, 0x75]
  gvLoad GV_apiRet
  op "concat"
  maxApiRetSize <- liftIO $ readIORef eMaxApiRetSize
  clog_ $ 4 + maxApiRetSize
  code "b" ["done"]
  label "errl"
  op "err"
  label "done"
  cp True
  op "return"
  label "apiReturn_check"
  code "txn" ["OnCompletion"]
  -- XXX A remote Reach API could have an `OnCompletion` of `DeleteApplication` due to `updateStateHalt`.
  output $ TConst "NoOp"
  asserteq
  output $ TCheckOnCompletion
  code "b" [ "apiReturn_noCheck" ]
  label "alloc"
  let ctf f v = do
        insertResult (LT.toStrict f) $ AS.Number $ fromIntegral v
        cp v
        code "txn" [f]
        asserteq
  ctf "GlobalNumUint" $ appGlobalStateNumUInt
  stateKeys <- liftIO $ eGetStateKeys
  ctf "GlobalNumByteSlice" $ appGlobalStateNumBytes + fromIntegral stateKeys
  ctf "LocalNumUint" $ appLocalStateNumUInt
  ctf "LocalNumByteSlice" $ appLocalStateNumBytes
  forM_ mGV_companion $ \gv -> do
    ctzero $ gvType gv
    gvStore gv
  cWasntMeth
  padding stMaxSize
  cint 0
  cint 0
  code "b" ["updateStateNoOp"]
  -- Library functions
  libDefns

compile_algo :: (HasFunVars a, HasStateMap a, HasCounter a, HasALGOExitMode a, Compile a) => Outputer -> IGd a -> IO ConnectorInfo
compile_algo disp x = do
  -- This is the final result
  eRes <- newIORef mempty
  totalLenR <- newIORef (0 :: Integer)
  let compileProg :: String -> [TEAL] -> IO CodeAndMap
      compileProg lab ts' = do
        t <- renderOut ts'
        tf <- mustOutput disp (T.pack lab <> ".teal") $ flip TIO.writeFile t
        bc <- compileTEAL tf
        unless unsafeDisableVerify $
          Verify.run lab bc [gvSlot GV_txnCounter, gvSlot GV_mbrAdd, gvSlot GV_mbrSub, gvSlot GV_wasntMeth] [gvSlot GV_apiRet]
        return bc
  let addProg lab ts' = do
        (tbs, sm) <- compileProg lab ts'
        modifyIORef totalLenR $ (+) (fromIntegral $ BS.length tbs)
        let tc = LT.toStrict $ encodeBase64 tbs
        modifyIORef eRes $ M.insert (T.pack lab) $ AS.String tc
        modifyIORef eRes $ M.insert (T.pack $ lab <> "Map") $
          AS.object $ map (\(k, v) -> (fromString (show k), AS.String (T.pack $ show v))) $ M.toAscList sm
        return tbs
  -- Clear state is never allowed
  cr_clearstate <- addProg "appClear" []
  -- Companion
  let makeCompanionMaker = do
        let ts =
              [ TCode "txn" [ "Sender" ]
              , TCode "global" [ "CreatorAddress" ]
              , TCode "==" []
              ]
        let cr_ctor = fromIntegral $ length ts
        let cr_call = cr_ctor
        let cr_del = cr_call
        (cr_approval, _) <- compileProg "appCompanion" ts
        return $ \cr_rv -> do
          let cr_ro = DLA_Var cr_rv
          return $ CompanionRec {..}
  companionCache <- newIORef $ Nothing
  let readCompanionCache = do
        c <- readIORef companionCache
        case c of
          Just y -> return y
          Nothing -> do
            y <- makeCompanionMaker
            writeIORef companionCache $ Just y
            return y
  -- We start doing real work
  (gFailuresR, gbad) <- newErrorSetRef
  (gWarningsR, _gwarn) <- newErrorSetRef
  let eColoring = mempty
  let eVars = mempty
  let eLets = mempty
  let eWhich = Nothing
  let recordSize prefix size = do
        modifyIORef eRes $
          M.insert (prefix <> "Size") $
            AS.Number $ fromIntegral size
  let recordSizeAndKeys :: NotifyF -> T.Text -> Integer -> Integer -> IO [Word8]
      recordSizeAndKeys badx prefix size limit = do
        (keys, keysl) <- computeStateSizeAndKeys badx (LT.fromStrict prefix) size limit
        recordSize prefix size
        modifyIORef eRes $
          M.insert (prefix <> "Keys") $
            AS.Number $ fromIntegral keys
        return $ keysl
  eABI <- newIORef mempty
  eProgLs <- newIORef mempty
  eApiLs <- newIORef mempty
  eMaxApiRetSize <- newIORef 0
  let eStateMap = getStateMap x
  let eFunVars = getFunVars x
  let run :: CompanionInfo -> App () -> IO (TEALs, Notify, IO ())
      run eCompanion m = do
        eCounter <- dupeCounter $ getCounter x
        eStateSizeR <- newIORef 0
        eLabel <- newCounter 0
        eDoesMapSetR <- newIORef False
        eDoesContractNewR <- newIORef False
        eOutputR <- newIORef mempty
        eNewToks <- newIORef mempty
        eInitToks <- newIORef mempty
        eResources <- newResources
        (eFailuresR, lbad) <- newErrorSetRef
        (eWarningsR, lwarn) <- newErrorSetRef
        let finalize = do
              mergeIORef gFailuresR S.union eFailuresR
              mergeIORef gWarningsR S.union eWarningsR
        companionMaker <- readCompanionCache
        cr_rv <- allocVar_ eCounter sb T_Contract
        eCompanionRec <- companionMaker cr_rv
        eLibrary <- newIORef mempty
        let eGetStateKeys = do
              stateSize <- readIORef eStateSizeR
              l <- recordSizeAndKeys lbad "state" stateSize algoMaxGlobalSchemaEntries_usable
              return $ length l
        flip runReaderT (Env {..}) $
          store_let cr_rv (gvLoad GV_companion) $
            m
        void $ eGetStateKeys
        ts <- readIORef eOutputR
        let notify b = if b then lbad else lwarn
        return (ts, notify, finalize)
  let showCost = unsafeDebug
  do
    let lab = "appApproval"
    let rec r inclAll ci = do
          let r' = r + 1
          let rlab = "ALGO." <> show r
          loud $ rlab <> " run"
          (ts, notify, finalize) <- run ci $ cp_shellColor x
          loud $ rlab <> " optimize"
          let disp' = wrapOutput (T.pack lab) disp
          showTeal ".raw" disp' $ DL.toList ts
          !ts' <- optimize ts
          progLs <- readIORef eProgLs
          apiLs <- readIORef eApiLs
          let mls = if inclAll then progLs else Just apiLs
          let ls = fromMaybe (impossible "prog labels") mls
          loud $ rlab <> " check"
          showTeal "" disp' ts'
          checkCost rlab notify disp' ls ci ts' >>= \case
            Right ci' -> rec r' inclAll ci'
            Left msg ->
              case inclAll of
                False -> rec r' True ci
                True -> do
                  finalize
                  when showCost $ putStr msg
                  modifyIORef eRes $ M.insert "companionInfo" (AS.toJSON ci)
                  return ts'
    void $ addProg lab =<< rec (0::Integer) False Nothing
  totalLen <- readIORef totalLenR
  when showCost $
    putStrLn $ "The program is " <> show totalLen <> " bytes."
  unless (totalLen <= algoMaxAppProgramLen_really) $ do
    gbad $ LT.pack $ "The program is too long; its length is " <> show totalLen <> ", but the maximum possible length is " <> show algoMaxAppProgramLen_really
  modifyIORef eRes $
    M.insert "extraPages" $
      AS.Number $ fromIntegral $ extraPages totalLen
  gFailures <- readIORef gFailuresR
  gWarnings <- readIORef gWarningsR
  let wss w lab ss = do
        unless (null ss) $
          emitWarning Nothing $ w $ S.toAscList $ S.map LT.unpack ss
        modifyIORef eRes $ M.insert lab $
          aarray $ S.toAscList $ S.map (AS.String . LT.toStrict) ss
  wss W_ALGOConservative "warnings" gWarnings
  wss W_ALGOUnsupported "unsupported" gFailures
  abi <- readIORef eABI
  let apiEntry lab f = (lab, aarray $ map (AS.String . s2t) $ M.keys $ M.filter f abi)
  modifyIORef eRes $
    M.insert "ABI" $
      aobject $
        M.fromList $
          [ apiEntry "sigs" (const True)
          , apiEntry "impure" (not . abiPure)
          , apiEntry "pure" abiPure
          ]
  modifyIORef eRes $
    M.insert "version" $
      AS.Number $ fromIntegral $ reachAlgoBackendVersion
  res <- readIORef eRes
  return $ aobject res

data ALGOConnectorInfo = ALGOConnectorInfo
  { aci_appApproval :: String
  , aci_appClear :: String
  } deriving (Show)

instance AS.ToJSON ALGOConnectorInfo where
  toJSON (ALGOConnectorInfo {..}) = AS.object $
    [ "approvalB64" .= aci_appApproval
    , "clearStateB64" .= aci_appClear
    ]

instance AS.FromJSON ALGOConnectorInfo where
  parseJSON = AS.withObject "ALGOConnectorInfo" $ \obj -> do
    aci_appApproval <- obj .: "appApproval"
    aci_appClear <- obj .: "appClear"
    return $ ALGOConnectorInfo {..}

data ALGOCodeIn = ALGOCodeIn
  { aci_approval :: String
  , aci_clearState :: String
  }
  deriving (Show)

instance AS.ToJSON ALGOCodeIn where
  toJSON (ALGOCodeIn {..}) = AS.object $
    [ "approval" .= aci_approval
    , "clearState" .= aci_clearState
    ]

instance AS.FromJSON ALGOCodeIn where
  parseJSON = AS.withObject "ALGOCodeIn" $ \obj -> do
    aci_approval <- obj .: "approval"
    aci_clearState <- obj .: "clearState"
    return $ ALGOCodeIn {..}

data ALGOCodeOut = ALGOCodeOut
  { aco_approval :: String
  , aco_clearState :: String
  }
  deriving (Show)

instance AS.ToJSON ALGOCodeOut where
  toJSON (ALGOCodeOut {..}) = AS.object $
    [ "approvalB64" .= toBase64 aco_approval
    , "clearStateB64" .= toBase64 aco_clearState
    ]
    where
      toBase64 :: String -> String
      toBase64 = LT.unpack . encodeBase64 . B.pack

instance AS.FromJSON ALGOCodeOut where
  parseJSON = AS.withObject "ALGOCodeOut" $ \obj -> do
    let fromBase64 :: String -> String
        fromBase64 x =
          case decodeBase64 $ B.pack x of
           Left y -> impossible $ "bad base64: " <> show y
           Right y -> B.unpack y
    aco_approval <- fromBase64 <$> (obj .: "approvalB64")
    aco_clearState <- fromBase64 <$> (obj .: "clearStateB64")
    return $ ALGOCodeOut {..}

data ALGOCodeOpts = ALGOCodeOpts
  { aco_globalUints :: Integer
  , aco_globalBytes :: Integer
  , aco_localUints :: Integer
  , aco_localBytes :: Integer
  }
  deriving (Show)

instance AS.ToJSON ALGOCodeOpts where
  toJSON (ALGOCodeOpts {..}) = AS.object $
    [ "globalUints" .= aco_globalUints
    , "globalBytes" .= aco_globalBytes
    , "localUints" .= aco_localUints
    , "localBytes" .= aco_localBytes
    ]

instance AS.FromJSON ALGOCodeOpts where
  parseJSON = AS.withObject "ALGOCodeOpts" $ \obj -> do
    aco_globalUints <- fromMaybe 0 <$> firstJustM (obj .:?) [ "globalUints", "GlobalNumUint" ]
    aco_globalBytes <- fromMaybe 0 <$> firstJustM (obj .:?) [ "globalBytes", "GlobalNumByteSlice" ]
    aco_localUints  <- fromMaybe 0 <$> firstJustM (obj .:?) [ "localUints", "LocalNumUint" ]
    aco_localBytes  <- fromMaybe 0 <$> firstJustM (obj .:?) [ "localBytes", "LocalNumByteSlice" ]
    return $ ALGOCodeOpts {..}

ccTEAL :: String -> CCApp BS.ByteString
ccTEAL tealf = liftIO (compileTEAL_ tealf) >>= \case
  Right (x, _) -> return x
  Left x -> throwE $ B.unpack x

ccTok :: BS.ByteString -> String
ccTok = B.unpack

ccPath :: String -> CCApp String
ccPath fp =
  case takeExtension fp of
    ".tok" -> ccTok <$> ccRead fp
    ".teal" -> ccTok <$> ccTEAL fp
    x -> throwE $ "Invalid code path: " <> show x

connect_algo :: Connector
connect_algo = Connector {..}
  where
    conName = conName'
    conCons = conCons'
    conGen (ConGenConfig {..}) clp = compile_algo cgOutput clp
    conReserved = const False
    conCompileCode v = runExceptT $ do
      ALGOCodeIn {..} <- aesonParse' v
      a' <- ccPath aci_approval
      cs' <- ccPath aci_clearState
      return $ AS.toJSON $ ALGOCodeOut a' cs'
    conContractNewOpts :: Maybe AS.Value -> Either String AS.Value
    conContractNewOpts mv = do
      (aco :: ALGOCodeOpts) <- aesonParse $ fromMaybe (AS.object mempty) mv
      return $ AS.toJSON aco
    conCompileConnectorInfo :: Maybe AS.Value -> Either String AS.Value
    conCompileConnectorInfo v = do
      ALGOConnectorInfo {..} <- aesonParse $ fromMaybe (AS.object mempty) v
      return $ AS.toJSON $ ALGOConnectorInfo {..}
