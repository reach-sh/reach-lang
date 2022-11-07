{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Reach.Compiler (CompilerOpts (..), compile, make_connectors) where

-- We allow name shadowing because we want to use `p` for every program AST
-- to ensure that we don't accidentally use things out of order.

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import qualified Filesystem.Path.CurrentOS as FP
import Reach.APICut
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.SL
import Reach.AST.PL
import Reach.Backend.JS
import Reach.BigOpt
import Reach.CLike
import Reach.CommandLine
import Reach.Connector
import Reach.Connector.ALGO
import Reach.Connector.ETH_Solidity
import Reach.EditorInfo
import Reach.EPP
import Reach.EraseLogic
import Reach.Eval
import Reach.FloatAPI
import Reach.Linearize
import Reach.Parser (gatherDeps_top)
import Reach.Simulator.Server
import Reach.StateDiagram
import Reach.Texty
import Reach.Util
import Reach.UnsafeUtil
import Reach.Verify
import System.Directory
import System.Exit
import System.FilePath
import Reach.Counter (newCounter)

make_connectors :: CompilerToolEnv -> Connectors
make_connectors env =
  M.fromList $
    map
      (\x -> (conName x, x))
      [ connect_eth env
      , connect_algo env
      ]

mkCompileProg :: CompilerToolEnv -> CompilerOpts -> FilePath -> FilePath -> String -> String -> DLProg -> IO ConnectorObject
mkCompileProg (CompilerToolEnv {..}) (CompilerOpts {..}) buildDir dotReachDirAbs appDescr outputFile dl = do
  let outnMay = flip doIf $ co_intermediateFiles || cte_REACH_DEBUG
  let co_output ext = FP.encodeString $ FP.append (FP.decodeString buildDir) $ (FP.filename $ FP.decodeString co_source) `FP.replaceExtension` ext
  let interOut outn_ = case outnMay outn_ of
        Just f -> LTIO.writeFile . f
        Nothing -> \_ _ -> return ()
  -- We are now inside the main part of the compiler that actually does
  -- something for each DApp.
  putStrLn $ "Compiling " <> appDescr <> "..."
  let woutn = co_output . ((T.pack outputFile <> ".") <>)
  let woutnMay = outnMay woutn
  let showp :: Pretty a => T.Text -> a -> IO a
      showp l x = do
        let x' = pretty x
        let x'' = render x'
        loud $ "showp " <> show l
        interOut woutn l x''
        return x
  p <- showp "dl" dl
  let DLProg { dlp_opts = DLOpts {..} } = p
  case co_stopAfterEval of
    True -> return mempty
    False -> do
      -- A DL program can contain control flow and function calls like:
      --
      -- f = () => {
      --   if ( c ) {
      --     commit()
      --     publish(x)
      --   }
      --   commit()
      --   publish(y)
      -- }
      -- f()
      -- f()
      -- k
      --
      -- = dekont
      --
      -- This pass duplicates continuations and inlines function calls. (We
      -- ALSO inline function calls during Eval, but we can't do that for
      -- ones that do communication). So the program looks like:
      --
      -- if (c) {
      --   commit()
      --   publish(x)
      --   if (c) {
      --     commit()
      --     publish(x)
      --     k
      --   } else {
      --     commit()
      --     publish(y)
      --     k
      --   }
      -- } else {
      --   if (c) {
      --     commit()
      --     publish(x)
      --     k
      --   } else {
      --     commit()
      --     publish(y)
      --     k
      --   }
      -- }
      --
      -- This is needed because control-flow and functions are hard for
      -- multiple parts of our compiler to deal with.
      --
      -- First, it is hard for the SMT solver to deal with because we need to
      -- give each variable a unique identifier, so we can't reason about
      -- stacks there. We could linearize there instead if we wanted.
      --
      -- Second, it is hard for end-point projection, because we need to
      -- discover what program state is available at every point. We could
      -- deal with this by making EPP know about stacks.
      --
      -- = defluid
      --
      -- Because it is doing this, linearize ALSO implements fluid-variables
      -- (like `fluid-let` or `parameterize` in Racket). This is used to
      -- store things like the contract balance and the current time. In the
      -- DL program, those things look like `FluidRef(balance)` and
      -- `FluidSet(balance, 55)`, but afterwards are transformed to `v77` and
      -- `let v78 = 55` (and subsequent uses of `balance` become `v78`).
      --
      -- = liftcon
      --
      -- Finally, this pass ALSO moves things up out of "steps" into
      -- "consensus steps", because variables bound in a "step" could have
      -- been defined in consensus and are probably actually used in them.
      -- For example:
      --
      -- let x = 44
      -- commit()
      -- let y = x + 2
      -- publish(z)
      -- check(z == y + x)
      --
      -- We turn that into
      --
      -- let x = 44
      -- let y = x + 2
      -- commit()
      -- publish(z)
      -- check(z == y + x)
      --
      -- Because EPP will only allow consensus steps to use variables defined
      -- in them. It is maybe plausible that the value could be push DOWN
      -- rather than UP
      --
      -- let x = 44
      -- commit()
      -- publish(z)
      -- let y = x + 2
      -- check(z == y + x)
      --
      -- But it doesn't work that way now. It is not apparent that one is
      -- always better than the other (not all uses will use both `x` and
      -- `y`). One thing we do know is that there is only one "UP" but there
      -- could be many "DOWN"s (because of forking and timeouts).
      --
      -- = freshen
      --
      -- Finally, finally, this pass also does alpha-varying of everything
      -- (because dekont can duplicate code) and produces the LL program AST,
      -- which is where we define most of the other work
      --
      p <- showp "ll" =<< linearize showp p
      -- We repeatedly optimize the program during compilation. It is
      -- important to do this in a few different places, which I'll talk
      -- about separately.
      --
      -- Here's how optimization works...
      --
      -- = Optimize
      --
      -- This takes information from the top of the program and propagates it
      -- down. This is where we do things like constant propagation and
      -- deforestation.
      --
      -- = AddCounts
      --
      -- This takes information from the bottom of the program and brings it
      -- up. This is where we discover that variables are not used and can be
      -- removed if they do not have effects.
      --
      -- --
      --
      -- THIS optimization run is to reduce the size of the verification
      -- problem, because SMT solvers are exponential in program size (&
      -- state)
      p <- showp "ol" =<< bigopt (showp, "ol") p
      -- This runs all of the different verifications.
      --
      -- The knowledge checker is basically a graph connected-ness search on
      -- the knowledge graph.
      --
      -- The SMT engine does the standard SMT checking thing.
      case cte_REACH_ACCURSED_UNUTTERABLE_DISABLE_VERIFICATION_AND_LOSE_ALL_YOUR_MONEY_AND_YOUR_USERS_MONEY of
        True -> do
          putStrLn "!!! Verification Disabled.  !!!"
          putStrLn "!!! Assertions NOT checked. !!!"
          putStrLn "!!! This is not safe.       !!!"
        False -> do
          let vo_out = woutnMay
          let vo_mvcs = doIf dlo_connectors dlo_verifyPerConnector
          let vo_timeout = co_verifyTimeout
          let vo_dir = dotReachDirAbs
          let vo_first_fail_quit = co_verifyFirstFailQuit
          verify (VerifyOpts {..}) p >>= maybeDie
      -- Once we know that we've passed the verification engine, we can
      -- remove variables that only occur in `assert` and `invariant`
      -- statements. The only hard part of this is noticing that some loop
      -- variables can be removed.
      p <- showp "el" =<< erase_logic p
      unless (not co_sim) $ do
        src <- readFile co_source
        startServer p src
      -- We optimize again because since we just removed logic variables,
      -- there are probably tempories that we can get rid of too.
      p <- showp "eol" =<< bigopt (showp, "eol") p
      -- This is a really simple pass that knows how the DL code generator
      -- works generating API code. That code looks like this:
      --
      -- publish(d)
      -- ... checks about payment ...
      -- switch (d) {
      --  case Buy22: {
      --    ... some more checks ...
      --    setApiDetails("Buy")
      --  }
      -- }
      --
      -- and this pass turns it into
      --
      -- publish(d)
      -- ... checks about payment ...
      -- switch (d) {
      --  case Buy22: {
      --    setApiDetails("Buy")     // <-- moved up
      --    ... some more checks ...
      --  }
      -- }
      --
      -- Maybe we should stick this in Linearize
      --
      p <- showp "flap" =<< floatAPI p
      -- This is the end-point projection pass.
      --
      -- The basic idea of end-point projection is to turn
      --
      -- A.publish(x)
      -- let y = x + 1
      -- commit()
      -- B.publish(z)
      -- let u = y + z
      -- commit()
      --
      -- into three programs
      --
      -- A:  publish0(x)    B:
      --     recv0(x')         recv0(x')
      --     let y = x' + 1    let y = x' + 1
      --                       publish1(z)
      --     recv1(z')         recv1(z')
      --     let u = y + z'    let u = y + z'
      --
      -- C:
      --  data State
      --   = S0
      --   | S1 y
      --   | S2 u
      --
      --  m0 :: S0 -> x -> S1
      --  m0 (S0) x = do
      --   let y = x + 1
      --   return $ S1 y
      --
      --  m1 :: S1 -> z -> S2
      --  m1 (S1 y) z = do
      --   let u = y + z
      --   return $ S2 u
      --
      -- Producing A & B is pretty easy, because we are just tracking whether
      -- a given user is sending or receiving, but it is complicated because
      -- there are forks, races, timeouts, loops, and all that exciting
      -- stuff.
      --
      -- Producing C is the harder part. There we need to discover what the
      -- states are and create each one of the state transitions.
      --
      p <- showp "pil" =<< epp p
      -- The C program is a state machine... we can display it as dot
      void $ showp "state.dot" $ stateDiagram p
      -- In the DL code generation, APIs are represented as participant
      -- classes, but we don't want them to have to be attached for the
      -- entire program. So this pass changes an "A"-like program from
      --
      -- recv0
      -- let x = ...
      -- recv1
      -- let y = ...
      -- recv2
      -- let dom = ...
      -- send3(Buy(dom))
      -- ... x, y, dom ...
      -- recv3
      -- recv4
      --
      -- into
      --
      -- let (x, y) = getState
      -- let dom = ...
      -- send3(Buy(dom))
      -- ... x, y, dom ...
      -- recv3
      --
      p <- showp "apc" =<< apicut p
      -- We optimize again at this point, because some values may only be of
      -- interested to some participants, so the A/B/C programs can become
      -- smaller
      p <- showp "pl" =<< bigopt (showp, "pl") p
      p <- showp "cl" =<< clike p
      -- Next, we generate the backend code for each one of the connectors
      -- the user asked for. This return "connector info" structures that
      -- basically have the bytecode in them, plus stuff the runtime needs.
      --
      -- This only looks at the "C" piece
      crs <- forM dlo_connectors $ \c -> do
        let n = conName c
        loud $ "running connector " <> show n
        conGen c woutnMay $ plp_cpp p
      -- Those connector info things will be given to the JS code to get
      -- included in the actual backend.
      loud $ "running backend js"
      backend_js woutn crs $ plp_epp p
      return crs

-- This function is the actual compiler.
--
-- It reads from the environment (for things like debugging) and the actual
-- command-line options.
compile :: CompilerToolEnv -> CompilerOpts -> IO ()
compile env co@(CompilerOpts {..}) = do
  let source = if co_printKeywordInfo then ReachStdLib else ReachSourceFile co_source
  let outd = fromMaybe (takeDirectory co_source </> "build") co_moutputDir
  let co_dirDotReach = fromMaybe (takeDirectory co_source </> ".reach") co_mdirDotReach
  createDirectoryIfMissing True outd
  let co_tops = if null co_topl then Nothing else Just (S.fromList co_topl)
  dirDotReach' <- makeAbsolute co_dirDotReach
  -- First, we actually read the source files. This function is the only thing
  -- that will read the disk. It produces a "JS Bundle" which is a map from
  -- locations to code. This is so that we don't read the same module code
  -- multiple times and ensure that diamond dependencies share everything. It
  -- makes a DAG of the import relationships and will expose this to the
  -- evaluator so they can be run in order. It mostly exists as a misguided
  -- attempt to risk the IO monad to one place in the compiler, but it is maybe
  -- useful today.
  djp <- gatherDeps_top source co_installPkgs dirDotReach'
  -- interOut co_output "bundle.js" $ render $ pretty djp
  unless co_installPkgs $ do
    let all_connectors = make_connectors env
    -- Next, we run the "top-level" of every module. This is going to visit the
    -- modules in topo-order, but it only evaluates things that are at the
    -- top-level. It basically returns a structure that can be used to actually
    -- compile `Reach.App` structures.
    --
    -- `shared_lifts` are basically the module-level bindings that are
    -- available for every DApp. (The word "lifts" is used in reference to
    -- Racket's `syntax-local-lift`. Remember, Reach is basically a Scheme
    -- interpreter where most of the primitives are constructing a residual
    -- program in the "DL" language. Creating a statement in that language is
    -- called "lifting".)
    uniC <- newCounter 0
    (run, shared_lifts, exe_ex) <- evalBundle all_connectors djp co_printKeywordInfo uniC
    when co_printKeywordInfo $ do
      let exportMap = M.map sss_val exe_ex
      printBaseKeywordInfo exportMap
      exitSuccess

    -- This does a tiny bit more environment setup and it restricts the set of
    -- DApps that get compiled to the ones passed at the command-line. This is
    -- mostly boring administrative stuff and not interesting compilation.
    let compileProg = mkCompileProg env co outd dirDotReach'
    (avail, compileDApp) <- prepareDAppCompiles compileProg run shared_lifts exe_ex uniC
    -- This compileDApp function came out of `prepareDAppCompiles` and it
    -- embeds a call to Eval/Core, but shares the module state from
    -- `shared_lifts`. This is going to do evaluation of the source (or SL)
    -- program and produce the residual DL program. SL is Scheme with JS
    -- syntax and DL is basically ML with a bunch of DApp specific ideas.
    --
    -- `compileProg` is embedded into `compileDApp`, and will be called on
    -- the resulting DLProg.
    let chosen = S.toAscList $ fromMaybe avail co_tops
    forM_ chosen compileDApp

doIf :: a -> Bool -> Maybe a
doIf b = \case
  True -> Just b
  False -> Nothing
