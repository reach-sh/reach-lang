{-# LANGUAGE OverloadedStrings #-}

module Reach.EmitEVM where

--import qualified Data.Word as W
import qualified Data.HexString as H
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified EVM.Bytecode as EVM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import Data.Foldable

import Reach.AST
import Reach.EmitSol
  ( CompiledSol )

data LabelInfo = LabelInfo Int Int
data ASMOp a b
  = Op b
  | LabelRef a (LabelInfo -> b)
type ASMSeq a b = [ASMOp a b]
data ASMProg a b = ASMProg (M.Map a (ASMSeq a b)) (ASMSeq a b)

assemble :: Show l => Ord l => ASMProg l o -> [o]
assemble (ASMProg defs main) = map asmfix $ toList image
  where asmfix ao =
          case ao of
            Op o -> o
            LabelRef l f ->
              case M.lookup l locs of
                Nothing -> error $ "Undefined label in assembly: " ++ show l
                Just li -> f li
        (locs, image) = M.foldrWithKey loc1 (M.empty, Q.fromList main) defs
        loc1 label body (locs0, before) = (locs1, after)
          where body_seq = Q.fromList body
                sz = Q.length body_seq
                start = Q.length before
                locs1 = M.insert label (LabelInfo start sz) locs0
                after = before Q.>< body_seq

emit_evm :: FilePath -> BLProgram a -> CompiledSol -> IO ()
emit_evm _ _ (_, code) =
  if False then
    do
      let bs = H.toBytes $ H.hexString $ BC.pack code
      mapM_ (\o -> putStrLn $ show o) $ EVM.decode $ B.unpack bs
      return ()
  else
    return ()
