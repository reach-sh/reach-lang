module Reach.StateDiagram (stateDiagram) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Reach.Dotty
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Texty

loopl :: Int -> String
loopl i = "loop" <> show i
statel :: Int -> String
statel i = "state" <> show i

pointe :: String -> DotGraph
pointe it = [("!node", it, M.fromList [ ("shape", "point") ])]

geT :: String -> String -> CTail -> DotGraph
geT trans from = \case
  CT_Com _ t -> rec t
  CT_If _ _ t f -> rec t <> rec f
  CT_Switch _ _ csm -> concatMap go $ M.toAscList csm
    where
      go (_, (_, _, t)) = rec t
  CT_From _ i fi -> fi' <> add it
    where
      it = statel i
      fi' =
        case fi of
          FI_Continue _ -> []
          FI_Halt _ -> pointe it
  CT_Jump _ i _ _ -> add $ loopl i
  where
    rec = geT trans from
    add to = [(from, to, M.singleton "label" trans)]

showState :: String -> [DLVarLet] -> DotGraph
showState it vsl =
  case it of
    "state0" -> pointe it
    _ -> [("!node", it, M.fromList [ ("shape", "record"), ("label", l) ])]
  where
    l = "{" <> it <> vsl' <> "}"
    vsl' =
      case null vsl of
        True -> ""
        False -> "|" <> intercalate "|" (map nicev vsl)

class HasEdges a where
  getEdges :: a -> DotGraph

instance HasEdges CHandlers where
  getEdges (CHandlers m) = concatMap (uncurry go) $ M.toAscList m
    where
      go hi = \case
        C_Loop {..} -> showState l (cl_svs <> cl_vars) <> geT "" l cl_body
          where
            l = loopl hi
        C_Handler {..} -> showState s ch_svs <> geT (transl "p" hi ch_msg) s ch_body
          where
            s = statel ch_last
      transl pre i vsl =
        pre <> show i <> "(" <> intercalate ", " (map nicev vsl) <> ")"

nicev :: DLVarLet -> String
nicev v = mv <> ":" <> t
  where
    DLVarLet _ (DLVar _ ml ty i) = v
    t = show $ pretty $ ty
    mv =
      case ml of
        Just (_, s)-> s
        _ -> "v" <> show i

instance HasEdges CPProg where
  getEdges (CPProg _ _ _ _ hs) = getEdges hs

instance HasEdges PLProg where
  getEdges (PLProg _ _ _ _ _ cp) = getEdges cp

stateDiagram :: PLProg -> DotGraph_
stateDiagram = DotGraph_ . getEdges
