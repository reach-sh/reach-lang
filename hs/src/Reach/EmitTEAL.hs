module Reach.EmitTEAL where

import qualified Data.Map.Strict as M
import Data.List

import Reach.AST
import Reach.EmitSol
  ( CCounts
  , usesCTail )

data TEAL
  = TEAL TEALs

type TEALs = [ TEALLine ]

instance Show TEAL where
  show (TEAL tls) = concat $ intersperse "\n" $ map show tls

data TEALLine
  = TL_Comment String
  | TL_Label String
  | TL_Code [String]

instance Show TEALLine where
  show (TL_Comment c) = "// " ++ c
  show (TL_Label l) = l ++ ":"
  show (TL_Code s) = concat $ intersperse " " s

label_ext :: String -> String -> String
label_ext base ext = base ++ "_" ++ ext

bracket :: String -> TEALs -> TEALs
bracket lab ls = TL_Comment ("<" ++ lab ++ ">") : ls ++ [ TL_Comment ("</" ++ lab ++ ">") ]

xxx :: String -> TEALs
xxx x = [ TL_Comment $ "XXX " ++ x ]

label :: String -> TEALs
label lab = [ TL_Label lab ]

code :: String -> [ String ] -> TEALs
code op args = [ TL_Code $ op : args ]

comp_ctail :: CCounts -> String -> CTail a -> TEALs
comp_ctail ccs lab t =
  case t of
    C_Halt _ ->
      code "jump" ["halt"]
    C_Wait _ _i _svs ->
      xxx "wait"
    C_If _ _ca tt ft ->
      xxx "if arg"
      ++ code "bnz" [true_lab]
      ++ comp_ctail ccs false_lab ft
      ++ label true_lab
      ++ comp_ctail ccs true_lab tt
      where false_lab = label_ext lab "F"
            true_lab = label_ext lab "T"
    C_Let _ bv _ce kt ->
      case M.lookup bv ccs of
        Just 0 -> comp_ctail ccs lab kt
        --- FIXME do something on 1
        _ ->
          xxx "let binding"
          ++ comp_ctail ccs lab kt
    C_Do _ _ds kt ->
      xxx "do stmt"
      ++ comp_ctail ccs lab kt
    C_Jump _ _which _vs _ _as ->
      xxx "jump"

comp_chandler :: CHandler a -> TEALs
comp_chandler (C_Handler _ _from_spec _is_timeout (_last_i, _svs) _msg _delay body i) = bracket ("Handler " ++ show i) $ label lab ++ (xxx "handler") ++ comp_ctail (usesCTail body) lab body
  where lab = "h" ++ show i
comp_chandler (C_Loop _ _svs _args _inv _body _i) = []

comp_cloop :: CHandler a -> TEALs
comp_cloop (C_Handler _ _from_spec _is_timeout _ _msg _delay _body _i) = []
comp_cloop (C_Loop _ _svs _args _inv body i) = bracket ("Loop " ++ show i) $ label lab ++ (xxx "loop pre") ++ comp_ctail (usesCTail body) lab body
  where lab = "l" ++ show i

cp_to_teal :: CProgram a -> TEAL
cp_to_teal (C_Prog _ hs) = TEAL ls
  where ls = dispatch_ls ++ handlers_ls ++ loop_ls
        dispatch_ls = bracket "Dispatcher" $ xxx "dispatcher"
        handlers_ls = bracket "Handlers" $ concatMap comp_chandler hs
        loop_ls = bracket "Loops" $ concatMap comp_cloop hs

emit_teal :: FilePath -> BLProgram a -> IO String
emit_teal tf (BL_Prog _ _ cp) = do
  let bc = cp_to_teal cp
  writeFile tf (show bc)
  return ""
