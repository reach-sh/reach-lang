module Reach.EmitTEAL where

import Data.List

import Reach.AST

data TEAL
  = TEAL TEALs

type TEALs = [ TEALLine ]

instance Show TEAL where
  show (TEAL tls) = concat $ intersperse "\n" $ map show tls

data TEALLine
  = TL_Comment String

instance Show TEALLine where
  show (TL_Comment c) = "// " ++ c

bracket :: String -> TEALs -> TEALs
bracket lab code = TL_Comment ("<" ++ lab ++ ">") : code ++ [ TL_Comment ("</" ++ lab ++ ">") ]

xxx :: TEALs
xxx = [ TL_Comment "XXX" ]

comp_chandler :: CHandler a -> TEALs
comp_chandler (C_Handler _ _from_spec _is_timeout (_last_i, _svs) _msg _delay _body i) = bracket ("Handler " ++ show i) xxx
comp_chandler (C_Loop _ _svs _args _inv _body _i) = []

comp_cloop :: CHandler a -> TEALs
comp_cloop (C_Handler _ _from_spec _is_timeout _ _msg _delay _body _i) = []
comp_cloop (C_Loop _ _svs _args _inv _body i) = bracket ("Loop " ++ show i) xxx

cp_to_teal :: CProgram a -> TEAL
cp_to_teal (C_Prog _ hs) = TEAL ls
  where ls = dispatch_ls ++ handlers_ls ++ loop_ls
        dispatch_ls = bracket "Dispatcher" xxx
        handlers_ls = bracket "Handlers" $ concatMap comp_chandler hs
        loop_ls = bracket "Loops" $ concatMap comp_cloop hs

emit_teal :: FilePath -> BLProgram a -> IO String
emit_teal tf (BL_Prog _ _ cp) = do
  let bc = cp_to_teal cp
  writeFile tf (show bc)
  return ""
