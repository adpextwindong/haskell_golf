module Haskell.SylarDoom.Tick where

import qualified Data.IntMap as IntMap
import Data.Char
import Control.Monad.Writer

interpreter :: String -> String
interpreter tape = execWriter $ exec initialState command_stream
    where command_stream = fmap command tape

data Command = MoveL | MoveR | Incr | Put

data MachineState = MachineState {
                      ptr :: Int,
                      memory :: IntMap.IntMap Int
                    }

initialState = MachineState 0 (IntMap.fromList [])

exec :: MachineState -> [Command] -> Writer String ()
exec _ [] = return ()
exec (MachineState ptr mem) (MoveL:ts) = exec (MachineState (ptr - 1) mem) ts
exec (MachineState ptr mem) (MoveR:ts) = exec (MachineState (ptr + 1) mem) ts

exec (MachineState ptr mem) (Incr:ts) = exec (MachineState ptr newMem) ts
    where newMem = IntMap.insertWith wrap ptr 1 mem
          wrap :: Int -> Int -> Int
          wrap x y = if x + y > 255
                     then x + y - 255
                     else x + y

exec m@(MachineState ptr mem) (Put:ts) = do
    case IntMap.lookup ptr mem of
        Just val -> tell [chr val]
        Nothing -> tell [chr 0]
    exec m ts

command :: Char -> Command
command '<' = MoveL
command '>' = MoveR
command '+' = Incr
command '*' = Put
command _ = error "Illegal instruction"
